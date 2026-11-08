!$Procedure SHIFTL ( Shift left )

      SUBROUTINE SHIFTL ( IN, NSHIFT, FILLC, OUT )

! $ Abstract
!
!     Shift the contents of a character string to the left.
!     Characters moved past the beginning of the input string are
!     lost. Vacant spaces are filled with a specified character.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)      IN
      INTEGER            NSHIFT
      CHARACTER*1        FILLC
      CHARACTER*(*)      OUT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input string.
!     NSHIFT     I   Number of times to shift.
!     FILLC      I   Character to fill spaces left vacant.
!     OUT        O   Shifted string.
!
! $ Detailed_Input
!
!     IN       is the input character string.
!
!     NSHIFT   is the number of times the string is to be
!              shifted. If NSHIFT is negative, OUT will be
!              identical to IN.
!
!     FILLC    is the character with which spaces left vacant by
!              the shift are to be filled.
!
! $ Detailed_Output
!
!     OUT      is the output string. This is the input string,
!              shifted N times, filled with FILLC.
!
!              OUT may overwrite IN.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     As a string is shifted left or right, the leftmost or
!     rightmost characters of the string disappear (as if pushed
!     off the end of the string). This is .TRUE. regardless of
!     the length of the output string.
!
!     The remaining characters are shifted simultaneously, and
!     the spaces vacated by those characters are filled with a
!     replacement character.
!
! $ Examples
!
!     If FILLC = ' '
!
!        'abcde'   shifted left twice becomes     'cde  '
!        'abcde'   shifted right once becomes     ' abcd'
!
!     If FILLC = '.'
!
!        '12345 '  shifted right once becomes     '.12345'
!        'Apple '  shifted left ten times becomes '......'
!
!     Given the declarations
!
!        CHARACTER*3         SHORT
!        CHARACTER*10        LONG
!
!     The calls
!
!        CALL SHIFTR ( 'abcde ', 2, '-', SHORT )
!        CALL SHIFTR ( 'abcde ', 2, '-', LONG  )
!
!     yield the strings
!
!        SHORT = '--a'
!        LONG  = '--abcd    '
!
!     while the calls
!
!        CALL SHIFTL ( 'abcde ', 2, '-', SHORT )
!        CALL SHIFTL ( 'abcde ', 2, '-', LONG  )
!
!     yield the strings
!
!        SHORT = 'cde'
!        LONG  = 'cde ..     '
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
!     J. Diaz del Rio    (ODC Space)
!     M.J. Spencer       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.1, 22-AUG-2001 (EDW)
!
!        Corrected ENDDO to END DO.
!
!-    SPICELIB Version 2.0.0, 01-SEP-1994 (MJS)
!
!        This version correctly handles negative shifts.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     shift left
!
!-&


!
!     Local variables
!
      INTEGER          INLEN
      INTEGER          OUTLEN

      INTEGER          N
      INTEGER          NSAVE
      INTEGER          NFILL
      INTEGER          I
      INTEGER          S

!
!     Get the length of the input, output strings.
!
      INLEN  = LEN ( IN  )
      OUTLEN = LEN ( OUT )

!
!     If the shift is zero or negative, the string is not changed.
!     If longer than the input string, the entire string is shifted.
!
      S = MAX ( NSHIFT, 0 )
      N = MIN ( INLEN,  S )

!
!     Figure out how many characters in the input string will
!     be saved (will not be shifted off the end of the string,
!     and will fit in the output string), and how many fill
!     characters will be needed (no more than NSHIFT, no fewer
!     than zero).
!
      NSAVE =     MIN ( INLEN - N, OUTLEN         )
      NFILL = N - MAX ( 0,         INLEN - OUTLEN )

!
!     Move the saved characters to output.
!
      DO I = 1, NSAVE
         OUT(I:I) = IN(I+S:I+S)
      END DO

!
!     Add as many fill characters as appropriate.
!
      DO I = NSAVE+1, NSAVE+NFILL
         OUT(I:I) = FILLC
      END DO

!
!     Pad the output string with blanks (to cover any previous
!     ugliness there).
!
      IF ( OUTLEN .GT. INLEN ) THEN
         OUT(INLEN+1: ) = ' '
      END IF

      RETURN
      END
