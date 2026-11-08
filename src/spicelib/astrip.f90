!$Procedure ASTRIP ( STRIP Ascii characters from a string )

      SUBROUTINE ASTRIP ( INSTR, ASCIIB, ASCIIE, OUTSTR )

! $ Abstract
!
!     Remove from a character string all characters which fall
!     between specified starting and ending characters, inclusive.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASCII
!     CHARACTER
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    INSTR
      CHARACTER*1      ASCIIB
      CHARACTER*1      ASCIIE
      CHARACTER*(*)    OUTSTR

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INSTR      I   Input string.
!     ASCIIB     I   First ASCII character in range to be stripped.
!     ASCIIE     I   Last ASCII character in range to be stripped.
!     OUTSTR     O   Output (stripped) string.
!
! $ Detailed_Input
!
!     INSTR    is a character string from which all characters
!              between ASCIIB and ASCIIE, inclusive, are to be
!              removed.
!
!     ASCIIB   is the first ASCII character in the range of
!              characters to be removed from the input string.
!              ASCIIB is itself removed from the string, if
!              it occurs.
!
!     ASCIIE   is the last ASCII character in the range of
!              characters to be removed from the input string.
!              ASCIIE is itself removed from the string, if
!              it occurs.
!
! $ Detailed_Output
!
!     OUTSTR   is the input string after all the character
!              between ASCIIB and ASCIIE, inclusive, have
!              been removed.
!
!              If OUTSTR is not large enough to hold the output
!              string, it is truncated on the right.
!
!              OUTSTR may overwrite INSTR.
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
!     ASTRIP checks each character
!     in INSTR to determine if it falls between the characters ASCIIB
!     and ASCIIE. If so this character is removed from the string
!     (and the string is shortened). Remaining characters are copied
!     to the output string.
!
! $ Examples
!
!     The following examples illustrate the use of ASTRIP.
!
!           ASCIIB = 'b'
!           ASCIIE = 'k'
!           INSTR  = 'Now is the time for all good men to come quick.'
!           OUTSTR = 'Now s t tm or all oo mn to om qu.'
!
!           ASCIIB = 'a'
!           ASCIIE = 'z'
!           INSTR  = 'SELECT column TIME FROM table TEST'
!           OUTSTR = 'SELECT TIME FROM TEST'
!
!           ASCIIB = 'a'
!           ASCIIE = 'z'
!           INSTR  = 'this is going to be an empty string'
!           OUTSTR = ' '
!
!           ASCIIB = '!'
!           ASCIIE = '!'
!           INSTR  = 'Only 32 more shopping days until Christmas!'
!           OUTSTR = 'Only 32 more shopping days until Christmas'
!
!     ASTRIP may also be used to strip ASCII control characters
!     (line feeds, tab stops, and so on), as shown in the example
!     below.
!
!           ASCIIB = CHAR ( 0  )
!           ASCIIE = CHAR ( 31 )
!           CALL ASTRIP ( STRING, ASCIIB, ASCIIE, STRING )
!
! $ Restrictions
!
!     1)  If ASCIIB and ASCIIE are not properly ordered (that is,
!         if ICHAR(ASCIIB) is not less than or equal to ICHAR(ASCIIE))
!         then ASTRIP will not function as described. (In fact, it will
!         copy the input string to the output string without change.)
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
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     strip ascii characters from a string
!
!-&


!
!     SPICELIB functions
!
      INTEGER          LASTNB

!
!     Local Variables
!
      INTEGER          OUTLEN
      INTEGER          LAST
      INTEGER          LWRBND
      INTEGER          UPRBND
      INTEGER          I
      INTEGER          J
      INTEGER          K



!
!     Find the length of the output string. We don't want to
!     exceed it.
!
      OUTLEN = LEN ( OUTSTR )

!
!     Find the last non-blank character of the input string.
!
      LAST   = LASTNB ( INSTR )

!
!     Get the numeric representation of ASCIIB and ASCIIE.
!
      LWRBND = ICHAR ( ASCIIB )
      UPRBND = ICHAR ( ASCIIE )

!
!     Step through INSTR (I) a character at a time, transferring
!     characters to OUTSTR (J) whenever they fall outside the range
!     [ASCIIB, ASCIIE].
!
!     If the end of OUTSTR is reached, stop transferring characters
!     and return.
!
      J    = 0

      DO I = 1, LAST

         K = ICHAR ( INSTR(I:I) )

         IF ( ( K .LT. LWRBND ) .OR. ( K .GT. UPRBND ) ) THEN
!
!           The character is kept.  Note that if the user inputs
!           ASCIIB and ASCIIE in the wrong order this test will
!           always succeed so that the output string will be
!           the same as the input string.
!
            J           = J + 1
            OUTSTR(J:J) = INSTR(I:I)

            IF ( J .EQ. OUTLEN ) THEN
               RETURN
            END IF

         END IF

      END DO

!
!     Pad the output string with blanks.
!
      IF ( J .LT. OUTLEN ) THEN
         OUTSTR(J+1: ) = ' '
      END IF

      RETURN
      END
