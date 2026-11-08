!$Procedure LCASE ( Convert to lowercase )

      SUBROUTINE LCASE ( IN, OUT )

! $ Abstract
!
!     Convert the characters in a string to lowercase.
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

      CHARACTER*(*)    IN
      CHARACTER*(*)    OUT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input string.
!     OUT        O   Output string, all lowercase.
!
! $ Detailed_Input
!
!     IN       is the input string.
!
! $ Detailed_Output
!
!     OUT      is the output string. This is the input string
!              with all uppercase letters converted to lowercase.
!              Non-letters are not affected.
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
!     1)  If the output string length is less than the input string
!         length, the result will be truncated on the right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Convert each uppercase character in IN to lowercase.
!
! $ Examples
!
!     'This is an EXAMPLE'     becomes  'this is an example'
!     '12345 +-=? > * $ &'              '12345 +-=? > * $ &'
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
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 04-AUG-2021 (JDR)
!
!        Added IMPLICT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Added entry #1 in $Exceptions section.
!
!-    SPICELIB Version 1.1.0, 13-MAR-1996 (KRG)
!
!        Removed the calls to the string lexicographic functions.
!
!        Modified the algorithm to use the ICHAR() intrinsic function
!        and some local integer storage for the bases of the lower and
!        upper case letters.
!
!        Added a "FIRST" clause to the code so that the lower and
!        upper case bases and the separation between them are only
!        initialized the first time the subroutine is called rather
!        than every time.
!
!        These changes were made to improve the execution speed of
!        the subroutine
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
!     convert to lowercase
!
!-&


!
!     Local variables
!
      INTEGER          I
      INTEGER          ICH
!
!     Saved variables
!
      INTEGER          SHIFT
      INTEGER          UPPERA
      INTEGER          UPPERZ

      LOGICAL          FIRST

      SAVE             FIRST
      SAVE             SHIFT
      SAVE             UPPERA
      SAVE             UPPERZ
!
!     Initial Data
!
      DATA             FIRST / .TRUE. /
!
!     Do some set up stuff the first time through so that we do not
!     need to reinitialize the boundary values used for comparisons
!     and the shift on each call.
!
      IF ( FIRST ) THEN
         FIRST  = .FALSE.
         UPPERA = ICHAR ('A')
         UPPERZ = ICHAR ('Z')
         SHIFT  = ICHAR ('a') - UPPERA
      END IF

!
!     Move the string from IN to OUT. Step through OUT one character
!     at a time, translating letters between 'A' and 'Z' to lowercase.
!
      OUT   = IN

      DO I = 1, LEN (OUT)

         ICH = ICHAR( OUT(I:I) )

         IF ( ( ICH .GE. UPPERA ) .AND. ( ICH .LE. UPPERZ ) ) THEN
            OUT(I:I) = CHAR ( ICH + SHIFT )
         END IF

      END DO

      RETURN
      END
