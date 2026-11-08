!$Procedure LASTPC ( Last printable character )

      INTEGER FUNCTION  LASTPC ( STRING )

! $ Abstract
!
!     Return the index of the last printable character in a character
!     string. ASCII characters 33-126 are printable. (Blanks are not
!     considered printable.)
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASCII
!     CHARACTER
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    STRING

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   Input character string.
!
!     The function returns the index of the last non-blank printable
!     character in STRING.
!
! $ Detailed_Input
!
!     STRING   is the input character string.
!
! $ Detailed_Output
!
!     The function returns the index of the last printable character in
!     the input string.
!
!     ASCII characters 33-126 are considered to be printable characters.
!     Blanks are not considered printable characters. If the input
!     string contains no printable characters, LASTPC is zero.
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
!     This works exactly like LASTNB, except that it skips non-printable
!     characters (ASCII control characters) as well as blanks.
!
! $ Examples
!
!     The program
!
!        INTEGER         FRSTNB
!        INTEGER         FRSTPC
!        INTEGER         LASTNB
!        INTEGER         LASTPC
!
!        CHARACTER*10    S
!
!        S( 1: 1) = ' '
!        S( 2: 2) = CHAR (  2 )
!        S( 3: 3) = CHAR (  3 )
!        S( 4: 4) = 'A'
!        S( 5: 5) = 'B'
!        S( 6: 6) = 'C'
!        S( 7: 7) = CHAR (  7 )
!        S( 8: 8) = CHAR (  8 )
!        S( 9: 9) = CHAR (  9 )
!        S(10:10) = ' '
!
!        WRITE (*,*) 'Non-blank from ', FRSTNB(S), ' to ', LASTNB(S)
!        WRITE (*,*) 'Printable from ', FRSTPC(S), ' to ', LASTPC(S)
!
!        END
!
!     produces te following output:
!
!        Non-blank from 2 to 9.
!        Printable from 4 to 6.
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
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
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
!     last printable character
!
!-&


!
!     Local variables
!
      INTEGER          I



!
!     Look for the last character in the range [33,126], and return
!     its index.
!
      DO I = LEN ( STRING ), 1, -1

         IF (       (ICHAR ( STRING(I:I) ) .GE.  33) &
              .AND. (ICHAR ( STRING(I:I) ) .LE. 126) ) THEN

            LASTPC = I
            RETURN
         END IF

      END DO

!
!     Still here? No printable characters. Return zero.
!
      LASTPC = 0

      RETURN
      END
