!$Procedure NBLEN ( Non blank length of a string )

      INTEGER FUNCTION  NBLEN ( STRING )

! $ Abstract
!
!     Return the non-blank length of a character string. (That is,
!     the index of the last non-blank character when the string is
!     left-justified.)
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASCII
!     CHARACTER
!     UTILITY
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
!     The function returns the non-blank length of STRING.
!
! $ Detailed_Input
!
!     STRING   is the input character string.
!
! $ Detailed_Output
!
!     The function returns the non-blank length of STRING. This is the
!     same as the index of the last non-blank character in the left
!     justified string. If STRING is blank, NBLEN is zero.
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
!     Locate the first and last non-blank characters in the string.
!     Subtract to get the non-blank length.
!
! $ Examples
!
!     The following examples illustrate the use of NBLEN.
!
!           NBLEN ( 'ABCDE' )             = 5
!           NBLEN ( 'AN EXAMPLE' )        = 10
!           NBLEN ( '   AN EXAMPLE  ' )   = 10
!           NBLEN ( '               ' )   = 0
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
!        Edited the header to comply with NAIF standard.
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
!     non-blank length of a string
!
!-&


!
!     SPICELIB functions
!
      INTEGER          FRSTNB
      INTEGER          LASTNB


!
!     Blank string is easy.
!
      IF ( STRING .EQ. ' ' ) THEN
         NBLEN = 0
      ELSE
         NBLEN = LASTNB ( STRING ) - FRSTNB ( STRING ) + 1
      END IF

      RETURN
      END
