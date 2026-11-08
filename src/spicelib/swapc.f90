!$Procedure SWAPC ( Swap character values )

      SUBROUTINE SWAPC ( A, B )

! $ Abstract
!
!     Swap the contents of two character strings.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    A
      CHARACTER*(*)    B

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A         I-O  First string.
!     B         I-O  Second string.
!
! $ Detailed_Input
!
!     A,
!     B        are two character strings, the contents of which
!              are to be swapped (exchanged).
!
! $ Detailed_Output
!
!     A,
!     B        are the same two character strings, after their
!              contents have been exchanged.
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
!     This is just shorthand notation for the code fragment
!
!           TEMP = A
!           A    = B
!           B    = TEMP
!
!     The characters in the string are swapped one at a time, so
!     no intermediate string (TEMP) is needed. This means that the
!     strings may be of any length.
!
! $ Examples
!
!     Let
!           A = 11.D0
!           B = 22.D0
!
!     Then after calling SWAPD (A,B),
!
!           A = 22.D0
!           B = 11.D0
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
!-    SPICELIB Version 1.1.0, 18-MAR-2021 (JDR)
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
!     swap character values
!
!-&


!
!     Local variables
!
      INTEGER          ALEN
      INTEGER          BLEN
      INTEGER          SHORT

      CHARACTER*1      TEMP
      INTEGER          I


!
!     Get the lengths of the strings.
!
      ALEN  = LEN ( A )
      BLEN  = LEN ( B )
      SHORT = MIN ( ALEN, BLEN )

!
!     Keep going until the end of the shorter string is reached.
!
      DO I = 1, SHORT

         TEMP   = A(I:I)
         A(I:I) = B(I:I)
         B(I:I) = TEMP

      END DO

!
!     If either string is longer than the shortest one, pad it
!     with blanks.
!
      IF ( ALEN .GT. SHORT ) THEN
         A(SHORT+1: ) = ' '

      ELSE IF ( BLEN .GT. SHORT ) THEN
         B(SHORT+1: ) = ' '
      END IF

      RETURN
      END
