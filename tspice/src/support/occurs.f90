
!$Procedure OCCURS ( Count occurrences of a substring in a string )

      INTEGER FUNCTION OCCURS ( STR, SUB )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Count the number of times that a substring occurs within
!     a character string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     STRING
!     UTILITY
!
! $ Declarations

      CHARACTER*(*)         STR
      CHARACTER*(*)         SUB

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STR        I   Character string.
!     C          I   Target substring.
!
! $ Detailed_Input
!
!     STR         is an arbitrary character string.
!
!     SUB         is an arbitrary character string.
!
! $ Detailed_Output
!
!     The function returns the number of occurrences of the substring
!     within the string.
!
! $ Exceptions.
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Typically, this would be used to count the number of times
!     that a single character occurs within a string: for example,
!     to determine whether the number of left parentheses in an
!     expression matches the number of right parentheses.
!
!     The occurrences found by OCCURS are independent: that is,
!     the number of occurrences of 'XXX' in 'XXXXXXXX' is two,
!     and not six.
!
! $ Examples
!
!     The following code fragment checks to make sure that the
!     number of left parentheses in an expression matches the number
!     of right delimiters in the same expression.
!
!       IF ( OCCURS ( EXPR, '(' ) - OCCURS ( EXPR, ')' ) .NE. 0 ) THEN
!         WRITE (6,*) 'Parenthesis mismatch.'
!       END IF
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
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 9, 1994
!
!
!-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of META/2
!         software as of May 3, 1994
!
!
!     Version B1.0.0, 29-APR-1988 (WLT) (IMU)
!
!-&



!
!     Local variables
!

      INTEGER               LSTR
      INTEGER               LSUB
      INTEGER               I

      LSTR   = LEN ( STR )
      LSUB   = LEN ( SUB )

      I      = 0
      OCCURS = 0

      DO WHILE ( I .LE. LSTR - LSUB )

         IF ( STR(I+1:I+LSUB) .EQ. SUB ) THEN
            OCCURS = OCCURS + 1
            I      = I      + LSUB

         ELSE
            I      = I      + 1
         END IF

      END DO

      RETURN
      END
