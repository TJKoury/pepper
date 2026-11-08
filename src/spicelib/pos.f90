!$Procedure POS ( Position of substring )

      INTEGER FUNCTION POS ( STR, SUBSTR, START )

! $ Abstract
!
!     Find the first occurrence in a string of a substring, starting at
!     a specified location, searching forward.
!
! $ Required_Reading
!
!     SCANNING
!
! $ Keywords
!
!     CHARACTER
!     SEARCH
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         STR
      CHARACTER*(*)         SUBSTR
      INTEGER               START

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STR        I   A character string
!     SUBSTR     I   Substring to locate in the character string.
!     START      I   Where to start looking for SUBSTR in STR.
!
!     The function returns the index of SUBSTR in STR following START
!
! $ Detailed_Input
!
!     STR      is any character string.
!
!     SUBSTR   is a substring to look for in STR. Spaces in
!              SUBSTR are significant.
!
!     START    is the position in STR to begin looking for SUBSTR.
!
! $ Detailed_Output
!
!     The function returns the index of the beginning of the first
!     substring of STR that begins on or after index START and is equal
!     to SUBSTR. If the substring cannot be found after START, the
!     function is returns 0.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If START is less than 1, the search begins at the first
!         character of the string.
!
!     2)  If START is greater than the length of the string, POS
!         returns zero.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     POS is case sensitive.
!
!     An entire family of related SPICELIB routines (POS, CPOS, NCPOS,
!     POSR, CPOSR, NCPOSR) is described in the Required Reading.
!
!     Those familiar with the .TRUE. BASIC language should note that
!     these functions are equivalent to the .TRUE. BASIC intrinsic
!     functions with the same names.
!
! $ Examples
!
!     Let STRING = 'AN ANT AND AN ELEPHANT        '
!                   123456789012345678901234567890
!
!      Normal (Sequential) Searching:
!      ------------------------------
!
!            POS ( STRING, 'AN',  1 ) =  1
!            POS ( STRING, 'AN',  3 ) =  4
!            POS ( STRING, 'AN',  6 ) =  8
!            POS ( STRING, 'AN', 10 ) = 12
!            POS ( STRING, 'AN', 14 ) = 20
!            POS ( STRING, 'AN', 22 ) =  0
!
!      START out of bounds:
!      --------------------
!
!            POS ( STRING, 'AN', -5 ) =  1
!            POS ( STRING, 'AN',  0 ) =  1
!            POS ( STRING, 'AN', 31 ) =  0
!            POS ( STRING, 'AN', 44 ) =  0
!
!      Significance of Spaces:
!      -----------------------
!
!            POS ( STRING, 'AN',    1 ) =  1
!            POS ( STRING, ' AN',   1 ) =  3
!            POS ( STRING, ' AN ',  1 ) = 11
!            POS ( STRING, ' AN  ', 1 ) =  0
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
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.3, 31-JAN-2008 (BVS)
!
!        Removed non-standard end-of-declarations marker
!        'C%&END_DECLARATIONS' from comments.
!
!-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.1, 26-MAR-1991 (HAN)
!
!        The Required Reading file POSITION was renamed to SCANNING.
!        This header was updated to reflect the change.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     position of substring
!
!-&


!
!     Local variables
!
      LOGICAL               FOUND
      INTEGER               LENSTR
      INTEGER               OFFSET
      INTEGER               LCHNCE
      INTEGER               B


!
!     Let's find out how big every body is.
!
      LENSTR =  LEN   ( STR                    )
      OFFSET =  MAX   ( 0,  LEN ( SUBSTR ) - 1 )
      LCHNCE =  LENSTR - OFFSET

      B      =  MAX ( 1, START )

!
!     Look for the string until we run find it or run out of room to
!     look.
!
      FOUND  = .FALSE.
      POS    =  0

      DO WHILE ( .NOT. FOUND )

         IF ( B .GT. LCHNCE ) THEN
            RETURN
         ELSE IF ( STR(B:B+OFFSET) .EQ. SUBSTR ) THEN
            POS = B
            RETURN
         ELSE
            B   = B + 1
         END IF

      END DO

      RETURN
      END
