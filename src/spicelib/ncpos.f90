!$Procedure NCPOS ( NOT character position )

      INTEGER FUNCTION NCPOS ( STR, CHARS, START )

! $ Abstract
!
!     Find the first occurrence in a string of a character NOT belonging
!     to a collection of characters, starting at a specified location,
!     searching forwards.
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
      CHARACTER*(*)         CHARS
      INTEGER               START

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STR        I   Any character string.
!     CHARS      I   A collection of characters.
!     START      I   Position to begin looking for one not in CHARS
!
!     The function returns the index of the first character of STR
!     at or following index START that is not in the collection CHARS.
!
! $ Detailed_Input
!
!     STR      is any character string.
!
!     CHARS    is a character string containing a collection of
!              characters. Spaces in CHARS are significant.
!
!     START    is the position in STR to begin looking for
!              characters not in CHARS.
!
! $ Detailed_Output
!
!     The function returns the index of the first character of STR (at
!     or following index START) that is not one of the characters in the
!     string CHARS. If no such character is found, the function returns
!     zero.
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
!     2)  If START is greater than the length of the string, NCPOS
!         returns zero.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     NCPOS is case sensitive.
!
!     An entire family of related SPICELIB routines (POS, CPOS, NCPOS,
!     POSR, CPOSR, NCPOSR) is described in the Required Reading.
!
!     Those familiar with the True BASIC language should note that
!     these functions are equivalent to the True BASIC intrinsic
!     functions with the same names.
!
! $ Examples
!
!     Let STRING = 'BOB, JOHN, TED, AND MARTIN    '
!                   123456789012345678901234567890
!
!     Let CHAR   = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
!
!     Normal (Sequential) Searching:
!     ------------------------------
!
!           NCPOS( STRING, CHAR,  1 ) = 4
!           NCPOS( STRING, CHAR,  5 ) = 5
!           NCPOS( STRING, CHAR,  6 ) = 10
!           NCPOS( STRING, CHAR, 11 ) = 11
!           NCPOS( STRING, CHAR, 12 ) = 15
!           NCPOS( STRING, CHAR, 16 ) = 16
!           NCPOS( STRING, CHAR, 17 ) = 20
!           NCPOS( STRING, CHAR, 21 ) = 27
!           NCPOS( STRING, CHAR, 28 ) = 28
!           NCPOS( STRING, CHAR, 29 ) = 29
!           NCPOS( STRING, CHAR, 30 ) = 30
!           NCPOS( STRING, CHAR, 31 ) =  0
!
!     START out of bounds:
!     --------------------
!
!           NCPOS( STRING, CHAR, -12 ) = 4
!           NCPOS( STRING, CHAR,   0 ) = 4
!           NCPOS( STRING, CHAR,  31 ) = 0
!           NCPOS( STRING, CHAR, 123 ) = 0
!
!     Order within CHARS:
!     -------------------
!
!           NCPOS( STRING, 'JOHN', 7 ) = 10
!           NCPOS( STRING, 'OHJN', 7 ) = 10
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
!     forward search for position of unlisted character
!
!-&


!
!     Local variables
!
      LOGICAL               FOUND
      INTEGER               LENSTR
      INTEGER               B


      LENSTR = LEN ( STR )

      B      = MAX(1,START)
      FOUND  = .FALSE.
      NCPOS   = 0

      DO WHILE ( .NOT. FOUND )

         IF ( B .GT. LENSTR ) THEN
            RETURN
         ELSE IF ( INDEX( CHARS, STR(B:B) ) .EQ. 0 ) THEN
            NCPOS = B
            RETURN
         ELSE
            B = B + 1
         END IF

      END DO

      RETURN
      END
