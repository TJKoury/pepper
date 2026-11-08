!$Procedure SCANRJ ( Scan --- reject tokens )

      SUBROUTINE SCANRJ ( IDS, N, NTOKNS, IDENT, BEG, END )

! $ Abstract
!
!     Reject those tokens descriptors whose identities are among those
!     of a specific collection.
!
! $ Required_Reading
!
!     SCANNING
!
! $ Keywords
!
!     SEARCH
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               IDS  ( * )
      INTEGER               N
      INTEGER               NTOKNS
      INTEGER               IDENT  ( * )
      INTEGER               BEG    ( * )
      INTEGER               END    ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IDS        I   value of id's of tokens that should be dumped.
!     N          I   number of id's.
!     NTOKNS    I-O  number of tokens input. The number kept.
!     IDENT     I-O  identity of each of the tokens.
!     BEG       I-O  indices of beginning of tokens.
!     END       I-O  indices of endings of tokens.
!
! $ Detailed_Input
!
!     IDS      is a list of the identity codes that we will want to
!              reject.
!
!     N        is the number of different cases.
!
!     NTOKNS   is the number of tokens to consider.
!
!     IDENT    holds the identities of each token that is up for
!              consideration.
!
!     BEG      holds the beginning indices of each token being
!              considered.
!
!     END      holds the ending indices of each token being
!              considered.
!
! $ Detailed_Output
!
!     NTOKNS   is the number of tokens remaining after the rejection
!              process has been completed.
!
!     IDENT    holds the identities of each token remaining.
!
!     BEG      holds the beginning indices of each token remaining.
!
!     END      holds the ending indices of each token remaining.
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
!     This routine serves as a macro for the rejection process that
!     is typically performed to remove tokens whose ID's fall into
!     some set.
!
! $ Examples
!
!     Suppose you wished to scan a string to locate the beginning and
!      endings of words together with punctuation, but that you did not
!      want to keep white space. The following code fragment illustrates
!      how you could use this routine to accomplish this task.
!
!      Words will be delimited by spaces, periods, commas, colons,
!      question marks, exclamation marks, semicolons, parentheses,
!      m-dashes, and quotes.
!
!      MARKS(1)  = ' '
!      MARKS(2)  = '.'
!      MARKS(3)  = ','
!      MARKS(4)  = '?'
!      MARKS(5)  = '!'
!      MARKS(6)  = '---'
!      MARKS(7)  = ':'
!      MARKS(8)  = ';'
!      MARKS(9)  = '('
!      MARKS(10) = ')'
!      MARKS(11) = '"'
!
!      NMARKS    = 11
!
!      IDS(1)    = 0
!      N         = 1
!
!
!      CALL SCANPR ( NMARKS, MARKS,  MRKLEN, MRKPTR )
!
!      IDS(1)    = BSRCHC ( ' ', NMARKS, MARKS )
!      N         = 1
!
!      CALL SCAN   ( STRING, MARKS,  MRKLEN, MRKPTR,
!     .              ROOM,   NTOKNS, IDENT,  BEG,    END )
!
!      CALL SCANRJ ( IDS, N, NTOKNS, IDENT,  BEG,    END )
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
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 02-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 26-JUL-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     Remove tokens from a scanned list of tokens
!
!-&


!
!     SPICELIB functions
!
      INTEGER               ISRCHI

!
!     Local variables
!
      INTEGER               I
      INTEGER               J


!
!     There's not much to do, shift forward the token attributes for
!     tokens whose identities don't belong to the rejection list.
!
      J = 0

      DO I = 1, NTOKNS

         IF ( ISRCHI(IDENT(I), N, IDS) .EQ. 0 ) THEN

            J        = J + 1
            IDENT(J) = IDENT(I)
            BEG  (J) = BEG  (I)
            END  (J) = END  (I)

         END IF

      END DO

      NTOKNS = J

      RETURN
      END
