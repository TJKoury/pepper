
!$Procedure UPTO ( Up to the next index of a substring )

      INTEGER FUNCTION UPTO ( STRING, SUBSTR, START )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Return up to (but not including) the index of the next occurrence
!     of a substring within a string, after some initial offset.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER, PARSING, SEARCH, STRING, TEXT
!
! $ Declarations

      CHARACTER*(*)         STRING
      CHARACTER*(*)         SUBSTR
      INTEGER               START

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   Input string.
!     SUBSTR     I   Target substring.
!     START      I   Begin searching here.
!
! $ Detailed_Input
!
!     STRING      is an arbitrary input string.
!
!     SUBSTR      is the target substring to be located.
!
!     START       is the location at which to begin searching. That is,
!                 the search is confined to STRING(START: ).
!
! $ Detailed_Output
!
!     The function returns one less than the next location of the
!     target substring within the string, or the length of the string
!     if the substring is not found.
!
! $ Exceptions
!
!     1) If START is greater than the length of the string, the
!        function returns zero.
!
!     2) If START is less than one it is treated as if were one.
!
! $ Particulars
!
!     UPTO is used primarily for extracting substrings bounded by
!     a delimiter. Because the function returns the length of the
!     string when the target substring is not found, the reference
!
!        NEXT = STRING ( START : UPTO ( STRING, SUBSTR, START ) )
!
!     is always legal.
!
! $ Examples
!
!     The following code fragment extracts (and prints) substrings
!     bounded by slash (/) characters.
!
!        BEGIN = 1
!        END   = BEGIN
!
!        DO WHILE ( END .NE. 0 )
!           END   = UPTO ( STR, '/', BEGIN )
!
!           WRITE (6,*) 'Next token is ', STR(BEGIN:END)
!
!           BEGIN = END + 2
!        END DO
!
!     Notice that the loop ends when BEGIN is greater than the length
!     of the string, causing the function to return zero.
!
!     Notice also that the last token in the string is printed whether
!     or not the string ends with a slash.
!
!     If STRING is
!
!        'first/second/third/fourth'
!
!     the output from the fragment is
!
!        Next token is first
!        Next token is second
!        Next token is third
!        Next token is fourth
!
!     Contrast this with the following fragment, written using the
!     intrinsic function INDEX.
!
!        BEGIN = 1
!        END   = BEGIN
!
!        DO WHILE ( END .NE. 0 )
!           I = INDEX ( STR(BEGIN: ), '/' )
!
!           IF ( I .GT. 0 ) THEN
!              END = BEGIN + I - 1
!           ELSE
!              END = LEN ( STR )
!           END IF
!
!           WRITE (6,*) 'Next token is ', STR(BEGIN:END)
!
!           BEGIN = END + 2
!
!           IF ( BEGIN .GT. LEN ( STR ) ) THEN
!              END = 0
!           END IF
!        END DO
! $ Files
!
!     None.
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
!     Version B1.0.0, 4-APR-1988, (IMU) (WLT)
!
!-&



!
!     Local variables
!
      INTEGER               STRLEN
      INTEGER               I
      INTEGER               B

!
!     Just like it says in the header.
!
      STRLEN = LEN ( STRING )
      B      = MAX ( 1, START )

      IF ( B .GT. STRLEN ) THEN
         UPTO = 0

      ELSE
         I = INDEX ( STRING(B: ), SUBSTR )

         IF ( I .GT. 0 ) THEN
            UPTO = B + I - 2
         ELSE
            UPTO = STRLEN
         END IF
      END IF

      RETURN
      END
