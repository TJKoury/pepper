!$Procedure      FNDNTK ( Find the next token in a string )

      SUBROUTINE FNDNTK ( STRING, DELIMS, START, BEG, END )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!      Find the next token in a string delimited by multiple delimiters.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      CHARACTER,  STRING,  PARSING
!
! $ Declarations

      CHARACTER*(*)         STRING
      CHARACTER*(*)         DELIMS
      INTEGER               START
      INTEGER               BEG
      INTEGER               END

! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      STRING     I    String of items delimited by DELIMS.
!      DELIMS     I    Single characters which delimit items.
!      START      I    Point to start looking for a token in the string.
!      BEG        O    Beginning index of the token.
!      END        O    End index of the token.
!
! $ Detailed_Input
!
!      STRING      is a character string containing tokens delimited
!                  by any one of the characters in the string DELIMS.
!                  Consecutive delimiters, and delimiters at the
!                  beginning and end of the string, are considered to
!                  delimit null items. A blank string is considered to
!                  contain a single (blank) item.
!
!      DELIMS      contains the individual characters which delimit
!                  the items in the string. These may be any ASCII
!                  characters, including blanks.
!
!                  However, by definition, consecutive blanks are NOT
!                  considered to be consecutive delimiters. Nor is
!                  a blank and any other delimiter considered to be
!                  consecutive delimiters. In addition, leading and
!                  trailing blanks are ignored.  (See "Particulars"
!                  for a discussion of blanks and how they are treated.)
!
!      START       is the point in the string to begin looking for the
!                  next token in the string.  To search for tokens in
!                  a string begin with START = 1, and for subsequent
!                  calls set START to END + 2, where END was returned
!                  by the previous call.
!
! $ Detailed_Output
!
!      BEG         is the beginning of the substring containing the
!                  token.
!
!      END         is the end of the substring containing the token.
!
! $ Exceptions
!
!      1.  If START is less than 1 it will be treated as though it were
!          1.
!
!      2.  If START is the declared length of the string plus 1 and the
!          last non-blank character is a delimiter (or the string is
!          blank) START will be regarded as pointing at a null token.
!          BEG = LEN(STRING) + 1, END = LEN(STRING).
!
!      3.  If START is at least two greater than the declared length of
!          the string, BEG and END will be returned as zero.
!
!
! $ Particulars
!
!
!      For the purposes of discussion, we regard STRING to be a
!      substring of the string that begins with a meta-delimiter
!      followed by STRING and ends with a meta-delimiter.  The
!      meta-delimiters have indexes 0 and LEN(STRING)+1.
!      Meta-delimiters are non-blank delimiters.
!
!      A token is a substring that
!
!         1.  begins with a non-blank character,
!         2.  ends   with a non-blank character,
!         3.  contains no delimiters
!         4.  cannot be extended on either end without violating
!             on of the first 3 conditions.
!
!      A good question to ask at this point is:
!
!         "Suppose that ',' is a delimiter and the string contains
!          the substring ',    ,' .  Is there a token between
!          the two commas?
!
!      Our answer to this question is "Yes".  But from the rules
!      1 through 4 above, whatever it is can contain no characters.
!      We call such a token a null token.  Another question:
!
!         "Ok. There's a token. Where does it begin and end?"
!
!      Now we have to adopt some convention.  The only consistent
!      one we could think of was this:  The null token begins at
!      the second delimiter and ends at the previous character.
!
!      Beginning at the second delimiter seems reasonable.  The
!      only consistent way to define the end is to give an index
!      such that the length computation END - BEG + 1 yields 0.
!      So whatever, we do for the beginning, end must be BEG - 1.
!
!      Choosing the beginning to be the second of the two delimiters
!      makes it possible to easily move on to the next delimiter.
!      If the assignment START = END + 2 is made after a call to
!      the routine, then START will always point beyond the end
!      of the token just found and will always point no further
!      than the beginning of the next token ( if there is one).
!      If we keep in mind that there are meta-delimiters at the ends
!      of the string then a string that begins with ',    ...'
!      begins with a null token. A string that ends with ...   ,  '
!      ends with a null token.  In the first case the beginning
!      of the null token is at character 1 of the string. In the second
!      case the null token begins at LEN(STRING) + 1,  i.e. at the
!      meta-delimiter past the end of the string.
!
!      Using these conventions, this routine finds the beginning and
!      end of the first token that begins at or following the input
!      START position in the string.  If no tokens follow the input
!      index, then both BEG and END will be returned as zero.  This is
!      the only case in which BEG will be returned as non-positive.
!
! $ Examples
!
!      STRING =
!
!      'A FEW OF US, THE BAD-BOYS, WENT TO TOWN IN 8//1984-'
!
!                1         2         3         4         5
!       123456789012345678901234567890123456789012345678901
!
!       If DELIMS = ' ,-/'
!
!          Tokens      BEG      END
!          ------      ---      ---
!          'A'          1        1
!          'FEW'        3        5
!          'OF'         7        8
!          'US'        10       11
!          'THE'       14       16
!          'BAD'       18       20
!          'BOYS'      22       24
!          'WENT'      28       31
!          'TO'        33       34
!          'TOWN'      36       39
!          'IN'        41       42
!          '8'         44       44
!           null       46       45
!          '1984'      47       50
!           null       52       51
!
!
!       If DELIMS = ',/'
!
!          Tokens                BEG      END
!          ------                ---      ---
!          'A FEW OF US'           1       11
!          'THE BAD-BOYS'         18       25
!          'WENT TO TOWN IN 8'    28       44
!           null                  46       45
!          '1984-'                47       51
!
!
!       To get all of the tokens in a string the following loop of code
!       will suffice
!
!
!             BEG   = 1
!             START = 1
!
!             DO WHILE ( BEG .NE. 0 )
!
!                CALL FNDNTK ( STRING, DELIMS, START, BEG, END )
!
!                   do something with the token STRING(BEG:END) taking
!                   appropriate care of the null tokens.
!
!                START = END + 2
!
!             END DO
!
!
! $ Restrictions
!
!      None.
!
! $ Files
!
!      None.
!
! $ Author_and_Institution
!
!      I.M. Underwood (JPL)
!      W.L. Taber     (JPL)
!
! $ Literature_References
!
!      None.
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
!     Version B1.0.0, 3-MAY-1988 (WLT) (IMU)
!
!-&



!
!     Local variables
!

      LOGICAL               SPACE
      INTEGER               LAST
      INTEGER               EOL
      INTEGER               B
      LOGICAL               BLANK
      INTEGER               NBR
      INTEGER               NBL
      LOGICAL               DELIMR
      LOGICAL               DELIML
      LOGICAL               NODELM

!%&END_DECLARATIONS


!
!     First we gather some data regarding the input string and
!     delimiters
!
      SPACE = INDEX( DELIMS, ' ' ) .NE. 0
      LAST  = LEN  ( STRING        )
      EOL   = LAST + 1
      B     =  MAX( 1, START )

!
!     We don't have to do anything if we are starting past the end of
!     the string.
!
      IF ( B .GT. EOL ) THEN
         BEG = 0
         END = 0
         RETURN
      END IF

!
!     Find the first non-blank character at or to the right of where
!     we are starting.
!
      BLANK = .TRUE.
      NBR   =  B

      DO WHILE ( BLANK )

         IF      ( NBR  .GE. EOL )            THEN
            BLANK = .FALSE.
         ELSE IF ( STRING(NBR:NBR) .NE. ' ' ) THEN
            BLANK = .FALSE.
         ELSE
            NBR    = NBR + 1
         END IF

      END DO

!
!     Find the first non-blank character and first non-blank delimiter
!     to the left of the starting point.
!
      BLANK  = .TRUE.
      NBL    = B - 1

      DO WHILE ( BLANK )

         IF ( NBL .LE. 0 ) THEN
            BLANK = .FALSE.
         ELSE IF ( STRING(NBL:NBL) .NE. ' ' ) THEN
            BLANK = .FALSE.
         ELSE
            NBL   = NBL - 1
         END IF
      END DO

!
!     If both the preceeding non-blank character and the following
!     non-blank character are delimiters, we have a null item.
!
      IF ( NBR .GE. EOL ) THEN
         DELIMR = .TRUE.
      ELSE
         DELIMR = INDEX( DELIMS, STRING(NBR:NBR) ) .NE. 0
      END IF

      IF ( NBL .LE. 0   ) THEN
         DELIML = .TRUE.
      ELSE
         DELIML = INDEX( DELIMS, STRING(NBL:NBL) ) .NE. 0
      END IF

      IF ( DELIMR .AND. DELIML ) THEN

         BEG = NBR
         END = BEG - 1

         RETURN

      END IF

!
!     Still here? See if we were past the last delimiter.
!
      IF ( ( NBR .GE. EOL ) .AND. ( .NOT. DELIML) )  THEN

         BEG = 0
         END = 0
         RETURN

      END IF

!
!     If the left most non-blank is a delimiter OR a blank is a
!     delimiter and the non-blank character to the left is at least
!     two characters away from the right non-blank character, then
!     we have a token beginning at the right non-blank. We just need
!     to find the right boundary.
!
      IF ( DELIML .OR. (       ( NBR - NBL .GE. 2 ) &
                         .AND. ( SPACE            ) &
                         .AND. ( .NOT. DELIMR     ) ) ) THEN

         BEG = NBR
         END = BEG

!
!        Note: DELIMR is already .FALSE. or else we couldn't get to
!        this point.
!
         DO WHILE ( .NOT. DELIMR )

            IF ( END + 1 .GE. EOL ) THEN
               DELIMR = .TRUE.
            ELSE IF ( INDEX( DELIMS, STRING(END+1:END+1) ) .NE. 0 ) THEN
               DELIMR = .TRUE.
            ELSE
               END    = END + 1
            END IF

         END DO

!
!        Back up END to the first non-blank that precedes it.
!
         DO WHILE ( STRING(END:END) .EQ. ' ')
            END = END - 1
         END DO

         RETURN
      END IF

!
!     Still here? In that case we were in the middle of something
!     to start with.  Move the pointer forward until we reach a
!     delimiter.
!
!     Keep in mind that DELIMR still has the information as to whether
!     or not NBR points to a non-blank delimiter. We are going to use
!     this information to determine whether to look for a delimiter
!     first or not.
!
      IF ( .NOT. DELIMR ) THEN

         NODELM = .TRUE.
         B      =  NBR

         DO WHILE ( NODELM )

            NBR    = NBR + 1

            IF ( NBR .GE. EOL ) THEN
               NODELM = .FALSE.
            ELSE
               NODELM = INDEX ( DELIMS, STRING(NBR:NBR) ) .EQ. 0
            END IF

         END DO


!
!        If a space is a delimiter and we happen to have landed on one,
!        we want to continue until we hit a non-blank delimiter or just
!        before a non-blank character.
!
         IF ( SPACE .AND. NBR .LT. EOL ) THEN

            NODELM = STRING(NBR:NBR) .EQ. ' '

            DO WHILE ( NODELM )

               NBR = NBR + 1

               IF ( NBR .EQ. EOL ) THEN
                  NODELM = .FALSE.
               ELSE IF ( INDEX ( DELIMS, STRING(NBR:NBR) ) .NE. 0 ) THEN
                  NODELM = STRING(NBR:NBR) .EQ. ' '
               ELSE IF (                 STRING(NBR:NBR) .NE. ' ' ) THEN
                  NODELM = .FALSE.
!
!                 Back up one, to just before the non-blank character
!
                  NBR    = NBR - 1
               END IF
            END DO
         END IF
!
!        Since we did not start on a delimiter if we reached the end of
!        the string before hitting one, then there is no token to find
!        here.
!
         IF ( NBR .GE. EOL ) THEN

            BEG = 0
            END = 0

            RETURN

         END IF
      END IF

!
!     Still here?  Then starting at the first character to the right of
!     the delimiter, find the next non-blank character, and the next
!     right delimiter after that.
!
      NBL   =  NBR
      BLANK = .TRUE.

      DO WHILE (BLANK)

         NBL = NBL + 1

         IF ( NBL .GE. EOL ) THEN
            BLANK = .FALSE.
         ELSE
            BLANK = STRING(NBL:NBL) .EQ. ' '
         END IF

      END DO

!
!     Now locate the next delimiter.
!
      NBR    = NBL - 1
      DELIMR = .FALSE.

      DO WHILE ( .NOT. DELIMR )

         NBR =  NBR + 1

         IF ( NBR .GE. EOL ) THEN
            DELIMR = .TRUE.
         ELSE
            DELIMR = INDEX( DELIMS, STRING(NBR:NBR) ) .NE. 0
         END IF

      END DO

      BEG = NBL
      END = NBR-1

      IF ( END .GT. BEG ) THEN

!
!        Backup until we are at a non-space.
!
         DO WHILE ( ( STRING(END:END) .EQ. ' ' ) .AND. (END .GT. BEG) )
            END = END - 1
         END DO

      END IF

      RETURN

      END
