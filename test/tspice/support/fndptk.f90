!$Procedure      FNDPTK ( Find the previous token in a string )

      SUBROUTINE FNDPTK ( STRING, DELIMS, START, BEG, END )
      IMPLICIT NONE

! $ Abstract
!
!      Find the previous token in a string delimited by multiple
!      delimiters.
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
!                  previous token in the string.  To search for tokens
!                  in a string begin with START = LEN(STRING) + 2 and
!                  for subsequent calls set START to BEG, where BEG
!                  was returned by the previous call.
!
! $ Detailed_Output
!
!      BEG         is the beginning of the substring containing the
!                  token.
!
!      END         is the end of the substring containing the token.
!
! $ Parameters
!
!     None.
!
!
! $ Exceptions
!
!      1.  If START is more than two greater than the length of the
!          string it will be treated as though its length is two more
!          than the length of the string.  Then if there is a null
!          string at the end of the string BEG will point to
!          LEN(STRING) + 1, otherwise it will point to the beginning
!          of the last token in the string.
!
!      2.  If START is LEN(STRING) + 1, BEG will point to the beginning
!          of the last token that preceeds the end of the string.
!
!      2.  If START is at less than or equal to 1, BEG and END will be
!          returned as zero.
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
!          the substring ',    ,' .  Is there a delimiter between
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
!      end of the last token that ends strictly before the input
!      START position in the string.  If no tokens preceeded the input
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
!             START = LEN ( STRING ) + 2
!
!             DO WHILE ( BEG .NE. 0 )
!
!                CALL FNDPTK ( STRING, DELIMS, START, BEG, END )
!
!                   do something with the token STRING(BEG:END) taking
!                   appropriate care of the null tokens.
!
!                START = BEG
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

! $ Index_Entries
!
!     Find a token preceding a location in a string
!
!-&


!
!     SPICE funtions.
!
      LOGICAL               RETURN
      INTEGER               CPOSR
      INTEGER               NCPOS
      INTEGER               NCPOSR
!
!     Local variables
!

      INTEGER               LAST
      INTEGER               EOL
      INTEGER               B
      LOGICAL               ATDELM
      LOGICAL               ONSPCE

!%&END_DECLARATIONS

!
!     Standard SPICE error handling
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'FNDPTK' )
      END IF

!
!     First we gather some data regarding the input string and
!     delimiters
!
      LAST  = LEN  ( STRING        )
      EOL   = LAST + 1
      B     = MIN( EOL+1, START )

!
!     We don't have to do anything if we are starting past the end of
!     the string.
!
      IF ( B .LT. 1 ) THEN
         BEG = 0
         END = 0
         CALL  CHKOUT ( 'FNDPTK' )
         RETURN
      END IF

      IF ( B .LT. EOL ) THEN
         ONSPCE = STRING(B:B) .EQ. ' '
      ELSE
         ONSPCE = .FALSE.
      END IF

!
!     Are we currently pointing at a delimiter?
!
      IF      ( B                            .GT. EOL ) THEN
         ATDELM = .FALSE.
      ELSE IF ( B                            .EQ. EOL ) THEN
         ATDELM = .TRUE.
      ELSE IF ( INDEX( DELIMS, STRING(B:B) ) .NE. 0   ) THEN
         ATDELM = .TRUE.
      ELSE
         ATDELM = .FALSE.
      END IF


      IF ( ATDELM ) THEN
!
!        Yes.  Move left to a non-blank character
!
         B = NCPOSR( STRING, ' ', B-1 )

!
!        If we didn't find a non-blank, then there is not a previous
!        token.
!
         IF ( B .EQ. 0 ) THEN

            BEG = 0
            END = 0
            CALL  CHKOUT ( 'FNDPTK' )
            RETURN

         END IF
!
!        Still here? Are we currently pointing at a delimiter?
!
         IF ( INDEX( DELIMS, STRING(B:B) ) .NE. 0   ) THEN

!
!           Yes. Move left to a non-blank.
!
            B = NCPOSR( STRING, ' ', B-1 )
         END IF

!
!        Move left to a delimiter, then Move right 1
!
         B = CPOSR ( STRING, DELIMS, B ) + 1

!
!     Are we on a space?
!
      ELSE IF  ( ONSPCE ) THEN

!
!        Yes.  (note: space is not a delimiter ) Find the next
!        non-blank to the right.
!
         B = NCPOS ( STRING, ' ', B )

!
!        Is this a delimiter?
!
         IF ( B .EQ. 0 ) THEN

!
!           it was all blanks to the end of the string.  Make the
!           B point to the end + 1, that is a delimiter
!
            B = EOL
            B = CPOSR ( STRING, DELIMS, B )

         ELSE IF ( INDEX(DELIMS, STRING(B:B)) .EQ. 0 ) THEN

!
!           No.   Move left to the first delimiter.
!
            B = CPOSR( STRING, DELIMS, B )

!
!           If we ran off the front of the string without hitting a
!           delimiter, there isn't a previous token.  Checkout and
!           head for home.
!
            IF ( B .EQ. 0 ) THEN
               BEG = 0
               END = 0
               CALL  CHKOUT ( 'FNDPTK' )
               RETURN
            END IF

         END IF

!
!        Move left to the first delimiter.
!        Move right 1
!
         B = CPOSR( STRING, DELIMS, B-1 ) + 1

      ELSE
!
!     Otherwise
!
!        Move left to the first delimiter.
!

         IF ( B .GT. EOL ) THEN
            B = EOL
         ELSE
            B = CPOSR ( STRING, DELIMS, B )

!
!           B is now pointing at a delimiter.
!
         END IF

!----------
         IF ( B .EQ. 0 ) THEN
            BEG = 0
            END = 0
            CALL  CHKOUT ( 'FNDPTK' )
            RETURN
         END IF
!
!        Move left to the first non-blank  (here or to the left)
!
         IF ( B .LT. EOL ) THEN

            B = NCPOSR ( STRING, ' ', B )

!
!           B is now pointing to the first non-blank character to the
!           left of the token we started in.
!
            IF (       ( INDEX ( DELIMS, STRING(B:B) ) .NE. 0   ) &
                 .AND. ( INDEX ( DELIMS, ' '         ) .NE. 0   ) &
                 .AND. ( STRING(B-1:B-1)               .EQ. ' ' ) )THEN

!
!              Move backwards to the true delimiter for the token
!              that ends here.
!
               B = NCPOSR ( STRING, ' ', B - 1 ) + 1

            END IF

         ELSE

!
!           If we were at or beyond the EOL position, we need to
!           know if backing up to a non-blank puts us on a delimiter
!           or not.  If it does reset B to EOL.
!
            B = NCPOSR ( STRING, ' ', B )

            IF ( INDEX ( DELIMS, STRING(B:B) ) .NE. 0 ) THEN
               B = EOL
            END IF

         END IF


!
!        Move left to the first deliter, and then move right 1.
!
         B = CPOSR ( STRING, DELIMS, B-1 ) + 1

      END IF

      CALL FNDNTK ( STRING, DELIMS, B, BEG, END )


      CALL  CHKOUT ( 'FNDPTK' )
      RETURN

      END
