
!$Procedure MATCHO ( Match the characters in two words )

      INTEGER FUNCTION MATCHO ( WORD, GUESS )
      IMPLICIT NONE

! $ Abstract
!
!      Assign a score to a pair of words which reflects the closeness
!      of the words in terms of the characters they contain and the
!      order in which the characters appear.
!
! $ Keywords
!
!      SEARCH
!      UTILITY
!
! $ Declarations

      CHARACTER*(*)         WORD
      CHARACTER*(*)         GUESS

! $ Brief_I/O
!
!      Variable  I/O  Description
!      --------  ---  --------------------------------------------------
!      WORD       I   Word to be matched against initial guess.
!      GUESS      I   Initial guess.
!
! $ Detailed_Input
!
!      WORD       is a character string to be checked for a match
!                 against an initial guess. The (non-printing) ASCII
!                 characters 1 and 2 are ignored. Typically, WORD will
!                 contain a single word.
!
!      GUESS      is an initial guess at the value of the input
!                 word. The (non-printing) ASCII characters 1 and 2
!                 are ignored. Like WORD, this will typically be a
!                 single word.
!
! $ Detailed_Output
!
!      The function returns a score between 0 (indicating that WORD
!      and GUESS have no common character patterns) and 100 (indicating
!      that WORD and GUESS match very closely).
!
! $ Exceptions
!
!      1) If neither WORD nor GUESS contains any printing characters,
!         the function returns 0.
!
! $ Particulars
!
!      In order to determine whether a word (usually typed by a user)
!      matches any of a series of known words (keywords, for example),
!      it is necessary to be able to judge the "closeness" of an
!      arbitrary pair of words. Several algorithms exist which make
!      such a comparison, the best-known of which is probably the
!      Soundex algorithm.
!
!      The score assigned by MATCHO indicates not only how many of the
!      letters two words have in common, but also the relative
!      difference between the order in which these letters appear.
!
!      MATCHO does not assign higher weights to more exotic characters,
!      like Q and Z, since these are as likely to appear in mistyped
!      words as are any other characters. (Both Q and Z, for instance,
!      are adjacent to A on a standard keyboard.)
!
!      The score assigned by MATCHO is computed in this way.
!
!          Suppose WORD is the string
!
!             w_1 w_2  ... w_n
!
!          and GUESS is the string
!
!             g_1 g_2 ... g_m
!
!          Each of the MATCHW templates
!
!          * w_i * w_j *  (where i < j)
!
!          is matched against GUESS and the total number of
!          matches tallied.  (There are n(n-1)/2 such templates)
!
!          Additionally 1 extra point is awarded for each match of GUESS
!          with a template of the form
!
!          * w_i w_i+1 * .
!
!          The total tally is multiplied by 200/n(n-1) and truncated to
!          100 if necessary to yield a GUESS to WORD tally.
!
!          Then the roles of WORD and GUESS are reversed and an
!          identical proceedure is followed to obtain a WORD to GUESS
!          tally.  The average of the two tallies is returned in
!          MATCHO.
!
!      Empirically it has been found that WORD and GUESS are in
!      close agreement if MATCHO is returned with a value of 75
!      or more.  Users may wish to use higher or lower score when
!      determining when a match between two words is close.
!
! $ Examples
!
!
! $ Restrictions
!
!      1) MATCHO is case-insensitive. Lowercase characters match
!         uppercase characters, and vice versa.
!
! $ Common_Variables
!
!      None.
!
! $ Author_and_Institution
!
!      W.L. Taber     (JPL)
!      I.M. Underwood (JPL)
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
!      Version B 1.0.0, 7-APR-1988 (WLT) (IMU)
!
!-&

!
!     SPICELIB functions
!
      INTEGER               QRTRIM
      INTEGER               LTRIM
      LOGICAL               EQSTR

!
!     Local Parameters
!
      INTEGER               BASE
      PARAMETER           ( BASE = 1024 )

      INTEGER               MXWLEN
      PARAMETER           ( MXWLEN = 32 )

      INTEGER               MXPAIR
      PARAMETER           ( MXPAIR = MXWLEN*MXWLEN/2 )

      INTEGER               LSTCHR
      PARAMETER           ( LSTCHR = 255 )



!
!     Local variables
!
      INTEGER               C1
      INTEGER               C2

      INTEGER               G2C
      INTEGER               G2SEQ   ( MXWLEN )
      INTEGER               GCOUNT
      INTEGER               GF
      INTEGER               GL
      INTEGER               GLEN
      INTEGER               GMSCOR
      INTEGER               GP
      INTEGER               GPAIRS  ( MXPAIR )
      INTEGER               GSCORE
      INTEGER               GTALLY

      INTEGER               I
      INTEGER               J

      INTEGER               UVALUE ( 0 : LSTCHR )
      INTEGER               VALUE

      INTEGER               W2C
      INTEGER               W2SEQ   ( MXWLEN )
      INTEGER               WCOUNT
      INTEGER               WF
      INTEGER               WL
      INTEGER               WLEN
      INTEGER               WMSCOR
      INTEGER               WP
      INTEGER               WPAIRS  ( MXPAIR )
      INTEGER               WSCORE
      INTEGER               WTALLY

      LOGICAL               FIRST
      SAVE

      DATA FIRST / .TRUE. /

      DATA (UVALUE(I), I = 0, 99 ) &
           /   0,   1,   2,   3,   4,   5,   6,   7,   8,   9, &
              10,  11,  12,  13,  14,  15,  16,  17,  18,  19, &
              20,  21,  22,  23,  24,  25,  26,  27,  28,  29, &
              30,  31,  32,  33,  34,  35,  36,  37,  38,  39, &
              40,  41,  42,  43,  44,  45,  46,  47,  48,  49, &
              50,  51,  52,  53,  54,  55,  56,  57,  58,  59, &
              60,  61,  62,  63,  64,  65,  66,  67,  68,  69, &
              70,  71,  72,  73,  74,  75,  76,  77,  78,  79, &
              80,  81,  82,  83,  84,  85,  86,  87,  88,  89, &
              90,  91,  92,  93,  94,  95,  96,  97,  98,  99 /

      DATA (UVALUE(I), I = 100, 199 ) &
           / 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, &
             110, 111, 112, 113, 114, 115, 116, 117, 118, 119, &
             120, 121, 122, 123, 124, 125, 126, 127, 128, 129, &
             130, 131, 132, 133, 134, 135, 136, 137, 138, 139, &
             140, 141, 142, 143, 144, 145, 146, 147, 148, 149, &
             150, 151, 152, 153, 154, 155, 156, 157, 158, 159, &
             160, 161, 162, 163, 164, 165, 166, 167, 168, 169, &
             170, 171, 172, 173, 174, 175, 176, 177, 178, 179, &
             180, 181, 182, 183, 184, 185, 186, 187, 188, 189, &
             190, 191, 192, 193, 194, 195, 196, 197, 198, 199 /

      DATA (UVALUE(I), I = 200, 255 ) &
           / 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, &
             210, 211, 212, 213, 214, 215, 216, 217, 218, 219, &
             220, 221, 222, 223, 224, 225, 226, 227, 228, 229, &
             230, 231, 232, 233, 234, 235, 236, 237, 238, 239, &
             240, 241, 242, 243, 244, 245, 246, 247, 248, 249, &
             250, 251, 252, 253, 254, 255 /

!
!     Set up the case insensitive mapping.
!
      IF ( FIRST ) THEN

         FIRST = .FALSE.

         UVALUE( ICHAR('a') ) = ICHAR( 'A' )
         UVALUE( ICHAR('b') ) = ICHAR( 'B' )
         UVALUE( ICHAR('c') ) = ICHAR( 'C' )
         UVALUE( ICHAR('d') ) = ICHAR( 'D' )
         UVALUE( ICHAR('e') ) = ICHAR( 'E' )
         UVALUE( ICHAR('f') ) = ICHAR( 'F' )
         UVALUE( ICHAR('g') ) = ICHAR( 'G' )
         UVALUE( ICHAR('h') ) = ICHAR( 'H' )
         UVALUE( ICHAR('i') ) = ICHAR( 'I' )
         UVALUE( ICHAR('j') ) = ICHAR( 'J' )
         UVALUE( ICHAR('k') ) = ICHAR( 'K' )
         UVALUE( ICHAR('l') ) = ICHAR( 'L' )
         UVALUE( ICHAR('m') ) = ICHAR( 'M' )
         UVALUE( ICHAR('n') ) = ICHAR( 'N' )
         UVALUE( ICHAR('o') ) = ICHAR( 'O' )
         UVALUE( ICHAR('p') ) = ICHAR( 'P' )
         UVALUE( ICHAR('q') ) = ICHAR( 'Q' )
         UVALUE( ICHAR('r') ) = ICHAR( 'R' )
         UVALUE( ICHAR('s') ) = ICHAR( 'S' )
         UVALUE( ICHAR('t') ) = ICHAR( 'T' )
         UVALUE( ICHAR('u') ) = ICHAR( 'U' )
         UVALUE( ICHAR('v') ) = ICHAR( 'V' )
         UVALUE( ICHAR('w') ) = ICHAR( 'W' )
         UVALUE( ICHAR('x') ) = ICHAR( 'X' )
         UVALUE( ICHAR('y') ) = ICHAR( 'Y' )
         UVALUE( ICHAR('z') ) = ICHAR( 'Z' )

      END IF

!
!     First get the ``dimensions'' of our two words (first non-blank,
!     last non-blank, and non-blank length).
!
      GF   = LTRIM ( GUESS )
      GL   = QRTRIM ( GUESS )

      WF   = LTRIM ( WORD  )
      WL   = QRTRIM ( WORD  )

      GLEN = GL - GF + 1
      WLEN = WL - WF + 1

!
!     Perform some of the obvious checks first.
!
      IF (  EQSTR( WORD(WF:WL), GUESS(GF:GL) ) ) THEN

         MATCHO = 100
         RETURN

      ELSE IF (      ( WLEN .LE. 1 ) &
                .OR. ( GLEN .LE. 1 ) ) THEN

         MATCHO = 0
         RETURN

      END IF

!
!     Initialize the score keeper and compute the length of GUESS.
!
      WMSCOR = ( (WLEN - 1) * WLEN ) / 2
      GMSCOR = ( (GLEN - 1) * GLEN ) / 2

!
!     We will encode ordered letter pairs as
!
!        BASE * ICHAR(first)   +   ICHAR(second)
!
!     Where BASE is chosen large enough so that we will never have
!     different pairs mapping to the same integer.
!
!     Compute the encoded collection of ordered pairs for
!     the GUESS (GCOUNT is the number of general pairs
!     G2C is the number of 2 character substrings) ...
!
      GCOUNT = 0
      G2C    = 0

      DO I = GF,GL-1

         C1 = UVALUE(  ICHAR( GUESS(I  :I  ) ) )
         C2 = UVALUE(  ICHAR( GUESS(I+1:I+1) ) )

         G2C        = G2C       + 1
         G2SEQ(G2C) = BASE * C1 + C2

         DO J = I+1, GL

            C1             = UVALUE(  ICHAR( GUESS(I:I) ) )
            C2             = UVALUE(  ICHAR( GUESS(J:J) ) )

            GCOUNT         = GCOUNT  + 1
            GPAIRS(GCOUNT) = BASE*C1 + C2

         END DO
      END DO


!
!     ... then construct the encoded ordered letter pairs for WORD.
!
      WCOUNT = 0
      W2C    = 0

      DO I = WF,WL-1

         C1 = UVALUE(  ICHAR( WORD(I  :I  ) ) )
         C2 = UVALUE(  ICHAR( WORD(I+1:I+1) ) )

         W2C        = W2C       + 1
         W2SEQ(W2C) = BASE * C1 + C2

         DO J = I+1, WL

            C1             = UVALUE(  ICHAR( WORD(I:I) ) )
            C2             = UVALUE(  ICHAR( WORD(J:J) ) )

            WCOUNT         = WCOUNT  + 1
            WPAIRS(WCOUNT) = BASE*C1 + C2

         END DO

      END DO

!
!     Now sort the various arrays of encoded letter pairs
!
      CALL SHELLI ( G2C,    G2SEQ  )
      CALL SHELLI ( GCOUNT, GPAIRS )
      CALL SHELLI ( W2C,    W2SEQ  )
      CALL SHELLI ( WCOUNT, WPAIRS )

      G2SEQ (G2C   +1) = 0
      GPAIRS(GCOUNT+1) = 0
      W2SEQ (W2C   +1) = 0
      WPAIRS(WCOUNT+1) = 0

!
!     First tally up the matches of the form *L1*L2*.  This is
!     virtually the same algorithm used for computing set
!     intersections.
!
      WP = 1
      GP = 1

      WTALLY = 0
      GTALLY = 0

      DO WHILE (      ( WP .LE. WCOUNT ) &
                .AND. ( GP .LE. GCOUNT ) )

         IF      ( WPAIRS(WP) .LT. GPAIRS(GP) ) THEN

            WP = WP + 1

         ELSE IF ( WPAIRS(WP) .GT. GPAIRS(GP) ) THEN

            GP = GP + 1

         ELSE

            VALUE = WPAIRS(WP)

            DO WHILE (       ( WPAIRS(WP) .EQ. VALUE  ) &
                       .AND. ( WP         .LE. WCOUNT ) )

               WTALLY = WTALLY + 1
               WP     = WP     + 1

            END DO

            DO WHILE (       ( GPAIRS(GP) .EQ. VALUE  ) &
                       .AND. ( GP         .LE. GCOUNT ) )

               GTALLY = GTALLY + 1
               GP     = GP     + 1

            END DO

         END IF

      END DO


!
!     Next tally up the various matches of the form *L1L2*
!
      WP = 1
      GP = 1

      DO WHILE (      ( WP .LE. W2C ) &
                .AND. ( GP .LE. G2C ) )

         IF      ( W2SEQ(WP) .LT. G2SEQ(GP) ) THEN

            WP = WP + 1

         ELSE IF ( W2SEQ(WP) .GT. G2SEQ(GP) ) THEN

            GP = GP + 1

         ELSE

            VALUE = W2SEQ(WP)

            DO WHILE (       ( W2SEQ(WP) .EQ. VALUE  ) &
                       .AND. ( WP        .LE. W2C    ) )

               WTALLY = WTALLY + 1
               WP     = WP     + 1

            END DO

            DO WHILE (       ( G2SEQ(GP) .EQ. VALUE  ) &
                       .AND. ( GP        .LE. G2C    ) )

               GTALLY = GTALLY + 1
               GP     = GP     + 1

            END DO

         END IF

      END DO

      GTALLY =  MIN ( GTALLY, GMSCOR )
      WTALLY =  MIN ( WTALLY, WMSCOR )

      WSCORE =  ( 100 * WTALLY ) / WMSCOR
      GSCORE =  ( 100 * GTALLY ) / GMSCOR

      MATCHO =  MIN(  ( WSCORE + GSCORE ) / 2,  100 )

      RETURN
      END
