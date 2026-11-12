
!$Procedure MATCHE ( Match two words, allowing for common errors )

      SUBROUTINE MATCHE ( WORD, GUESS, TRANSF, LOC )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!      Determines whether or not two words may be the same,
!      allowing for common typing errors.
!
! $ Keywords
!
!      SEARCH
!      UTILITY
!
! $ Declarations

      CHARACTER*(*)         WORD
      CHARACTER*(*)         GUESS
      CHARACTER*(*)         TRANSF
      INTEGER               LOC

! $ Brief_I/O
!
!      Variable  I/O  Description
!      --------  ---  --------------------------------------------------
!      WORD       I   Word to be matched against initial guess.
!      GUESS      I   Initial guess.
!      TRANSF     O   Transformation that makes WORD match GUESS.
!      LOC        O   Location at which to apply transformation.
!
! $ Detailed_Input
!
!      WORD       is a character string to be checked for a match
!                 against an initial guess. Leading and trailing
!                 blanks are ignored. Typically, WORD will contain
!                 a single word. In any case, the significant part
!                 of WORD may not exceed 64 characters.
!
!      GUESS      is an initial guess at the value of the input word.
!                 Leading and trailing blanks are ignored. Like WORD,
!                 this will typically be a single word.
!
! $ Detailed_Output
!
!      TRANSF     is the name of a transformation which, when applied
!                 to WORD, makes WORD match with GUESS. The possible
!                 transformations are:
!
!                    'TRANSPOSE'   Transpose two characters.
!
!                    'REPLACE'     Replace a single character.
!
!                    'INSERT'      Insert an extra character.
!
!                    'REMOVE'      Remove a character.
!
!                    'IDENTITY'    Do nothing.
!
!                 These reflect some of the most common typing mistakes.
!                 If none if these transformations will do the trick,
!                 TRANSF is 'NONE'.
!
!      LOC        is the location at which the indicated transformation
!                 should be applied.
!
!                    When TRANSF is   LOC is
!                    --------------   ------
!                    'TRANSPOSE'      Location of the first character
!                                     to be transposed.
!
!                    'REPLACE'        Location of the character to be
!                                     replaced.
!
!                    'INSERT'         Location at which the character
!                                     should be inserted.
!
!                    'REMOVE'         Location of the character to be
!                                     removed.
!
!                    'IDENTITY'       Zero.
!
!                    'NONE'           Zero.
!
! $ Exceptions
!
!      None.
!
! $ Particulars
!
!      Some typing mistakes should be relatively easy to catch, since
!      the difference between the intended word and the typed word may
!      involve a single transformation. MATCHE applies the most common
!      transformations to an input word, and attempt to match the
!      resulting word to a an initial guess.
!
! $ Examples
!
!      Let
!
!         GUESS = 'APPLE'
!
!      Then
!
!         If WORD is        TRANSF is         LOC is
!         -----------       -------------     ------
!         'APPEL'           'TRANSPOSE'        4
!         'APPLY'           'REPLACE'          5
!         'DAPPLE'          'REMOVE'           1
!         'APPLES'          'REMOVE'           5
!         'PPLE'            'INSERT'           1
!         'APPE'            'INSERT'           4
!         'APPL'            'INSERT'           5
!         'APPLE'           'IDENTITY'         0
!         'APPEAL'          'NONE'             0
!
! $ Restrictions
!
!      1) MATCHE is case-sensitive. Lowercase characters do not match
!         uppercase characters, and vice versa.
!
!      2) ASCII characters 1 and 2 are used internally as wildcard
!         characters, and should not appear in either WORD or GUESS.
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
!      Version B 1.0.0, 5-APR-1988
!
!-&



!
!     SPICELIB functions
!
      INTEGER               QRTRIM

!
!     Local variables
!
      CHARACTER*(65)        COPY
      CHARACTER*(65)        TEMPL
      CHARACTER*(65)        MYGUES


      INTEGER               CLEN
      INTEGER               I


!
!     Save a copy of the first 64 significant characters in a buffer,
!     from which we may construct templates.
!
      COPY = ' '

      CALL LJUST ( WORD,  COPY(1:64) )
      CALL LJUST ( GUESS, MYGUES     )

      CLEN = QRTRIM ( COPY )

!
!     Apply the transformations one at a time, in the order most
!     likely to succeed:
!
!        Removal
!        Transposition
!        Replacement
!        Insertion
!
!     Quit as soon as a possible match is found.
!
!     Actually, we need to check for identity first. Otherwise,
!     we're likely to find a transposition that yields the same
!     word: for example, transposing the second and third letters
!     of APPLE yields APPLE.
!
      IF ( EQSTR( WORD, MYGUES ) ) THEN
         TRANSF = 'IDENTITY'
         LOC    = 0
         RETURN
      END IF


!
!     Removal
!     -------
!
!     Remove the character at each location, and check against MYGUES.
!
      DO I = 1, CLEN

         CALL REMSUB ( COPY, I, I, TEMPL )

         IF ( EQSTR( TEMPL, MYGUES ) ) THEN
            TRANSF = 'REMOVE'
            LOC    = I
            RETURN
         END IF
      END DO



!
!     Transposition
!     -------------
!
!     Transpose each pair of characters, and check against MYGUES.
!
      DO I = 1, CLEN - 1

         TEMPL          = COPY
         TEMPL(I  :I  ) = COPY(I+1:I+1)
         TEMPL(I+1:I+1) = COPY(I  :I  )

         IF ( EQSTR( TEMPL, MYGUES ) ) THEN
            TRANSF = 'TRANSPOSE'
            LOC    = I
            RETURN
         END IF

      END DO



!
!     Replacement
!     -----------
!
!     Replace each character with a wild character, and check
!     against MYGUES.
!
      DO I = 1, CLEN

         TEMPL      = COPY
         TEMPL(I:I) = MYGUES(I:I)

         IF ( EQSTR( TEMPL, MYGUES ) ) THEN
            TRANSF = 'REPLACE'
            LOC    = I
            RETURN
         END IF
      END DO



!
!     Insertion
!     ---------
!
!     Insert a wild character at each location, and check against
!     MYGUES.
!
      DO I = 1, CLEN + 1


         IF ( I .EQ. 1 ) THEN
            TEMPL(1:1) = MYGUES(1:1)
            TEMPL(2:)  = COPY
         ELSE IF ( I .EQ. CLEN + 1 ) THEN
            TEMPL      = COPY
            TEMPL(I:I) = MYGUES(I:I)
         ELSE
            TEMPL(1  :I-1) = COPY(1:I-1)
            TEMPL(I  :I  ) = MYGUES(I:I)
            TEMPL(I+1:   ) = COPY(I:)
         END IF

         IF ( EQSTR(TEMPL, MYGUES) ) THEN
            TRANSF = 'INSERT'
            LOC    = I
            RETURN
         END IF
      END DO




!
!     None of these transformations work.
!
      TRANSF = 'NONE'
      LOC    = 0

      RETURN
      END
