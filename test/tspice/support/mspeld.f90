!$Procedure      MSPELD ( Misspelling diagnosis )

      SUBROUTINE MSPELD ( WORD, GUESS, CAUSE )
      IMPLICIT NONE

! $ Abstract
!
!     Diagnose possible spelling errors that might cause a word
!     to differ from another (known) word.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     COMPARE
!     ERROR
!     PARSING
!     UTILITY
!     WORD
!
! $ Declarations

      CHARACTER*(*)         WORD
      CHARACTER*(*)         GUESS
      CHARACTER*(*)         CAUSE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     WORD       I   A word that is thought to be misspelled.
!     GUESS      I   A word that is thought to be "close" to WORD.
!     CAUSE      O   A message indicating the difference between them.
!
! $ Detailed_Input
!
!     WORD       A word that is thought to be misspelled.
!
!     GUESS      A word that is thought to be "close" to WORD.
!
! $ Detailed_Output
!
!     CAUSE      A message that indicates the difference between WORD
!                and GUESS.
!
! $ Exceptions
!
!     1) CAUSE is blank whenever WORD and GUESS are the same.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     A number of spelling errors are due to the lack of cooperation
!     between the hands that do the typing and the brain that knows
!     how something should be spelled.  Four common errors are:
!
!        1) Leaving out a necessary character.
!        2) Adding an extra character.
!        3) Mistyping a single character.
!        4) Transposing two characters.
!
!     This routine creates "friendly" diagnostic messages indicating
!     whether or not the difference between WORD and GUESS could have
!     been caused by one of these simple errors.
!
!     This routine will typically be used only after the list of
!     guesses has been narrowed down to words that are "close" to
!     the unrecognized word.
!
! $ Examples
! $
!
!      WORD  :   LENGHT
!      GUESS :   LENGTH
!      CAUSE :  'It appears that you have transposed the fifth and
!                sixth letters of LENGTH (the letters T and H).'
!
!
!      WORD  :   EPHEMRIS
!      GUESS :   EPHEMERIS
!      CAUSE :  'It appears that you have left out the sixth letter of
!                EPHEMERIS. (The sixth letter should be E.)'
!
!      WORD  :   INTWGRATE
!      WORD  :   INTEGRATE
!      CAUSE :   'It appears that you have mistyped the fourth letter
!                 of INTEGRATE.  (The fourth letter should be E. You
!                 have W instead.)'
!
!      WORD :    INTERGER
!      GUESS:    INTEGER
!      CAUSE    'It appears that you have an extra letter at the fifth
!                letter of INTERGER. (The fifth letter R should be
!                removed.)'
!
!      WORD :    URUNAS
!      GUESS:    URANUS
!      CAUSE:   'I believe you meant URANUS. However, the actual
!                spelling error is not a simple one.'
!
!      WORD :    INTERDENOMINATIONAL
!      GUESS:    INTERDENOMINATIONAL
!      CAUSE:   ' '
!
! $ Restrictions
!
!      Any restrictions that apply to the words compared by MATCHE
!      apply as well to WORD and GUESS.
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
!     Version B1.0.0, 13-APR-1988 (WLT) (IMU)
!
!-&



!
!     Local variables
!
      INTEGER               LOC
      CHARACTER*12          DIAGNS

      CHARACTER*16          FIRST
      CHARACTER*16          LAST


      CALL MATCHE ( WORD, GUESS, DIAGNS, LOC )

      IF ( DIAGNS .EQ. 'IDENTITY' ) THEN

         CAUSE = ' '

      ELSE IF ( DIAGNS .EQ. 'TRANSPOSE' ) THEN

         CALL INTORD ( LOC,   FIRST )
         CALL INTORD ( LOC+1, LAST  )

         CALL LCASE  ( FIRST, FIRST )
         CALL LCASE  ( LAST,  LAST  )

         CAUSE = 'It appears that you have transposed the '

         CALL SUFFIX ( FIRST,              1, CAUSE )
         CALL SUFFIX ( 'and',              1, CAUSE )
         CALL SUFFIX ( LAST,               1, CAUSE )
         CALL SUFFIX ( 'letters of',       1, CAUSE )
         CALL SUFFIX ( GUESS,              1, CAUSE )
         CALL SUFFIX ( '(the letters',     1, CAUSE )
         CALL SUFFIX ( GUESS(LOC:LOC),     1, CAUSE )
         CALL SUFFIX ( 'and',              1, CAUSE )
         CALL SUFFIX ( GUESS(LOC+1:LOC+1), 1, CAUSE )
         CALL SUFFIX ( ').',               0, CAUSE )


      ELSE IF ( DIAGNS .EQ. 'INSERT' ) THEN

         CALL INTORD ( LOC,   FIRST )
         CALL LCASE  ( FIRST, FIRST )

         CAUSE = 'It appears that you have left out the '

         CALL SUFFIX ( FIRST,                1, CAUSE )
         CALL SUFFIX ( 'letter of ',         1, CAUSE )
         CALL SUFFIX ( GUESS,                1, CAUSE )
         CALL SUFFIX ( '. (The ',            0, CAUSE )
         CALL SUFFIX ( FIRST,                1, CAUSE )
         CALL SUFFIX ( 'letter should be ',  1, CAUSE )
         CALL SUFFIX ( GUESS(LOC:LOC),       1, CAUSE )
         CALL SUFFIX ( '.)',                 0, CAUSE )


      ELSE IF ( DIAGNS .EQ. 'REPLACE' ) THEN

         CALL INTORD ( LOC,   FIRST )
         CALL LCASE  ( FIRST, FIRST )

         CAUSE = 'It appears that you have mistyped the '

         CALL SUFFIX ( FIRST,                 1, CAUSE )
         CALL SUFFIX ( 'letter of ',          1, CAUSE )
         CALL SUFFIX ( GUESS,                 1, CAUSE )
         CALL SUFFIX ( '. (The ',             0, CAUSE )
         CALL SUFFIX ( FIRST,                 1, CAUSE )
         CALL SUFFIX ( 'letter should be ',   1, CAUSE )
         CALL SUFFIX ( GUESS(LOC:LOC),        1, CAUSE )
         CALL SUFFIX ( '. You have ',         0, CAUSE )
         CALL SUFFIX ( WORD(LOC:LOC),         1, CAUSE )
         CALL SUFFIX ( 'instead.)',           1, CAUSE )


      ELSE IF ( DIAGNS .EQ. 'REMOVE'  ) THEN

         CALL INTORD ( LOC,   FIRST )
         CALL LCASE  ( FIRST, FIRST )

         CAUSE = 'It appears that you have an extra letter at the '

         CALL SUFFIX ( FIRST,                   1, CAUSE )
         CALL SUFFIX ( 'letter of ',            1, CAUSE )
         CALL SUFFIX ( WORD,                    1, CAUSE )
         CALL SUFFIX ( '. (The ',               0, CAUSE )
         CALL SUFFIX ( FIRST,                   1, CAUSE )
         CALL SUFFIX ( 'letter ',               1, CAUSE )
         CALL SUFFIX ( WORD(LOC:LOC),           1, CAUSE )
         CALL SUFFIX ( 'should be removed.)',   1, CAUSE )


      ELSE

         CAUSE = 'I believe you meant '

         CALL SUFFIX ( GUESS,                              1, CAUSE )
         CALL SUFFIX ( '.  However, the actual spelling ', 1, CAUSE )
         CALL SUFFIX ( 'error is not a simple one.      ', 1, CAUSE )


      END IF


      RETURN
      END
