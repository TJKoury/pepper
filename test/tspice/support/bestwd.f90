
!$Procedure      BESTWD ( Perform a spell match against a set of words )

      SUBROUTINE BESTWD ( WORD, KNOWN, CUTOFF, BEST, SCORES, MSSG )
      IMPLICIT NONE

! $ Abstract
!
!     Given a word and a list of known words, return those of the list
!     closest to the word along with a diagnostic message.
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
!     PARSING
!     UTILITY
!     WORD
!
! $ Declarations

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         WORD
      CHARACTER*(*)         KNOWN  ( LBCELL : * )
      INTEGER               CUTOFF
      INTEGER               BEST   ( LBCELL : * )
      INTEGER               SCORES ( LBCELL : * )
      CHARACTER*(*)         MSSG

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     WORD       I   Word to compare against a list of known words.
!     KNOWN      I   List of known words.
!     CUTOFF     I   Fine tuning value.  A "good" value is 70.
!     BEST       O   Indices of the best matches.
!     SCORES     O   Scores of the best matches.
!     MSSG       O   Explanatory message.
!
! $ Detailed_Input
!
!     WORD       is any word.  Typically it will be a word that was not
!                equal to some "known" word and for which one wants to
!                find the "closest" known word. ONLY the first 32
!                characters of WORD are regarded as being significant.
!
!     KNOWN      is a cell containing "known" words.  These might be
!                keywords from a command, filenames, directories, etc.
!                From these a collection are found that most closely
!                match WORD.
!
!     CUTOFF     is an integer used to "fine tune" the matching
!                between WORD and the words in KNOWN.
!
!                CUTOFF should be between 0 and 100.  Values of
!                CUTOFF greater than 100 will effectively disable
!                the more detailed error diagnostics.  Values
!                less than 0 will simply cause the routine to work
!                harder with no gain in information.
!
!                A "good" value for CUTOFF is 70.  You will probably
!                want your input value to be close to this.
!
! $ Detailed_Output
!
!      BEST      BEST is a cell. On output BEST contains the indices of
!                the items in KNOWN that yielded the maximum comparison
!                score when compared to word. BEST will never contain
!                more than 10 indices. (It will rarely contain more
!                than two.)
!
!      SCORE     SCORE is a cell. SCORE is assumed to be declared the
!                same size as BEST.  On output SCORE(I) contains the
!                score that measures the similarity between between
!                KNOWN(BEST(I)) and WORD.
!
!                If WORD should happen to equal one of the KNOWN words
!                SCORE will be returned with a value of 1000.  Otherwise
!                it will be returned with a value between 0 and 100.
!                The higher the value of SCORE(I) the greater the
!                similarity between WORD and KNOWN(BEST(I)).
!
!                By comparing the values in SCORE with CUTOFF you can
!                determine how good a particular match is.
!                If SCORE is at least as big as CUTOFF the match is
!                regarded as a good one.  An attempt will have been
!                made at giving detailed diagnostics on the difference
!                between WORD and the best matching KNOWNs.
!
!      MSSG      is a message that identifies those KNOWN words that
!                best match WORD.  Moreover, if detailed diagnostics
!                are available, they will be reported in MSSG.
!
! $ Error_Handling
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!      This routine can be used to help a program recover from common
!      typing and spelling mistakes.  When a word is not recognized, it
!      is possible (perhaps likely) that a keystroke or two went awry in
!      the typing of the word.  If the list of legitimate words is
!      available, the unrecognized word can be compared with the
!      recognized words.  In this way, the one or ones that most nearly
!      resemble the unrecognized word can be identified.  From there the
!      program may point out the likely problem, offer to fix it, fix it
!      and continue (or any subset of these).  Thus to some extent the
!      program can do what you meant, not what you typed.
!
!      To measure the similarity between two words several techniques
!      are employed.  The first of these just compares the letter
!      sets of the two words.  For example the letter sets for the
!      words 'SIMILARITY' and 'SIMILITUDE' are
!
!         {A1, I1, I2, I3, L1, M1, R1, S1, T1, Y1 }
!
!      and
!
!         {E1, I1, I2, I3, L1, M1, S1, T1, U1 }
!
!      (Note that repeated letters are regardeds as distinct.)
!
!      By examining the symmetric difference between these two sets
!      one can get a measure of the how close the two words are.
!      The method used to compute the score will yield a value of
!      75 or higher for pairs of words whose letter sets have
!      a symmetric difference of 2 or fewer letters.
!
!      This does a good job of separating words such as
!      'CANADA' and 'ILLINOIS'.  However, it fails completely to
!      separate the words 'TRIANGLE', 'INTEGRAL', 'RELATING' and
!      'ALTERING'.  These four words all have the same letter sets.
!
!      Thus for words that compare well on the basis of letter sets
!      a second (more time consuming) comparison is made to see if
!      the words preserve the relative letter order.  In this step
!      each word is used to construct a sequence of templates
!      that are then matched against the other.  A tally of the
!      hits to misses is kept.  The roles of the two words are then
!      reversed and another tally computed.  The average of these
!      two scores is given to the word pair.
!      This is best illustrated with a simple example.
!
!      Consider the words ANGER and RANGE.
!
!      ANGER will be used to construct the 10 templates:
!
!         *A*N*, *A*G*, *A*E*, *A*R*, *N*G*,
!         *N*E*, *N*R*, *G*E*, *G*R*, *E*R*
!
!      Six of these match RANGE, namely
!
!         *A*N*, *A*G*, *A*E*, *N*G*, *N*E*, *G*E*, *E*R*
!
!      Next the 4 templates
!
!         *AN*,  *NG*, *GE*, *ER*
!
!      will be compared with RANGE,  The first three match.  Each
!      of these matches are "extra matches" that are added on to
!      the first 6 matches.  The score for ANGER to RANGE is
!
!        100 * MIN{1,(total matches / numer of templates of form *x*y*)}
!      = 100 * MIN{1, 9/10 }
!      = 90
!
!      The method extends in the obvious way to longer and shorter
!      words than ANGER and RANGE. As can be seen, this method of
!      comparing one word against another, requires not only the
!      correct letters to be present but they must also be in the
!      correct relative order.  Note that a perfect score of 100
!      does not mean the words are the same.  For example
!
!         AEAE and EAEA
!
!      yield an identical set of templates and hence have a matching
!      score of 100.  However, if both words have no letters repeated,
!      a score of 100 implies that the words are in fact the same.
!
!      If both methods of scoring exceed the value of CUTOFF, an
!      attempt is made to determine the exact difference between the
!      two words. The recognizable differences are: transposition of
!      two letters, a letter missing, a letter mistyped, an extra
!      letter. Thus CUTOFF allows the user to tune the circumstances
!      underwhich a attempts will be made to perform detailed
!      diagnosis of the the difference between a pair of words.
!
!      Empirically, it has been found that two words match up well if
!      both methods of scoring yield values of at least 70. This
!      is the recommended value for CUTOFF.
!
!      If both methods of scoring above yield values that exceed CUTOFF,
!      the two scores are averaged to give the score reported in SCORE.
!      If they do not both exceed CUTOFF but the average does, then
!      the score returned is CUTOFF-1.
!
!      CUTOFF can also be used as your means of determining how good
!      a match was.  Words with matching scores at least CUTOFF are
!      regarded as "good" matches, otherwise the match is viewed as
!      "poor."
!
! $ Examples
!
!      Suppose that
!
!      CUTOFF = 70
!      KNOWN  = 'ALTITUDE',      'CONTRACT',
!               'APPLE',         'INTEGRATE',
!               'LONGITUDE',     'EXTRACT',
!               'JUPITER',       'LATITUDE',
!               'EXPAND',        'SATURN',
!               'MERIDIAN',      'SHIFT',
!               'URANUS',        'ELEVATION',
!               'EPOCH',         'NEPTUNE',
!               'ASCENSION',     'DELTA',
!               'PLUTO',         'DECLINATION',
!               'COMPLEMENT'
!
!      If    WORD    = 'APPLY'   then     BEST(0)        = 1
!                                         KNOWN(BEST(1)) = 'APPLE'
!                                         SCORE(     1 ) = 89
!
!
!      If    WORD    = 'X'       then     BEST(0)        = 2
!                                         KNOWN(BEST(1)) = 'EXTRACT'
!                                         SCORE(     1 ) = 7
!
!                                         KNOWN(BEST(2)) = 'EXPAND'
!                                         SCORE(     2 ) = 8
!
!
!      If    WORD    = 'NEMPTUNE'  then   BEST(0)        = 1
!                                         KNOWN(BEST(1)) = 'NEPTUNE'
!                                         SCORE(     1 ) = 95
!
!
!      If    WORD    = 'ELATION'   then   BEST(0)        = 1
!                                         KNOWN(BEST(1)) = 'ELEVATION'
!                                         SCORE(     1 ) = 94
!
!
!      If    WORD    = 'QQQ'       then   BEST(0)        = 0
!
!
!      If    WORD    = 'COEMPLMENT' then  BEST(1)        = 1
!                                         KNOWN(BEST(1)) = 'COMPLEMENT'
!                                         SCORE(     1 ) = 100
!
!
! $ Restrictions
!
!     SCORES must be declared to be at least as large as BEST.
!
!     At most 10 best indices will be returned in BEST.
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
!     Version B1.0.0, 12-APR-1988 (WLT) (IMU)
!
!-&



!
!     SPICELIB functions
!
      INTEGER               CARDI
      INTEGER               CARDC
      INTEGER               SIZEI
      INTEGER               MATCHC
      INTEGER               MATCHO
      INTEGER               LSTLTI
      INTEGER               QRTRIM

      LOGICAL               EQSTR

!
!     Local variables
!
      INTEGER               BSIZE
      PARAMETER           ( BSIZE = 10 )

      INTEGER               BSCORE (BSIZE)
      INTEGER               ITEM   (BSIZE)
      INTEGER               HELP   (BSIZE)
      INTEGER               USIZE
      INTEGER               NBEST

      CHARACTER*80          NTH

      INTEGER               NKNOWN
      INTEGER               CSCORE
      INTEGER               OSCORE
      INTEGER               MAXSC

      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L

      INTEGER               LOC
      CHARACTER*16          TRANS

      LOGICAL               HIT
      INTEGER               HITS
      CHARACTER*32          CASE
      CHARACTER*(32)        MYWD

      INTEGER               TRIES
      INTEGER               LENGTH


!
!     First determine how many words we have to compare with
!     and the amount of room for reporting indices of "good"
!     matches.
!
      NKNOWN = CARDC( KNOWN )
      NBEST  = SIZEI( BEST  )


!
!     This routine only works on words of 32 or fewer characters
!
      MYWD   = ' '
      MYWD   = WORD


!
!     USIZE refers to the amount of space we will actually
!     use in the buffers that store the best MATCHC scores and
!     the associated KNOWN word.
!
      USIZE  = MIN  ( BSIZE, NKNOWN, NBEST )

      DO I = 1, USIZE
         BSCORE(I) = 0
         ITEM  (I) = 0
         HELP  (I) = 0
         SCORES(I) = 0
      END DO

!
!     First apply MATCHC against each of the KNOWNs and keep the
!     top USIZE words that match.
!

      DO I = 1, NKNOWN



!
!        Just in case, see if we have an exact match.
!
         IF ( EQSTR (MYWD,KNOWN(I)) ) THEN

            CALL SCARDI ( 1, BEST   )
            CALL SCARDI ( 1, SCORES )

            BEST  (1) = I
            SCORES(1) = 1000

            CALL INTORD ( I,   NTH )
            CALL LCASE  ( NTH, NTH )

            MSSG = MYWD

            CALL SUFFIX ( 'is equal to the ',        1,  MSSG )
            CALL SUFFIX ( NTH,                       1,  MSSG )
            CALL SUFFIX ( ' known word.',            1,  MSSG )

            RETURN

         END IF

         CSCORE  = MATCHC ( MYWD,    KNOWN(I)        )
         J       = LSTLTI ( CSCORE , USIZE,   BSCORE )

         DO K = 1, J-1
            BSCORE(K) = BSCORE(K+1)
            ITEM  (K) = ITEM  (K+1)
         END DO

         IF ( J .GT. 0 ) THEN
            BSCORE(J) = CSCORE
            ITEM  (J) = I
         END IF

      END DO

!
!     Now for the top USIZE matches, perform a MATCHO comparison.
!     If we get a match of CUTOFF or higher.  Run MATCHE against it
!     to see if we can guess at what went wrong.
!
!     So far our best score is 0 and we haven't HIT any good matches.
!
      MAXSC = 0
      HITS  = 0

      DO I = 1, USIZE

!
!        Only examine items that have legitimate indices.
!
         IF ( ITEM(I) .NE. 0 ) THEN

            BSCORE(I) = MATCHO(MYWD,      KNOWN(ITEM(I)) )
            CSCORE    = MATCHC(MYWD,      KNOWN(ITEM(I)) )
            MAXSC     = MAX   (BSCORE(I), MAXSC          )

            IF (       ( BSCORE(I) .GE. CUTOFF ) &
                 .AND. ( CSCORE    .GE. CUTOFF ) ) THEN

!
!              We've HIT a good match.
!
               HITS    = HITS + 1

!
!              See if the problem with this word can be diagnosed
!              with MATCHE.
!
               CALL MATCHE ( MYWD, KNOWN(ITEM(I)), TRANS, LOC )

!
!              If a diagnosis can be performed on this item, we
!              say that HELP is available at level 2.  Otherwise
!              since we have a good match anyway we say HELP is
!              available at level 1.
!

               IF ( TRANS .NE. 'NONE' ) THEN
                  HELP(I) = 2
               ELSE
                  HELP(I) = 1
               END IF

            END IF

         END IF

      END DO

!
!     If none of the words had a sufficiently high score, just
!     report those that had the maximum score.
!
      IF ( HITS .EQ. 0 ) THEN
!
!        Just report the item(s) that had the biggest score.
!
!        First see how many had the maximum score.
!
         DO I = 1, USIZE
            IF ( ITEM(I) .NE. 0 ) THEN

               IF ( BSCORE(I) .EQ. MAXSC ) THEN
                  HITS = HITS + 1
               END IF

            END IF
         END DO

!
!        If there were no KNOWN words that had letters in common
!        with MYWD, all of the elements of the array ITEM will be
!        zero and we will not have made any HITS against MAXSC.
!        There is nothing at all we can do in this case.
!
         IF ( HITS .EQ. 0 ) THEN

            MSSG = 'The word'

            CALL SUFFIX ( MYWD,                                1, MSSG )
            CALL SUFFIX ( 'has nothing in common with any of', 1, MSSG )
            CALL SUFFIX ( 'the words I can recognize.  If ',   1, MSSG )
            CALL SUFFIX ( 'this word was typed interactively,',1, MSSG )
            CALL SUFFIX ( 'you may want to see if your ',      1, MSSG )
            CALL SUFFIX ( 'fingers are over the correct keys.',1, MSSG )

            CALL SCARDI ( 0 , BEST   )
            CALL SCARDI ( 0 , SCORES )

            RETURN

         END IF


!
!        Still here.  Then we have at least some item that has
!        something in common with MYWD.  Set up a closing string so
!        that grammar will be correct.
!
         IF ( HITS .GT. 1 ) THEN
            CASE = 'my closest matches are: '
         ELSE
            CASE = 'my closest match is: '
         END IF


         MSSG = 'The word '''

         CALL SUFFIX ( MYWD,                                   1, MSSG )
         CALL SUFFIX ( ''' did not match up well with any of', 1, MSSG )
         CALL SUFFIX ( 'the words I was comparing against.',   1, MSSG )
         CALL SUFFIX ( 'However,',                             1, MSSG )
         CALL SUFFIX ( CASE,                                   1, MSSG )

!
!        Now append the list of KNOWN words that matched MYWD with
!        the highest score.
!
         HIT  = .FALSE.
         J    = 0

         DO I = 1, USIZE

            IF ( ITEM(I) .EQ. 0 ) THEN

!
!              don't do anything
!

            ELSE IF ( BSCORE(I) .EQ. MAXSC ) THEN

               J       = J + 1

               BEST(J) = ITEM(I)
               L       = QRTRIM( KNOWN(ITEM(I)) )

               IF ( HIT ) THEN

                  CALL SUFFIX ( ',  ''', 0, MSSG )
                  CALL SUFFIX ( KNOWN(ITEM(I))(1:L), 0, MSSG)
                  CALL SUFFIX ( '''', 0, MSSG )

               ELSE

                  HIT = .TRUE.

                  CALL SUFFIX ( '''',                1, MSSG )
                  CALL SUFFIX ( KNOWN(ITEM(I))(1:L), 0, MSSG )
                  CALL SUFFIX ( '''',                0, MSSG )

               END IF

               CALL SUFFIX ( '.', 0, MSSG )

            END IF

         END DO

!
!        Set the cardinality of the window of BEST indices.
!
         CALL SCARDI ( J, BEST )

      ELSE IF ( HITS .EQ. 1 ) THEN

!
!        There was just one KNOWN word for which there was a good
!        match.  Call MSPELD to produce a diagnosis of the problem
!        and record the index of the item.
!
         I = 1

         DO WHILE ( HELP(I) .EQ. 0 )
            I = I + 1
         END DO

         CALL MSPELD ( MYWD, KNOWN(ITEM(I)), MSSG )

         BEST(1) = ITEM(I)
         CALL SCARDI ( 1,    BEST                 )

      ELSE

!
!        There were at least two "good" words.  If any of them
!        could be diagnosed, then report them.  Otherwise
!        report only those that had a maximum MATCHO score.
!
         TRIES = 0

         DO I = 1, 5

            IF ( HELP(I) .EQ. 2 ) THEN
               TRIES = TRIES + 1
            END IF

         END DO

         IF ( TRIES .EQ. 0 ) THEN

!
!           None of the KNOWN words had diagnostics available.
!
            MSSG =  'Although a the spelling '                        // &
                    'error can''t be described in a simple '          // &
                    'way,  I have found the following '               // &
                    'words that may be what you were trying to '      // &
                    'say.  '
            J = 0

            DO I = 1, USIZE

               IF ( HELP(I) .NE. 0 ) THEN

                  CALL SUFFIX( '''',           2, MSSG )
                  CALL SUFFIX( KNOWN(ITEM(I)), 0, MSSG )
                  CALL SUFFIX( ''',',          0, MSSG )

                  J       = J + 1
                  BEST(J) = ITEM(I)

               END IF

            END DO

            CALL SCARDI ( J, BEST  )

            MSSG(QRTRIM(MSSG):QRTRIM(MSSG)) = ' '

         ELSE IF ( TRIES .EQ. 1 ) THEN

!
!            Only one of the KNOWN words had diagnostics available.
!
             DO I = 1, 5

                IF ( HELP(I) .EQ. 2 ) THEN
                   CALL MSPELD ( MYWD, KNOWN(ITEM(I)), MSSG)
                   BEST(1) = ITEM(I)
                END IF
             END DO

             CALL SCARDI( 1, BEST )

         ELSE

!
!           At least two of the KNOWN words had diagnostics available.
!           Report all of them.
!
            MSSG =  'The following '                                  // &
                    'common spelling mistakes may be the reason I '   // &
                    'did not recognize '


            CALL SUFFIX ( MYWD,                                1, MSSG )
            CALL SUFFIX ('.',                                  1, MSSG )

            LENGTH = LEN ( MSSG )
            J      = 0

            DO I = 1, USIZE

               IF ( HELP(I) .EQ. 2) THEN
                  IF ( QRTRIM(MSSG) .LT. LENGTH - 3 ) THEN

                     CALL MSPELD ( MYWD, KNOWN(ITEM(I)), &
                                   MSSG(QRTRIM(MSSG)+3:)      )

                     J       = J + 1
                     BEST(J) = ITEM(I)

                  END IF
               END IF

            END DO

            CALL SCARDI ( J, BEST )

         END IF

      END IF



!
!     As for the scores, we will report the average of the MATCHO and
!     MATCHC scores for the best matches.
!
      DO I = 1, CARDI(BEST)

         OSCORE    = MATCHO ( MYWD, KNOWN(BEST(I)) )
         CSCORE    = MATCHC ( MYWD, KNOWN(BEST(I)) )

         SCORES(I) = ( OSCORE + CSCORE ) / 2

         IF (      ( OSCORE .LT. CUTOFF ) &
             .OR.  ( CSCORE .LT. CUTOFF ) ) THEN

            SCORES (I) = MIN ( SCORES(I), CUTOFF - 1 )

         END IF

      END DO

      CALL SCARDI ( CARDI(BEST), SCORES )

      RETURN
      END
