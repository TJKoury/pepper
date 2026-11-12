!$Procedure      M2CORE ( META/2 core syntax checking routines. )

      SUBROUTINE M2CORE ( TEMP,   TBEG,   KEYWDS, &
                          STRING, SBEG,   REASON, CUTOFF, &
                          M2CODE, SCORE,  CAUSE,  SEND    )
      use pepper_legacy, pepper_legacy_countc => countc
      IMPLICIT NONE


! $ Abstract
!
!     This routine is the header routine for use by M2MTCH and its
!     associated entry point M2RCVR.  As it takes no action, it should
!     not be called directly.
!
! $ Required_Reading
!
!      The META/2 book.
!
! $ Keywords
!
!     PARSING
!     UTILITY
!     WORD
!
! $ Declarations


      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         TEMP
      INTEGER               TBEG
      CHARACTER*(*)         KEYWDS ( LBCELL: * )
      CHARACTER*(*)         STRING
      INTEGER               SBEG
      LOGICAL               REASON
      INTEGER               CUTOFF
      INTEGER               M2CODE
      INTEGER               SCORE
      CHARACTER*(*)         CAUSE ( 2 )
      INTEGER               SEND

! $ Brief_I/O
!
!     See the entry point headers for description of each of the
!     input/output arguements.
!
! $ Detailed_Input
!
!     See individual entry points.
!
! $ Detailed_Output
!
!     See individual entry points.
!
! $ Error_Handling
!
!     See individual entry points.
!
! $ Input_Files
!
!     None.
!
! $ Output_Files
!
!     None.
!
! $ Particulars
!
!     This routine serves as the header routine for entry point M2MTCH
!     and its associated entry M2RCVR.  M2MTCH is the essential syntax
!     checking portion of the META/2 syntax comparison routine.
!
! $ Examples
!
!     To compare two templates call M2MTCH
!
!     To find the position of a mispelled keyword in the input string
!     and the possible spelling corrections call M2RCVR
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
!     Version B1.0.0, 7-APR-1988 (WLT) (IMU)
!
!-&
!



!
!     SPICELIB functions
!
      LOGICAL               M2WMCH
      LOGICAL               M2KEYW
      INTEGER               QLSTNB
      INTEGER               QRTRIM



      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 120 )

!
!     Local variables
!
      INTEGER               TBEGIN
      INTEGER               SBEGIN
      INTEGER               BEGOUT
      INTEGER               SLEN

      CHARACTER*(32)        ROOT
      CHARACTER*(LNSIZE)    PHRASE
      CHARACTER*(2)         ARTCLE

      LOGICAL               ERROR
      LOGICAL               CALWRD
      LOGICAL               KEYTBE

      INTEGER               MSPELL

      INTEGER               TE
      INTEGER               TB
      INTEGER               TC

      INTEGER               ORIGNL

      INTEGER               LOWER
      INTEGER               UPPER

      LOGICAL               USEEND
      LOGICAL               USELST
      LOGICAL               USEKEY
      LOGICAL               ENDOK

      INTEGER               ENDCHK
      INTEGER               KB
      INTEGER               KE

      INTEGER               NKEY

      LOGICAL               ENDIT
      INTEGER               MCOUNT
      INTEGER               COUNT
      INTEGER               DCOUNT

      INTEGER               SB
      INTEGER               SE
      INTEGER               DB
      INTEGER               DE
      INTEGER               OVERSB
      INTEGER               LASTSB
      INTEGER               OVERSE
      INTEGER               LASTSE
      INTEGER               TIMEB
      INTEGER               SUFFSB
      INTEGER               SUFFSE

      INTEGER               TCODE

      LOGICAL               KEYWRD
      LOGICAL               CMATCH

      CHARACTER*64          LOWERC
      CHARACTER*64          UPPERC
      CHARACTER*64          COUNTC

      INTEGER               MAXBST
      PARAMETER           ( MAXBST = 10 )

      INTEGER               SCORES ( LBCELL: MAXBST)
      INTEGER               BEST   ( LBCELL: MAXBST)

      INTEGER               BSCORE


!
!     KNOWN, PBEG, and PEND are storage for the recovery entry point.
!
!     Should a spelling error be detected, the best matching words will
!     be stored in KNOWN and the index of the beginning and ending
!     of the problem word in STRING will be stored in PBEG and PEND
!     respectively.
!
      CHARACTER*32          KNOWN  ( LBCELL: MAXBST)

      INTEGER               PBEG

      INTEGER               PEND

      CHARACTER*420         MSSG

      INTEGER               METASC
      PARAMETER           ( METASC = 15 )

      INTEGER               KEYSC
      PARAMETER           ( KEYSC  = 100)

      LOGICAL               PASS1

      INTEGER               I

      SAVE

!
!     Initial values
!
      DATA                  PASS1   / .TRUE.    /

      RETURN

!$Procedure  M2MTCH ( Match a string with a simple META/2 template )

      ENTRY  M2MTCH ( TEMP,   TBEG,  KEYWDS, &
                      STRING, SBEG,  REASON, CUTOFF, &
                      M2CODE, SCORE, CAUSE    )

! $ Abstract
!
!     This entry points compares simple templates with strings and
!     produces scores reflecting the extent of agreement between
!     the template and string.  If requested diagnostics are produced.
!
! $ Required_Reading
!
!     The META/2 book.
!
! $ Keywords
!
!     PARSING
!     UTILITY
!     WORD
!
! $ Declarations
!
!     INTEGER               LBCELL
!     PARAMETER           ( LBCELL = -5 )
!
!     CHARACTER*(*)         TEMP
!     INTEGER               TBEG
!     CHARACTER*(*)         KEYWDS ( LBCELL: * )
!     CHARACTER*(*)         STRING
!     INTEGER               SBEG
!     LOGICAL               REASON
!     INTEGER               CUTOFF
!     INTEGER               M2CODE
!     INTEGER               SCORE
!     CHARACTER*(*)         CAUSE  ( 2 )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TEMP       I   A simple language specification sentence .
!     TBEG       I   Position in the template to start match attempt.
!     KEYWDS     I   A cell of possible terminators of a META-KEY string
!     STRING     I   A potential language sentence
!     SBEG      I/O  Marker of the current start of the string
!     REASON     I   Set to .TRUE. to request production of diagnostics
!     CUTOFF     I   Spelling error threshold
!     M2CODE     O   Indicates type of mismatch between TEMP and  STRING
!     SCORE      O   Number giving a measure of how closeness of match.
!     CAUSE      O   If requested, a diagnostic of mismatch.
!
! $ Detailed_Input
!
!     TEMP       A simple language specification sentence.  Such a
!                statement consists of only keyword and META/2 class
!                specifiers.  No groups or @then directives are allowed.
!
!     TBEG       Position in the template to start match attempt.
!
!     KEYWDS     A cell of possible terminators of a META-KEY string.
!                This is primarily usefull for higher level routines
!                that pick simple templates out of more complex META/2
!                specification sentences.
!
!     STRING     A is a collection of words that might make up a valid
!                sentence in a META/2 language.  A substring of STRING
!                beginning with SBEG will be matched against TEMP to
!                see if we have a valid phrase in a META/2 language.
!
!     SBEG       Marker of the current start of the string
!
!     REASON     Set to .TRUE. to request production of diagnostics
!
!     CUTOFF     is a parameter used to determine how close words
!                of STRING must match up with keywords in TEMP
!                in order to be diagnosed as spelling errors.
!                Ranges from 0 to 100 are acceptable.  A "good" range
!                of values is from from 65 to 75.
!
! $ Detailed_Output
!
!     SBEG       if the match is successful, SBEG will be set to the
!                first word of the input string that follows the
!                matched substring. ( Note that words in KEYWDS do
!                not qualify as part of the template, but merely
!                serve to delimit the ends of variable length
!                templates. Thus if one of these words was actually
!                used to delimit the end of TEMP, SBEG will point to
!                the beginning of that word in STRING.)
!
!
!     M2CODE     Indicates type of mismatch between TEMP and  STRING
!
!     M2CODE = 0     Indicates that the template supplied matched the
!                    input string as far as it went.
!
!     M2CODE = 10    Indicates that the keyword that was supposed to
!                    terminate a variable length template was probably
!                    mispelled.
!
!     M2CODE = 11    We were expecting a specific keyword and failed
!                    in our match attempt.  It is likely that the
!                    keyword was simply misspelled.
!
!     M2CODE = 101   Indicates that a variable length template had too
!                    few entries before the keyword was encountered.
!
!     M2CODE = 102   Indicates that a variable length template had too
!                    many entries before the keyword was encountered.
!
!     M2CODE = 103   Indicates that the correct number of entries
!                    for a variable length template were encountered
!                    but the input string terminated without finding
!                    the correct keyword.
!
!     M2CODE = 104   Indicates that the string should have terminated
!                    but instead contained extra characters.
!
!     M2CODE = 105   Indicates that correct number if items were
!                    present in the variable length template but that
!                    it did not end with an expected keyword.
!                    Moreover, it is not thought that the problem is
!                    likely to be a simple spelling error.
!
!     M2CODE = 106   The number of items found in a variable length
!                    template was too small and we did not get
!                    an expected keyword.  A possible explanation
!                    is a mistyping one or more of the letters in
!                    one of the META class words.
!
!     M2CODE = 107   The number of items found in a variable length
!                    template was too large and we did not get
!                    an expected keyword.  A possible explanation
!                    is a a forgotten keyword.
!
!     M2CODE = 108   We ran out of string while in a fixed length
!                    template.
!
!     M2CODE = 109   We failed to match a META class word while within
!                    a fixed length template.
!
!     M2CODE = 110   We were expecting to see a specific keyword and
!                    got something else.  This is not thought to be
!                    the result of a spelling error.
!
!     M2CODE = 111   We were expecting to see a META class word and
!                    failed in our matching attempt.
!
!     M2CODE values from 1001 to 1014 indicate problems that can occur
!     when attempting to match a substring with the @calendar specifier.
!
!     M2CODE = 1001  Too many tokens in a @calendar string.
!
!     M2CODE = 1002  Time indicated is JD but no numeric portion
!                    supplied.
!
!     M2CODE = 1003  The date portion of the Julian date didn't make
!                    it through the number parsing.
!
!     M2CODE = 1004  No date was supplied
!
!     M2CODE = 1005  A year was not supplied in a calendar date
!
!     M2CODE = 1006  Ambiguous date specification.
!
!     M2CODE = 1007  Ambiguous month specification
!
!     M2CODE = 1008  Invalid day specification in a calendar date.
!
!     M2CODE = 1009  Year appears as the second item without a
!                    spelled month.
!
!     M2CODE = 1010  Month is not spelled and is not an integer
!                    between 1 and 12.
!
!     M2CODE = 1011  Month not spelled and day is not between 1 and
!                    366.
!
!     M2CODE = 1012  Hour portion of time is not an integer from 0
!                    to 23.
!
!     M2CODE = 1013  Minutes portio of time is not an integer from
!                    0 to 59.
!
!     M2CODE = 1014  Seconds must be a positive number less than 61
!
!     SCORE      Number giving a measure of how closeness of match, 100
!                points are awarded for matched keywords, 15 points
!                for matched classes, 100 points for matched calendar
!                strings.  Fractions of 100 awarded for words that
!                look like they might be misspelled keyword.  The
!                score is used primarily in thos cases when a substring
!                does not match any of a collection of templates
!                exactly.  In this case the one that has the highest
!                score is regarded as being what the user probably
!                meant.
!
!     CAUSE      If requested, a diagnostic of mismatch.
!
! $ Exceptions
!
!     The following errors are detected by this routine.
!
!           'SPICE(KEYWORDNOTFOUND)'
!
!     Additional errors may be detected by SPICELIB routines called
!     by this routine.
!
!
! $ Input_Files
!
!     None.
!
! $ Output_Files
!
!     None.
!
! $ Particulars
!
!     This routine is the central utility used in META/2 when attempting
!     to match potential sentences with language templates.  It compares
!     simple templates with substrings of a command and produces a score
!     indicating the degree of match.  Moreover, if requested,
!     diagnostics are available that indicate why a string did not
!     match a given template.
!
! $ Examples
!
!      None.
!
! $ Restrictions
!
!      It is assumed that all templates are simple META/2 templates.
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
!     Version B1.0.0, 7-APR-1988 (WLT) (IMU)
!
!-&
!


!
!     Take care of the SPICE error handling first.
!
      IF ( PASS1 ) THEN

         PASS1  = .FALSE.

         CALL SSIZEI ( MAXBST, BEST   )
         CALL SSIZEI ( MAXBST, SCORES )
         CALL SSIZEC ( MAXBST, KNOWN  )
         CALL SCARDC ( 1,      KNOWN  )

      END IF

      SLEN   = LEN ( STRING )

      CAUSE(1)  = ' '
      SCORE     = 0

      TBEGIN =  TBEG
      SBEGIN =  SBEG

      ERROR  = .FALSE.

      M2CODE = 0
      MSPELL = 0

      KB     = 0
      KE     = 0

      NKEY   = CARDC ( KEYWDS )

!
!     Locate the next word of the template.
!
      CALL FNDNWD ( TEMP, TBEGIN, TB,   TE )


      DO WHILE (       (  TB .NE.  0     ) &
                 .AND. (     .NOT. ERROR ) )

!
!        Zero out the keyword pointers.
!
         KB    =  0
         KE    =  0
         ENDOK = .FALSE.

!
!        Examine the current template word.  Is there a range template
!        attatched?
!
         ORIGNL = TB

         CALL M2BEGR ( TEMP, TB,   TE, LOWER, UPPER )

!
!        Locate the boundaries of the root of this template word.
!
         CALL M2TRIM ( TEMP(TB:TE),           ROOT  )

         TC = QRTRIM(ROOT) - 1 + TB



!
!        If TB changed from its original value there is a range template
!        attached to the word TEMP(TB:TE).  The associated values are in
!        LOWER and UPPER.
!
         KEYTBE = M2KEYW   (  TEMP(TB:TE) )
         CALWRD =             TEMP(TB:TC)  .EQ. '@calendar' &
                  .AND. .NOT. KEYTBE

         IF (      ( ORIGNL .LT. TB ) &
              .OR. ( CALWRD         ) ) THEN

!
!           Yes.   There is a range template attatched.  Is it of
!           variable length?
!
            IF ( CALWRD ) THEN
               LOWER  = 1
               UPPER  = 40
               TIMEB  = SBEGIN
            END IF

            IF ( LOWER .NE. UPPER ) THEN

!
!              Yes.  The template has a variable length. Determine
!              what delimiters might signal the end of a matching
!              substring of word from string.
!
!              Possibilities are:  The end of the string     (USEEND)
!                                  One of the listed KEYWDS  (USELST)
!                                  A keyword listed in TEMP. (USEKEY)
!
!              Right now we don't know which of the three cases to use.
!

               USEEND = .FALSE.
               USELST = .FALSE.
               USEKEY = .FALSE.
               ENDOK  = .FALSE.

               ENDCHK = TE + 1

!
!              If the end of the current template word, was not
!              at the end of the template, then there might be
!              a keyword next.  Look for the next word to find out.
!
               CALL FNDNWD ( TEMP, ENDCHK, KB, KE )

               IF ( KE .GT. 0 ) THEN

!
!                 There is a word in the template that follows
!                 our current template word.  See if it is a keyword.
!
                  IF ( M2KEYW( TEMP(KB:KE) )  )THEN

!
!                    If it is a keyword, it will be used as the
!                    delimiter for a sequence of words in STRING.
!                    ( Note we only want to work with the root of this
!                      template word. )
!
                     USEKEY = .TRUE.
                     CALL M2TRIM ( TEMP(KB:KE), ROOT )
                     KE = QRTRIM ( ROOT ) + KB - 1

                  ELSE

!
!                    Its not a keyword.  Bad, Bad. The user was not
!                    using META/2 properly.
!

                     CALL CHKIN  ( 'M2MTCH' )
                     CALL SETMSG ( 'M2MTCH: Any META-KEY that '       // &
                                   'is preceded by a variable length' // &
                                   ' range template '                 // &
                                   'in a specification statement '    // &
                                   'must be followed by a keyword. ' )

                     CALL SIGERR ( 'SPICE(KEYWORDNOTFOUND)' )
                     CALL CHKOUT ( 'M2MTCH' )

                     RETURN

                  END IF

               ELSE IF ( KE .LE. 0 ) THEN

!
!                 We got to this point because there was nothing
!                 to look at beyond where we were in TEMP.  So we
!                 either use one of the listed keywords or the end
!                 of the string will be our delimiter.
!
                  IF ( NKEY .GT. 0 ) THEN
                     USELST = .TRUE.
                     ENDOK  = ESRCHC ( '@end', NKEY, KEYWDS(1) ) .NE. 0
                  ELSE
                     USEEND = .TRUE.
                  END IF

               END IF

!
!              Until we have detected one of the keywords
!                 or we have not matched the current class
!                 or we run out of words in the sentence
!
!                   Grab the next word of the sentence
!                   Check it for keyword .
!                   Check it for class .
!
               ENDIT  = .FALSE.
               KEYWRD = .FALSE.

               MCOUNT =  0
               SUFFSB =  0
               OVERSB =  0
               OVERSE =  0
               LASTSB =  SBEGIN
               LASTSE =  POS ( STRING, ' ', SBEGIN ) - 1

               DO WHILE ( .NOT. ENDIT )

!
!                 Fetch the next word of the sentence.
!
                  CALL FNDNWD ( STRING, SBEGIN, SB, SE )

!
!                 If there WAS a next word SE will not be zero.
!
                  IF ( SE .EQ. 0 ) THEN

                     KEYWRD = USEEND .OR. ENDOK
                     ENDIT  = .TRUE.

!
!                    BEGOUT will point past the matched portion of the
!                    string.  If no errors occur, it will be used to
!                    set SBEG on output.
!
                     BEGOUT = SLEN + 1

                  ELSE

!
!                    is this a delimiting word for a variable length
!                    list?
!
                     IF ( USELST ) THEN

                        KEYWRD = ESRCHC ( STRING(SB:SE), &
                                          NKEY, &
                                          KEYWDS(1)     ) &
                                 .NE. 0

                        ENDIT  = KEYWRD

                        IF ( KEYWRD ) THEN

!
!                          Mark the position just before the beginning
!                          of this word in STRING  so that SBEG will
!                          point to the first word past the end of
!                          the matched portion of STRING.
!
                           BEGOUT = SB - 1
                        END IF

                     ELSE IF ( USEKEY ) THEN

                        KEYWRD =        M2WMCH ( STRING, SB, SE, &
                                                 TEMP(KB:KE)    ) &
                                 .AND.  M2KEYW ( TEMP(KB:KE)    )
                        ENDIT  = KEYWRD

!
!                       Mark the position of the "next" character
!                       in the string beyond the end of the current
!                       STRING word.
!
                        BEGOUT = SE + 1

                     END IF

!
!                    If we didn't bump into a keyword this must
!                    be (or should be) another of the words specified
!                    by the META-KEY TEMP(TB:TE)
!
                     IF ( .NOT. KEYWRD ) THEN

                        CMATCH = M2WMCH ( STRING, SB, SE, TEMP(TB:TE) )

                        IF ( CMATCH ) THEN

                           MCOUNT = MCOUNT + 1

!
!                          Mark the position of the first character
!                          beyond the end of the current STRING
!                          word.
!
                           BEGOUT = SE     + 1

!
!                          If MCOUNT has gotten too big, record the
!                          begin and end of the "bad" portion of the
!                          substring.
!
                           IF      ( MCOUNT .EQ. LOWER + 1 ) THEN

!
!                             Mark the location of the beginning
!                             and end of this word in case we need to
!                             backtrack to here.
!
                              SUFFSB = SB
                              SUFFSE = SE

                           ELSE IF ( MCOUNT .LE. UPPER     ) THEN

!
!                             Mark the end of this word in case
!                             we need it later.
!
                              SUFFSE = SE

                           ELSE IF ( MCOUNT .EQ. UPPER + 1 ) THEN

                              OVERSB = SB
                              OVERSE = SE

                           ELSE IF ( MCOUNT .GT. UPPER ) THEN

                              OVERSE = SE

                           END IF

                        ELSE

                           ENDIT  = .TRUE.

                        END IF

                     END IF

                     LASTSB = SB
                     LASTSE = SE

                  END IF

!
!                 Set the pointer to the input string to the first
!                 character past the end of the current word.
!
                  SBEGIN = SE     + 1

               END DO


!
!              We're now at the end of the loop matching words of STRING
!              with the class of object that had a variable length
!              template.
!
!              The question now is: 'Did we get out of the loop in
!              a healthy or unhealthy way?'
!
!
!              Did we have the required range of items in the class?
!              Did we hit  the keyword?
!
!              If both questions were answered YES,
!
               IF (        KEYWRD &
                    .AND. (MCOUNT .GE. LOWER ) &
                    .AND. (MCOUNT .LE. UPPER ) ) THEN

!
!                 Increment the score by METASC times the number of
!                 words found in the variable length template plus
!                 KEYSC for getting the keyword right.
!

                  IF ( .NOT. CALWRD ) THEN

                     SCORE = SCORE + METASC*MCOUNT  + KEYSC

                     IF ( USEKEY ) THEN

!
!                       set the end of the last template word used to
!                       be the end of the keyword that we just hit.
!
                        TE = KE

                     END IF

                  ELSE

                     MSSG = ' '
                     CALL M2CAL ( STRING(TIMEB:SUFFSE), MSSG, TCODE )

                     IF ( TCODE .EQ. 0 ) THEN

                        SCORE = SCORE + 2*KEYSC

                     ELSE

                        SCORE = SCORE + KEYSC
                        ERROR = .TRUE.

                        IF ( M2CODE .EQ. 0 ) THEN
                           M2CODE = 1000 + TCODE
                        END IF

                        IF ( REASON ) THEN

                           CAUSE(1) = 'I was not able to parse the ' // &
                                      'calendar string "'

                           CALL SUFFIX (STRING(TIMEB:SUFFSE), 0, CAUSE(1))
                           CALL SUFFIX ('". ',                0, CAUSE(1))
                           CALL SUFFIX (MSSG,                 1, CAUSE(1))

                           CALL M2MARK ( STRING, TIMEB, SUFFSE,  CAUSE)

                           CAUSE(2) = CAUSE(1)

                        END IF

                     END IF

                  END IF

!
!              If less than the required range but a keyword was found
!              the error was: " not enough values loaded. "
!
               ELSE IF ( KEYWRD .AND. MCOUNT .LT. LOWER ) THEN

                  CALL INTTXT ( LOWER,  LOWERC )
                  CALL INTTXT ( MCOUNT, COUNTC )

                  CALL LCASE  ( LOWERC, LOWERC )
                  CALL LCASE  ( COUNTC, COUNTC )

                  ERROR  = .TRUE.

                  IF ( M2CODE .EQ. 0 ) THEN
                     M2CODE = 101
                  END IF

!
!                 We grant METASC points for every word of the current
!                 class that was found, but we subtract METASC points
!                 for each item we were short.  That is:
!
!                 MCOUNT + ( LOWER - MCOUNT ) = 2*MCOUNT - LOWER
!
                  SCORE  = SCORE + METASC * MAX ( 0, 2*MCOUNT - LOWER )

!
!                 Add on KEYSC points for getting the correct keyword.
!
                  SCORE = SCORE + KEYSC

                  IF ( REASON ) THEN

                     CAUSE(1) = 'I was expecting to see at least ' &
                     //         '# # at this point in the command ' &
                     //         'string. ' &
                     //         'I counted #. '

                     CALL M2CLSS ( TEMP(TB:TC), LOWER,   PHRASE )
                     CALL REPMC  ( CAUSE(1), '#',  LOWERC,  CAUSE(1) )
                     CALL REPMC  ( CAUSE(1), '#',  PHRASE,  CAUSE(1) )
                     CALL REPMC  ( CAUSE(1), '#',  COUNTC,  CAUSE(1) )

!
!                    OK. now we want to tack on the string and keep
!                    track of where the current word STRING(SB:SE)
!                    will get put.
!
                     CALL M2MARK ( STRING, LASTSB, LASTSE, CAUSE )
                     CAUSE(2) = CAUSE(1)

                  END IF


!
!              If more than the required range but a keyword was found
!              the error was too many values loaded.
!
               ELSE IF ( KEYWRD .AND. MCOUNT .GT. UPPER ) THEN

                  CALL INTTXT ( UPPER,  UPPERC )
                  CALL INTTXT ( MCOUNT, COUNTC )

                  CALL LCASE  ( UPPERC, UPPERC )
                  CALL LCASE  ( COUNTC, COUNTC )

                  ERROR  = .TRUE.

                  IF ( M2CODE .EQ. 0 ) THEN
                     M2CODE = 102
                  END IF

!
!                 We grant METASC points for every word of the current
!                 class that was found prior to the cutoff limit.
!                 But we subtract METASC points for each extra item.
!                 That is:
!
!                 UPPER + ( MCOUNT - UPPER ) = 2*UPPER - MCOUNT
!
                  SCORE  = SCORE + METASC * MAX ( 0, 2*UPPER - MCOUNT )

!
!                 Add on KEYSC points for getting the correct keyword.
!
                  SCORE = SCORE + KEYSC

                  IF ( REASON ) THEN

                     CAUSE(1) = 'I was expecting to see at most ' &
                     //         '# #. I counted #. ' &
                     //         'I''ve marked the location of ' &
                     //         'the problem for you. '

                     CALL M2CLSS ( TEMP(TB:TC), UPPER,   PHRASE )
                     CALL REPMC  ( CAUSE(1), '#',  UPPERC,  CAUSE(1) )
                     CALL REPMC  ( CAUSE(1), '#',  PHRASE,  CAUSE(1) )
                     CALL REPMC  ( CAUSE(1), '#',  COUNTC,  CAUSE(1) )


                     CALL M2MARK ( STRING, OVERSB, OVERSE,  CAUSE )
                     CAUSE(2) = CAUSE(1)

                  END IF

!
!              If required range but no keyword, error could be
!              misspelled keyword ( we estimate this ) or keyword
!              was missing.
!
               ELSE IF (       (MCOUNT .GE. LOWER) &
                         .AND. (MCOUNT .LE. UPPER)  ) THEN


!
!                 Add METASC points to the score for each of the
!                 words encountered.
!

                  IF ( SE .EQ. 0 ) THEN

!
!                    We are going to try to see if we had a spelling
!                    error that caused us to run out of string
!
                     CALL FNDNWD ( STRING, SUFFSB, DB, DE )

                     ORIGNL = SUFFSB
                     COUNT  = LOWER + 1
                     BSCORE = 0
                     DCOUNT = 0

                     DO WHILE ( COUNT .LE. MCOUNT )

                        IF     ( USEKEY ) THEN

                           KNOWN(1) = TEMP(KB:KE)

!
!                          Compare the last word encountered in the
!                          string with the KEYWORD we were expecting.
!
                           CALL BESTWD(STRING(DB:DE), KNOWN,  CUTOFF, &
                                       BEST,          SCORES, MSSG  )

                        ELSE IF ( USELST ) THEN

!
!                          Compare the last word that we hit with one
!                          of the keywords from the list of possible
!                          closing keywords.
!
                           CALL BESTWD(STRING(DB:DE), KEYWDS, CUTOFF, &
                                       BEST,          SCORES, MSSG  )

                        END IF

                        IF (       ( CARDI (SCORES) .GT. 0      ) &
                             .AND. ( SCORES(1     ) .GE. CUTOFF ) ) &
                        THEN

!
!                          We are going to treat this as a spelling
!                          error.
!
                           IF ( M2CODE .EQ. 0 ) THEN
                              M2CODE = 13
                           END IF

!
!                          Save the beginning and ending of the
!                          problem word for use in the recovery
!                          entry point.
!
                           IF ( SCORES(1) .GT. BSCORE ) THEN

                              BSCORE = SCORES(1)
                              PBEG   = DB
                              PEND   = DE

!
!                             Everything up to this is now regarded
!                             as simply matching the META-KEY.  Store
!                             this number of META-KEYs for use by
!                             diagnostics generation.
!
                              DCOUNT = COUNT - 1

                           END IF

                        END IF

                        SUFFSB = DE + 1

!
!                       Look at the next word until we have gone
!                       past UPPER even if we already have a
!                       candidate for misspelling, there might be
!                       a better one.
!
                        CALL FNDNWD ( STRING, SUFFSB, DB, DE )

                        COUNT = COUNT + 1

                     END DO

!
!                    Save the misspelling information associated
!                    with the best match (if there was one).
!
                     IF ( BSCORE .GT. 0 ) THEN

                        IF     ( USEKEY ) THEN

                           KNOWN(1) = TEMP(KB:KE)

!
!                          Compare the last word encountered in the
!                          string with the KEYWORD we were expecting.
!
                           CALL BESTWD( STRING(PBEG:PEND),  KNOWN, &
                                        CUTOFF,             BEST, &
                                        SCORES,             MSSG  )

                        ELSE IF ( USELST ) THEN

!
!                          Compare the last word that we hit with one
!                          of the keywords from the list of possible
!                          closing keywords.
!
                           CALL BESTWD( STRING(PBEG:PEND), KEYWDS, &
                                        CUTOFF,            BEST, &
                                        SCORES,            MSSG  )

!
!                          Save the best matches for use in the
!                          recovery entry point.
!
                           DO I = 1, CARDI(BEST)
                              KNOWN(I) = KEYWDS( BEST(I) )
                           END DO

                           CALL SCARDC ( CARDI(BEST), KNOWN )

                        END IF

!
!                       This is not regarded as an error worth
!                       stopping for unless our
!                       misspelling total has runs over 100.
!
                        MSPELL = MSPELL + ( 100 - BSCORE )

                        IF ( MSPELL .LT. 100 ) THEN
                           SCORE = SCORE + DCOUNT*METASC + BSCORE

!
!                          Back the value of SBEGIN back up to the
!                          point of failure, so that we can continue
!                          processing as if nothing had gone wrong.
!
                           SBEGIN = PEND + 1
                        ELSE
                           SCORE = SCORE + DCOUNT*METASC
                           ERROR = .TRUE.
                        END IF

                     ELSE

!
!                       Restore the initial value of SUFFSB
!
                        SUFFSB = ORIGNL
                        SCORE  = SCORE + MCOUNT * METASC

                        IF ( M2CODE .EQ. 0 ) THEN
                           M2CODE = 103
                        END IF


!
!                       This occurs if we ran out of stuff in STRING
!                       and we were looking to find a keyword instead.
!
                        ERROR  = .TRUE.

                     END IF

                     IF      ( USEKEY .AND. REASON ) THEN

                        CAUSE(1) = 'I was looking for the keyword "'

                        CALL SUFFIX ( TEMP(KB:KE),            1, CAUSE(1) )
                        CALL SUFFIX ( '" when I reached the', 1, CAUSE(1) )
                        CALL SUFFIX ( 'end of the input ',    1, CAUSE(1) )
                        CALL SUFFIX ( 'command. ',            1, CAUSE(1) )

                     ELSE IF ( USELST .AND. REASON ) THEN

                        CAUSE(1) = 'I was looking for one of the '   // &
                                   'keywords that follow when I '    // &
                                   'reached the end of the input '   // &
                                   'command.  Keywords: {'

                        DO I = 1, NKEY

                           CALL SUFFIX ( KEYWDS(I), 2, CAUSE(1) )
                           CALL SUFFIX ( ',',       0, CAUSE(1) )

                        END DO

                        CAUSE(1)(QLSTNB(CAUSE(1)):) = ' }.'

                     END IF

                     IF ( REASON .AND. ( BSCORE .NE. 0 ) ) THEN

                        CAUSE(2) = CAUSE(1)
                        CAUSE(1) = ' '

                        CALL SUFFIX ( MSSG,           1,      CAUSE(1)   )
                        CALL M2MARK ( STRING, PBEG,   PEND,   CAUSE   )
                        CALL SUFFIX ( MSSG,           1,      CAUSE(2))
                        CALL M2MARK ( STRING, PBEG,   PEND,   CAUSE(2))

                     ELSE IF ( REASON ) THEN

                        CALL M2MARK ( STRING, LASTSB, LASTSE, CAUSE   )
                        CAUSE(2) = CAUSE(1)

                     END IF


!
!                 Recall that we are examining the case when the number
!                 of word matches is within the expected range, but
!                 no keyword was present.  We have already looked at
!                 what to do if we ran out of string prematurely.
!
                  ELSE IF ( SE .GT. 0 ) THEN


                     SCORE = SCORE + MCOUNT * METASC

!
!                    We ran into something unexepected.  Possibly
!                    a misspelled keyword.  See if any of the
!                    expected keywords are close to what we got.
!
                     IF  ( USEEND ) THEN

                        ERROR  = .TRUE.

                        IF ( M2CODE .EQ. 0 ) THEN
                           M2CODE = 104
                        END IF

                        IF ( REASON ) THEN

                           CAUSE(1) = 'The input command contains '   // &
                                      'extra characters that are not '// &
                                      'part of a valid command.  '

                           CALL M2MARK ( STRING, SB, SE, CAUSE       )

                           CAUSE(2) = CAUSE(1)

                        END IF

                     ELSE IF ( USEKEY .OR. USELST ) THEN


                        IF     ( USEKEY ) THEN

                           KNOWN(1) = TEMP(KB:KE)

!
!                          Compare the last word encountered in the
!                          string with the KEYWORD we were expecting.
!

                           CALL BESTWD ( STRING(SB:SE), KNOWN,  CUTOFF, &
                                         BEST,          SCORES, MSSG   )

                        ELSE IF ( USELST ) THEN

!
!                          Compare the last word that we hit with one
!                          of the keywords from the list of possible
!                          closing keywords.
!
                           CALL BESTWD ( STRING(SB:SE), KEYWDS, CUTOFF, &
                                         BEST,          SCORES, MSSG   )

!
!                          Save the best matches for use in the recovery
!                          entry point.
!
                           DO I = 1, CARDI(BEST)
                              KNOWN(I) = KEYWDS( BEST(I) )
                           END DO

                           CALL SCARDC ( CARDI(BEST), KNOWN )

                        END IF

!
!                       We are still checking out the case in which we
!                       had a correct range of words for a variable
!                       length template, but ran into
!                       something that was not a terminating keyword
!                       that we were expecting.  Possibly we hit a
!                       mispelled keyword.
!
!                       Well? Was there anything to the rumor of a
!                       spelling error?
!
                        IF (       ( CARDI (SCORES) .GT. 0      ) &
                             .AND. ( SCORES(1     ) .GE. CUTOFF ) ) THEN

                           IF ( M2CODE .EQ. 0 ) THEN
                              M2CODE = 10

!
!                             Save the beginning and ending of the
!                             problem word for use in the recovery
!                             entry point.
!
                              PBEG  = SB
                              PEND  = SE

                           END IF

!
!                          This is probably a spelling error.
!                          Point out the error.
!
                           MSPELL = MSPELL + ( 100 - SCORES(1) )

                           IF ( MSPELL .LT. 100 ) THEN
                              SCORE  = SCORE + SCORES(1)
                           ELSE
                              ERROR  = .TRUE.
                           END IF

                           IF ( REASON ) THEN

!
!                             Construct an error message indicating
!                             the spelling diagnostic.
!
                              ERROR  = .TRUE.

                           END IF

                        ELSE IF (    (CARDI (SCORES) .EQ. 0     ) &
                                 .OR.(SCORES(1     ) .LT. CUTOFF)) THEN

!
!                          This is not a misspelling.
!                          Set the error flag
!
                           ERROR  = .TRUE.

                           IF ( M2CODE .EQ. 0 ) THEN
                              M2CODE = 105
                           END IF

                           MSSG = ' '

                        END IF

                        IF ( REASON .AND. USEKEY ) THEN

                           CAUSE(1) = 'I was looking for the '

                           CALL SUFFIX ('keyword "',    1, CAUSE(1))
                           CALL SUFFIX (TEMP(KB:KE),    0, CAUSE(1))
                           CALL SUFFIX ('" when I ',    0, CAUSE(1))
                           CALL SUFFIX ('encountered ', 1, CAUSE(1))
                           CALL SUFFIX ('the word "',   1, CAUSE(1))
                           CALL SUFFIX (STRING(SB:SE),  0, CAUSE(1))
                           CALL SUFFIX ('".   ',        0, CAUSE(1))
                           CALL SUFFIX ( MSSG,          1, CAUSE(1))
                           CAUSE(2) = CAUSE(1)
                           CAUSE(1) = MSSG

                           CALL M2MARK ( STRING, SB,   SE, CAUSE(1))
                           CALL M2MARK ( STRING, SB,   SE, CAUSE(2))

                        ELSE IF ( REASON .AND. USELST ) THEN

                           CAUSE(1) = 'I was looking for one of ' // &
                                      'the keywords in the list: '// &
                                      '{ '

                           DO I = 1, NKEY
                              CALL SUFFIX ( KEYWDS(I),  1, CAUSE(1))

                              IF ( I .NE. NKEY ) THEN
                                 CALL SUFFIX ( ',',     0, CAUSE(1))
                              END IF
                           END DO

                           CALL SUFFIX ('}  when I ',   1, CAUSE(1))
                           CALL SUFFIX ('encountered ', 1, CAUSE(1))
                           CALL SUFFIX ('the word "',   1, CAUSE(1))
                           CALL SUFFIX (STRING(SB:SE),  0, CAUSE(1))
                           CALL SUFFIX ('".   ',        0, CAUSE(1))
                           CALL SUFFIX ( MSSG,          1, CAUSE(1))

                           CALL M2MARK ( STRING, SB,   SE, CAUSE)
                           CAUSE(2) = CAUSE(1)

                        END IF

                     END IF

                  END IF


!
!              If out of range and no keyword then we don't have
!              a good guess as to what went wrong.
!
               ELSE IF ( .NOT. KEYWRD &
                         .AND. (    ( MCOUNT .LT. LOWER ) &
                                .OR.( MCOUNT .GT. UPPER ) ) ) THEN


                  IF      ( MCOUNT .LT. LOWER ) THEN

                     IF ( M2CODE .EQ. 0 ) THEN
                        M2CODE = 106
                     END IF

                     SCORE = SCORE + METASC * MAX (0, 2*MCOUNT - LOWER)

                     ERROR  = .TRUE.

                  ELSE IF ( MCOUNT .GT. UPPER ) THEN

                     IF ( USEKEY .OR. USELST ) THEN

!
!                       We are going to try to see if we had a spelling
!                       error that caused us to get too many words.
!
                        CALL FNDNWD ( STRING, SUFFSB, DB, DE )

                        COUNT  = LOWER + 1
                        BSCORE = 0
                        DCOUNT = 0

                        DO WHILE ( COUNT .LE. UPPER + 1 )

                           IF     ( USEKEY ) THEN

                              KNOWN(1) = TEMP(KB:KE)

!
!                             Compare the last word encountered in the
!                             string with the KEYWORD we were expecting.
!
                              CALL BESTWD(STRING(DB:DE), KNOWN,  CUTOFF, &
                                          BEST,          SCORES, MSSG  )

                           ELSE IF ( USELST ) THEN

!
!                             Compare the last word that we hit with one
!                             of the keywords from the list of possible
!                             closing keywords.
!
                              CALL BESTWD(STRING(DB:DE), KEYWDS, CUTOFF, &
                                          BEST,          SCORES, MSSG  )

                           END IF

                           IF (       ( CARDI (SCORES) .GT. 0      ) &
                                .AND. ( SCORES(1     ) .GE. CUTOFF ) ) &
                           THEN

!
!                             We are going to treat this as a spelling
!                             error.
!
                              IF ( M2CODE .EQ. 0 ) THEN
                                 M2CODE = 12
                              END IF

!
!                             Save the beginning and ending of the
!                             problem word for use in the recovery
!                             entry point.
!
                              IF ( SCORES(1) .GT. BSCORE ) THEN

                                 BSCORE = SCORES(1)
                                 PBEG   = DB
                                 PEND   = DE

!
!                                Everything up to this is now regarded
!                                as simply matching the META-KEY.  Store
!                                this number of META-KEYs for use by
!                                diagnostics generation.
!
                                 DCOUNT = COUNT - 1

                              END IF

                           END IF

                           SUFFSB = DE + 1

!
!                          Look at the next word until we have gone
!                          past UPPER even if we already have a
!                          candidate for misspelling, there might be
!                          a better one.
!
                           CALL FNDNWD ( STRING, SUFFSB, DB, DE )

                           COUNT = COUNT + 1

                        END DO

!
!                       Save the misspelling information associated
!                       with the best match (if there was one).
!
                        IF ( BSCORE .GT. 0 ) THEN

                           IF     ( USEKEY ) THEN

                              KNOWN(1) = TEMP(KB:KE)

!
!                             Compare the last word encountered in the
!                             string with the KEYWORD we were expecting.
!
                              CALL BESTWD( STRING(PBEG:PEND),  KNOWN, &
                                           CUTOFF,             BEST, &
                                           SCORES,             MSSG  )

                           ELSE IF ( USELST ) THEN

!
!                             Compare the last word that we hit with one
!                             of the keywords from the list of possible
!                             closing keywords.
!
                              CALL BESTWD( STRING(PBEG:PEND), KEYWDS, &
                                           CUTOFF,            BEST, &
                                           SCORES,            MSSG  )

!
!                             Save the best matches for use in the
!                             recovery entry point.
!
                              DO I = 1, CARDI(BEST)
                                 KNOWN(I) = KEYWDS( BEST(I) )
                              END DO

                              CALL SCARDC ( CARDI(BEST), KNOWN )

                           END IF

!
!                          This is not regarded as an error worth
!                          stopping for unless our
!                          misspelling total has runs over 100.
!
                           MSPELL = MSPELL + ( 100 - BSCORE )

                           IF ( MSPELL .LT. 100 ) THEN
                              SCORE = SCORE + DCOUNT*METASC + BSCORE

!
!                             Back the value of SBEGIN back up to the
!                             point of failure, so that we can continue
!                             processing as if nothing had gone wrong.
!
                              SBEGIN = PEND + 1
                           ELSE
                              SCORE = SCORE + DCOUNT*METASC
                              ERROR = .TRUE.
                           END IF

                        END IF

                     END IF

!
!                    We might not have had a good candidate for a
!                    misspelling, if not we don't have a good clue
!                    as to what went wrong.
!
                     IF ( M2CODE .EQ. 0 ) THEN

                        M2CODE = 107
                        SCORE  = SCORE &
                               + METASC * MAX (0, 2*UPPER - MCOUNT)

                        ERROR  = .TRUE.

                     END IF


                  END IF

!
!                 If there is to be a diagnostic generated, set up
!                 the beginning of it so that everyone else can
!                 share in the same work.
!
                  IF ( REASON ) THEN

                     ERROR = .TRUE.

                     CAUSE(1) = 'I was expecting to see between ' &
                     //         '# and # # '

                     CALL M2CLSS ( TEMP(TB:TC),UPPER,  PHRASE )
                     CALL REPMC  ( CAUSE(1), '#', LOWERC, CAUSE(1) )
                     CALL REPMC  ( CAUSE(1), '#', UPPERC, CAUSE(1) )
                     CALL REPMC  ( CAUSE(1), '#', PHRASE, CAUSE(1) )



                     IF      ( USEKEY ) THEN

                        CALL SUFFIX ( 'followed by ',      1, CAUSE(1) )
                        CALL SUFFIX ( 'the keyword, ',     1, CAUSE(1) )
                        CALL SUFFIX ( TEMP(KB:KE),         1, CAUSE(1) )
                        CALL SUFFIX ( '.',                 0, CAUSE(1) )

                     ELSE IF ( USELST ) THEN

                        CALL SUFFIX ( 'followed by ',      1, CAUSE(1) )
                        CALL SUFFIX ( 'one of the ',       1, CAUSE(1) )
                        CALL SUFFIX ( 'keywords from the', 1, CAUSE(1) )
                        CALL SUFFIX ( 'list {',            1, CAUSE(1) )

                        DO I = 1, NKEY

                           CALL SUFFIX ( KEYWDS(I),        1, CAUSE(1) )

                           IF ( I .NE. NKEY ) THEN
                              CALL SUFFIX ( ',',           1, CAUSE(1) )
                           END IF
                        END DO

                        CALL SUFFIX ( '}.',                1, CAUSE(1) )

                     ELSE IF ( USEEND ) THEN

                        CALL SUFFIX ( 'filling out the   ',1, CAUSE(1) )
                        CALL SUFFIX ( 'end of the string.',1, CAUSE(1) )

                     END IF

!
!                    Use the information stored in M2CODE to determine
!                    how many words we encountered before we figured
!                    out we had an error.
!
                     IF ( M2CODE .GE. 100 ) THEN

                        CALL INTTXT ( MCOUNT, COUNTC  )
                        CALL LCASE  ( COUNTC, COUNTC  )

                     ELSE

                        CALL INTTXT ( DCOUNT, COUNTC )
                        CALL LCASE  ( COUNTC, COUNTC )

                     END IF

                     CALL SUFFIX ( 'I had counted ',      1, CAUSE(1) )
                     CALL SUFFIX ( COUNTC,                1, CAUSE(1) )

                     IF ( MCOUNT .EQ. 1 ) THEN
                        CALL SUFFIX ( 'such word',  1, CAUSE(1) )
                     ELSE
                        CALL SUFFIX ( 'such words', 1, CAUSE(1) )
                     END IF

                     CALL SUFFIX ( 'when I encountered',  1, CAUSE(1) )

                  END IF

!
!                 We are still in the case of a variable length template
!                 for which we did not hit a keyword and did not have
!                 the expected range of items for the current META-KEY.
!
!                 OK. Now tailor the end of the message to reflect
!                 what went wrong in particular.
!
                  IF ( REASON .AND. ( M2CODE .LT. 100 ) ) THEN

                     CALL SUFFIX ( 'the word "',          1, CAUSE(1) )
                     CALL SUFFIX ( STRING(PBEG:PEND),     0, CAUSE(1) )
                     CALL SUFFIX ( '" .',                 0, CAUSE(1) )

                     CALL SUFFIX ( MSSG,                  1, CAUSE(1) )

                     CALL M2MARK ( STRING, PBEG,       PEND, CAUSE )
                     CAUSE(2) = CAUSE(1)
                  ELSE IF ( REASON .AND. ( SE .EQ. 0 ) ) THEN

                     CALL SUFFIX ( 'the end of the input',1,   CAUSE(1) )
                     CALL SUFFIX ( 'string.    ',         1,   CAUSE(1) )
                     CALL M2MARK ( STRING, QLSTNB(STRING)+1, &
                                           QLSTNB(STRING)+1,   CAUSE )
                     CAUSE(2) = CAUSE(1)

!
!                  check for a misspell.
!

                  ELSE IF (  REASON .AND. ( SE  .NE. 0   )  ) THEN

                     CALL SUFFIX ( 'the word "',          1, CAUSE(1) )
                     CALL SUFFIX ( STRING(SB:SE),         0, CAUSE(1) )
                     CALL SUFFIX ( '" .',                 0, CAUSE(1) )

!
!                    If misspell likely mention that too.
!
                     IF      ( USEKEY ) THEN

                        KNOWN(1) = TEMP(KB:KE)

                        CALL BESTWD ( STRING(SB:SE), KNOWN,  CUTOFF, &
                                      BEST,          SCORES, MSSG    )


                     ELSE IF ( USELST ) THEN

                        CALL BESTWD ( STRING(SB:SE), KEYWDS, CUTOFF, &
                                      BEST,          SCORES, MSSG    )

                     END IF

                     IF (       ( CARDI ( SCORES ) .GT. 0      ) &
                          .AND. ( SCORES(1)        .GT. CUTOFF ) ) THEN

                        CALL SUFFIX ( MSSG, 1, CAUSE(1) )


                     END IF

                     CALL M2MARK ( STRING, SB,    SE, CAUSE     )
                     CAUSE(2) = CAUSE(1)

                  END IF

               END IF


            ELSE

!
!              This "ELSE" is the "NO" response to the question:  "Ok.
!              we have a range template. Is it of variable length?"
!
               ENDIT   = LOWER .EQ. 0
               MCOUNT  = 0

               DO WHILE ( .NOT. ENDIT )

                  CALL FNDNWD ( STRING, SBEGIN, SB, SE )

                  IF      ( SE .EQ. 0 ) THEN

                     ENDIT  = .TRUE.
                     ERROR  = .TRUE.

                     IF ( M2CODE .EQ. 0 ) THEN
                        M2CODE = 108
                     END IF

                     IF ( REASON ) THEN

                        CAUSE(1) = 'I was expecting to see # ' &
                        //         '# when I ran out of ' &
                        //         'words in the command string. '


                        CALL M2CLSS ( TEMP(TB:TC), 1, PHRASE )
                        ARTCLE = ANA( PHRASE, 'L'              )
                        CALL REPMC  ( CAUSE(1), '#', ARTCLE, CAUSE(1) )
                        CALL REPMC  ( CAUSE(1), '#', PHRASE, CAUSE(1) )

                        CALL M2MARK( STRING, QLSTNB(STRING)+1, &
                                             QLSTNB(STRING)+1, CAUSE )

                        CAUSE(2) = CAUSE(1)
                     END IF

                  ELSE IF ( M2WMCH ( STRING, SB, SE, TEMP(TB:TE) ) ) &
                  THEN

                     MCOUNT  = MCOUNT + 1
                     SCORE  =  SCORE  + METASC

                     SBEGIN = SE + 1

!
!                    Mark the position of the first character beyond the
!                    current STRING word.
!
                     BEGOUT = SBEGIN
                     ENDIT  = MCOUNT .GE. LOWER

                  ELSE

                    IF ( M2CODE .EQ. 0 ) THEN
                       M2CODE = 109
                    END IF

                     ERROR  = .TRUE.
                     ENDIT  = .TRUE.
                     IF ( REASON ) THEN

                        CAUSE(1) = 'I was expecting to see # # ' &
                        //         'when I encounterd the word ' &
                        //         '"#" in the command. '

                        CALL M2CLSS ( TEMP(TB:TC), 1, PHRASE )
                        ARTCLE = ANA( PHRASE, 'L'              )
                        CALL REPMC  ( CAUSE(1), '#', ARTCLE,        CAUSE(1) )
                        CALL REPMC  ( CAUSE(1), '#', PHRASE,        CAUSE(1) )
                        CALL REPMC  ( CAUSE(1), '#', STRING(SB:SE), CAUSE(1) )


                        CALL M2MARK ( STRING, SB,           SE, CAUSE )
                        CAUSE(2) = CAUSE(1)

                     END IF

                  END IF

               END DO

            END IF

         ELSE

            CALL FNDNWD ( STRING, SBEGIN, SB, SE )

!
!           This "ELSE" is the "NO" response to the question: "Is a
!           range template present?" that was asked a very long, long
!           time ago.
!
            CMATCH = M2WMCH ( STRING, SB, SE, TEMP(TB:TE) )

!
!           Set the string pointer to the first character following
!           the current string word.
!
            SBEGIN = SE + 1

!
!           Record SBEGIN in case we have run out of teplate and
!           haven't produced any errors.
!
            BEGOUT = SBEGIN

            IF ( CMATCH ) THEN

               KEYWRD = M2KEYW ( TEMP(TB:TE) )

               IF ( KEYWRD )THEN
                  SCORE  = SCORE  + KEYSC
               ELSE
                  SCORE  = SCORE  + METASC
               END IF


            ELSE IF ( .NOT. CMATCH ) THEN

               KEYWRD = M2KEYW ( TEMP(TB:TE) )

!
!              See if we were supposed to get a keyword and if
!              so see if this is just some simple spelling error.
!
               IF ( KEYWRD ) THEN

                  KNOWN (1) = TEMP(TB:TC)
                  CALL  SCARDC ( 1, KNOWN )

                  IF ( SE .GT. 0 ) THEN

                     CALL BESTWD ( STRING(SB:SE), KNOWN,  CUTOFF, &
                                   BEST,          SCORES, MSSG    )
                  END IF

                  IF (       ( CARDI(SCORES) .GT. 0      ) &
                       .AND. ( SCORES(1)     .GE. CUTOFF ) ) THEN

                     IF ( M2CODE .EQ. 0 ) THEN
                        M2CODE = 11

!
!                       Save the beginning and ending of the
!                       problem word for use in the recovery
!                       entry point.
!
                        PBEG  = SB
                        PEND  = SE

                     END IF

!
!                    We regard this to be a spelling error  of the
!                    keyword. This will be a signal to stop looking at
!                    this keyword if we are asking for diagnostics.
!

                     IF ( MSPELL .GT. 100 ) THEN
                        ERROR  = .TRUE.
                     ELSE
                        SCORE  = SCORE  + SCORES(1)
                        MSPELL = MSPELL + ( 100 - SCORES(1) )
                     END IF

                     IF ( REASON ) THEN

                        ERROR  = .TRUE.

                        CAUSE(1) = 'I was expecting to see the '      // &
                                   'keyword "'

                        CALL SUFFIX ( TEMP(TB:TC),            0, CAUSE(1) )
                        CALL SUFFIX ( '" when I encountered', 0, CAUSE(1) )
                        CALL SUFFIX ( 'the word "',           1, CAUSE(1) )
                        CALL SUFFIX ( STRING(SB:SE),          0, CAUSE(1) )
                        CALL SUFFIX ( '" in the input ',      0, CAUSE(1) )
                        CALL SUFFIX ( 'string.     ',         1, CAUSE(1) )
                        CALL SUFFIX ( MSSG,                   1, CAUSE(1) )
                        CAUSE(2) = CAUSE(1)
                        CAUSE(1) = MSSG
                        CALL M2MARK ( STRING, SB,    SE,      CAUSE(1) )
                        CALL M2MARK ( STRING, SB,    SE,      CAUSE(2) )

                     END IF

                  ELSE IF (      ( CARDI(SCORES) .EQ. 0      ) &
                            .OR. ( SCORES(1)     .LT. CUTOFF ) ) THEN

                     ERROR  = .TRUE.

                     IF ( M2CODE .EQ. 0 ) THEN
                        M2CODE = 110

                        IF ( SE .GT. 0 ) THEN

                           CALL BESTWD ( STRING(SB:SE), KNOWN,  1, &
                                         BEST,          SCORES, MSSG )

                        END IF

                        IF (       ( SB            .NE. 0 ) &
                             .AND. ( CARDI(SCORES) .GT. 0 ) ) &
                        THEN
                           SCORE = SCORE + SCORES(1)
                        END IF
                     END IF


                     IF (REASON) THEN

                        CAUSE(1) = 'I was expecting to see the '      // &
                                   'keyword "'

                        CALL SUFFIX ( TEMP(TB:TC),            0, CAUSE(1) )
                        CALL SUFFIX ( '" when I ',            0, CAUSE(1) )

                        IF ( SB .EQ. 0 ) THEN
                           CALL SUFFIX ( 'ran out of ',       1, CAUSE(1) )
                           CALL SUFFIX ( 'characters in the', 1, CAUSE(1) )
                           CALL SUFFIX ( 'input string. ',    1, CAUSE(1) )

                           SB = QLSTNB ( STRING ) + 1
                           SE = SB
                        ELSE
                           CALL SUFFIX ( 'encountered',       1, CAUSE(1) )
                           CALL SUFFIX ( 'the word "',        1, CAUSE(1) )
                           CALL SUFFIX ( STRING(SB:SE),       0, CAUSE(1) )
                           CALL SUFFIX ( '" in the input ',   0, CAUSE(1) )
                           CALL SUFFIX ( 'string.     ',      1, CAUSE(1) )
                        END IF

                        CALL M2MARK ( STRING, SB,    SE,         CAUSE )
                        CAUSE(2) = CAUSE(1)

                     END IF

                  END IF

               ELSE IF ( .NOT. M2KEYW(TEMP(TB:TE)) ) THEN

                  ERROR  = .TRUE.

                  IF ( M2CODE .EQ. 0 ) THEN
                     M2CODE = 111
                  END IF

                  IF ( REASON ) THEN

                     CAUSE(1) = 'I was expecting to see # # ' &
                     //         'when I '


                     CALL M2CLSS ( TEMP(TB:TC), 1, PHRASE )
                     ARTCLE = ANA( PHRASE, 'L'              )
                     CALL  REPMC ( CAUSE(1), '#', ARTCLE,        CAUSE(1) )
                     CALL  REPMC ( CAUSE(1), '#', PHRASE,        CAUSE(1) )


                     IF ( SB .EQ. 0 ) THEN
                        CALL SUFFIX ( 'ran out of characters', 1, CAUSE(1))
                        CALL SUFFIX ( 'in the input string. ', 1, CAUSE(1))

                        SB = QLSTNB ( STRING ) + 1
                        SE = SB
                     ELSE
                        CALL SUFFIX ( 'encountered the word "', 1, CAUSE(1))
                        CALL SUFFIX ( STRING(SB:SE),            0, CAUSE(1))
                        CALL SUFFIX ( '" in the input string.', 0, CAUSE(1))
                     END IF

                     CALL M2MARK (STRING, SB, SE, CAUSE )
                     CAUSE(2) = CAUSE(1)

                  END IF

               END IF

            END IF

         END IF

         TBEGIN = MAX(KE,TE) + 1

!
!        Locate the next word of the template and continue unless
!        we get a second error detected.
!
         CALL FNDNWD ( TEMP, TBEGIN, TB, TE )
      END DO


!
!     If we got out of the template without an error, set SBEG to
!     BEGOUT---the first character after the matched portion of the
!     STRING and before the first word of whatever is left.
!
      IF ( M2CODE .EQ. 0 ) THEN
         SBEG = BEGOUT
      END IF

      RETURN


!
!$Prodedure M2RCVR ( Recover from a spelling error )
!
      ENTRY M2RCVR ( SBEG, SEND, KEYWDS )

!
! $ Abstract
!
!     Fetch the indices of the beginning and end of a "misspelled"
!     keyword along with the list of corrections.
!
! $ Required_Reading
!
!     The META/2 book.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     INTEGER               SBEG
!     INTEGER               SEND
!     CHARACTER*(*)         KEYWDS ( LBCELL: * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SBEG       O   Beginning of "misspelled" word in STRING
!     SEND       O   Ending of "misspelled" word in STRING
!     KEYWDS     O   Cell of possible correct spellings of keyword.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     SBEG       Beginning of "misspelled" word in STRING
!
!     SEND       Ending of "misspelled" word in STRING
!
!     KEYWDS     Cell of possible correct spellings of keyword.
!
! $ Error_Handling
!
!     No errors are detected by this entry point.
!
! $ Input_Files
!
!     None.
!
! $ Output_Files
!
!     None.
!
! $ Particulars
!
!
! $ Examples
!
!
! $ Restrictions
!
!     One must call M2MTCH before calling this routine if correct
!     results are desired.
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
!     Version B1.0.0, 7-APR-1988 (WLT) (IMU)
!
!-&
!



      SBEG = PBEG
      SEND = PEND

      CALL COPYC ( KNOWN, KEYWDS )


      RETURN

      END
