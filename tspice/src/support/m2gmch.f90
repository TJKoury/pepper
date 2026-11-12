
!$Procedure      M2GMCH ( Match a META/2 template including groups )

      SUBROUTINE M2GMCH ( TEMP,    THNWDS, &
                          STRING,  SBEG,   REASON, CUTOFF, &
                          PSSTHN,  M2CODE, SCORE,  CAUSE   )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!      This routine will match a META/2 template that contains no
!      qualified @then directives.
!
! $ Required_Reading
!
!      The META/2 book.
!
! $ Keywords
!
!     PARSING
!     STRING
!     UTILITY
!
! $ Declarations

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         TEMP
      CHARACTER*(*)         THNWDS  ( LBCELL : * )
      CHARACTER*(*)         STRING
      INTEGER               SBEG
      LOGICAL               REASON
      INTEGER               CUTOFF
      LOGICAL               PSSTHN
      INTEGER               M2CODE
      INTEGER               SCORE
      CHARACTER*(*)         CAUSE   ( 2 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TEMP       I   A META/2 template
!     THNWDS     I   A cell of initial keywords for a following @then
!     STRING     I   A candidate META/2 sentence.
!     SBEG       I   Where to start processing this sentence
!     REASON     I   Flag indicating diagnostics should be produced.
!     CUTOFF     I   Threshold used for spelling error diagnostics.
!     PSSTHN     O   Flag to indicate we made it past a @then
!     M2CODE     O   META/2 code indicating how a match failed.
!     SCORE      O   a measure of how well STRING matched TEMP.
!     CAUSE      O   Diagnostic message if requested for non-matches.
!
! $ Detailed_Input
!
!     TEMP       is a META/2 template to be compared with a portion
!                of the candidate input sentence.
!
!     THNWDS     is a cell containing KEYWORDS that may be used as
!                terminators for the entire template.  Typically
!                this cell will contain the initial keywords of
!                a class of templates that can be branched to from
!                this template.
!
!     STRING     A string, a substring of which ( STRING(SBEG:) )
!                should be compared with the input META/2 template.
!
!     SBEG       is the beginning of the substring that should be
!                compared with TEMP.
!
!     REASON     Is a logical flag, that should be set to .TRUE.
!                if the user wishes to have error mismatch diagnostics
!                to be returned by this routine.
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
!                matched substring. ( Note that words in THNWDS do
!                not qualify as part of the template, but merely
!                serve to delimit the ends of variable length
!                templates. Thus if one of these words was actually
!                used to delimit the end of TEMP, SBEG will point to
!                the beginning of that word in STRING.)
!
!     PSSTHN     is set only if the template matches up to an
!                an unqualified @then directive.  In such a case
!                PSSTHN will be set to .TRUE.  Otherwise it will not
!                be changed from its input value.
!
!     M2CODE     is an integer META/2 code that indicates how an attempt
!                to match the input failed.  If the match was successful
!                M2CODE will be returned as zero.  Otherwise it will
!                be returned with a positive value.  Possible values
!                and meanings are:
!
!
!
!     SCORE      is a measure of how well STRING matched TEMP.  This
!                is useful primarily when looking through several
!                templates, none of which yield an M2CODE of zero.  In
!                this case, the template with the highest SCORE is
!                likely to be the template the input string was
!                "intended" to match.
!
!     CAUSE      If REASON is set to .TRUE. and the match fails
!                (M2CODE .NE. 0 ), this string will contain a
!                description of the suspected cause of the match
!                failure.  Moreover, the input string will be "marked"
!                at the location of the match failure.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1)  If the number of delimiting keywords is greater than 64 a
!         SPICE TOOLKIT error will be SIGERRled.
!
!         SPICE(TOOMANYKEYWORDS)
!
!         Delimiting keywords are:
!
!         a) Keywords that immediately follow group templates.
!         b) Keywords that are the initial keywords of a simple template
!            contained within a group template.
!         c) The keywords passed to the routine in the cell THNWDS.
!
! $ Particulars
!
!     This routine allows one to compare strings with those META/2
!     language templates that do not end with a qualified-'@then'.
!     Moreover, it serves as the principle tool for matching the
!     various pieces of full META/2 templates.  If a match occurs
!     the remainder of the string can be compared with the templates
!     pointed to by the @then  directive.
!
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     The maximum number of delimiting keywords is 64.
!
!     No checks are made to see if the template supplied is in fact
!     a valid META/2 template.
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
!-     META/2 Configured Version 2.1.0, 08-NOV-2005 (BVS)
!
!        Updated to remove non-standard use of duplicate arguments
!        in FNDNWD calls.
!
!-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 9, 1994
!
!
!-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
!
!         Added an extra blank after a carriage return "/cr"
!         substring in
!
!           DIAGNS = 'I was trying to match part of the input '  ...
!           //       'string with one of the expresions listed ' ...
!           //       'here: /cr/cr '                             ...
!           //        SUBTMP(1:RTRIM(SUBTMP))                    ...
!           //       './cr/cr The expression that came closest ' ...
!           //       'was: /cr/cr, '
!
!-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of META/2
!         software as of May 3, 1994
!
!
!     Beta Version 1.0.0, 19-MAY-1988 (WLT) (IMU)
!
!-&



!
!     SPICELIB functions
!
      INTEGER               QLSTNB
      INTEGER               UPTO
      LOGICAL               M2KEYW
      LOGICAL               MATCH

!
!     Local variables
!
      LOGICAL               PASS1

      CHARACTER*32          TERMS  ( LBCELL : 64 )

      INTEGER               INDXES ( LBCELL : 64 )

      INTEGER               TLEN
      INTEGER               SIZE

      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               TMPJ

      INTEGER               TBEG
      INTEGER               TEND

      LOGICAL               MORE
      LOGICAL               GROUP
      LOGICAL               SIMPLE
      CHARACTER*8           LAST

      INTEGER               POSITN
      CHARACTER*32          LABEL

      INTEGER               EO
      INTEGER               BO
      LOGICAL               OPTDIR

      INTEGER               BW
      INTEGER               EW

      INTEGER               FEWEST
      INTEGER               MOST

      INTEGER               GMATCH
      INTEGER               BSCORE
      INTEGER               BEGOFG
      INTEGER               ENDOFG
      INTEGER               AFTERG

      INTEGER               BS
      INTEGER               ES

      INTEGER               A
      INTEGER               B
      INTEGER               E
      INTEGER               BLSTWD
      INTEGER               CLSTWD
      INTEGER               ELSTWD

      CHARACTER*32          KEYWDS ( LBCELL : 64 )
      CHARACTER*32          SWORDS ( LBCELL : 64 )

      INTEGER               T1CODE
      INTEGER               TSCORE

      INTEGER               LOC

      INTEGER               BDIAGS
      INTEGER               EDIAGS

      INTEGER               BCODE

      CHARACTER*1024        SUBTMP

      LOGICAL               REDIAG
      LOGICAL               VTEMPL

      SAVE


      DATA                  PASS1   / .TRUE. /


!
!     If this is the first pass through this routine, set the size of
!     the cells TERMS and INDXES
!
      IF ( PASS1 ) THEN

         PASS1 = .FALSE.

         CALL SSIZEC ( 64, TERMS  )
         CALL SSIZEI ( 64, INDXES )
         CALL SSIZEC ( 64, KEYWDS )
         CALL SSIZEC ( 64, SWORDS )

      END IF

!
!     Clear out the parse table.
!
      CALL M2PCLR

!
!     Collect the list of potential terminating keywords.
!
      CALL M2TERM ( TEMP, TERMS, INDXES )

!
!     Append all of the '@then(*)'-keywords to the list of terminators.
!
      IF ( CARDC(TERMS) + CARDC (THNWDS) .GE. SIZEC(TERMS) - 2 ) THEN

         CALL CHKIN  ( 'M2GMCH' )
         CALL SIGERR ( 'SPICE(TOOMANYKEYWORDS)' )
         CALL CHKOUT ( 'M2GMCH' )
         RETURN

      END IF

      TLEN = LEN   ( TEMP  ) + 1
      J    = CARDC ( TERMS ) + 1

      DO I = 1, CARDC( THNWDS )
         TERMS (J) = THNWDS(I)
         INDXES(J) = TLEN
         J         = J + 1
      END DO

!
!     Append a '@end' and a '}' to the end of the terminators, and
!     adjust the cardinality of the TERMS cell
!
      TERMS (J) = '@end'
      INDXES(J) = TLEN
      J         = J + 1

      TERMS (J) = '}'
      INDXES(J) = TLEN
      SIZE      = CARDC(TERMS) + CARDC(THNWDS) + 2

      CALL SCARDC ( SIZE, TERMS  )
      CALL SCARDI ( SIZE, INDXES )

!
!     This routine will only use the portion of the template up
!     to a qualified @then.
!
      CALL M2THNQ ( TEMP, POSITN, LABEL )

      IF ( POSITN .LE. LEN ( TEMP ) ) THEN
         TEMP(POSITN:) = ' '
      END IF

!
!     Now initialize pointers and the loop control variable MORE so
!     that we can start the loop.
!
      TBEG = 1
      TEND = 1

      MORE = .TRUE.

      DO WHILE ( MORE )
!
!     As long as we are not told to exit
!
!        Look at the next word,
!
         CALL FNDNWD ( TEMP, TBEG, BW, EW )

         IF ( BW .EQ. 0 ) THEN

!
!           There wasn't a next word.  There is nothing left to do.
!           We set MORE to .FALSE. so that we can exit the loop.
!
            MORE   = .FALSE.
            GROUP  = .FALSE.
            SIMPLE = .FALSE.

         ELSE IF ( TEMP(BW:EW) .EQ. '@then' ) THEN

!
!           We have an unqualified @then directive.  This means that
!           we are on the right track as far as determining what
!           command we are working on.  Set the PASSED-A-@then flag
!           (PSSTHN) to .TRUE. and the other candidates to .FALSE.
!
            PSSTHN = .TRUE.
            GROUP  = .FALSE.
            SIMPLE = .FALSE.

            TBEG   = EW + 1
            TEND   = TBEG

         ELSE IF ( MATCH( TEMP(BW:EW), '(%*:%*){' ) ) THEN

!
!           We are about to enter a group template.  Determine
!           the FEWEST number of simple templates in the group
!           that must match and the MOST that we will check.
!
            CALL M2BEGR ( TEMP, BW, EW, FEWEST, MOST )

            GROUP  = .TRUE.
            SIMPLE = .FALSE.
            LAST   = 'GROUP'

!
!           Set up the pointers for looking for the simple
!           templates within this group.
!
            TBEG   = EW + 1
            TEND   = TBEG

         ELSE

!
!           The only possible candidate is a simple template.
!
            GROUP  = .FALSE.
            SIMPLE = .TRUE.
            TBEG   = BW
            LAST   = 'SIMPLE'

         END IF


         IF ( GROUP ) THEN

!
!           Set up the initial values for this group.  We need
!
!           1)  The number of simple template matches so far for
!               this group.
!
            GMATCH = 0

!
!           2)  A best score of the simple templates checked so far.
!
            BSCORE = -1

!
!           3)  A temporary place to store the M2CODE returned for
!               a simple template of this group.
!
            T1CODE = 0

!
!           4)  The position in the full template to jump to when we are
!               done with this template, the beginning and the end of
!               the group
!
            BEGOFG = TBEG
            ENDOFG = UPTO ( TEMP, ' }', TBEG  )
            AFTERG = ENDOFG + 3

!
!           Make sure there is a viable simple template within this
!           group.
!
            BS = NCPOS( TEMP,  ' ',    BEGOFG        )
            ES = MIN  ( UPTO ( TEMP, ' | ', BEGOFG  ), &
                        UPTO ( TEMP, ' } ', BEGOFG  )  )

            IF (      ( BS          .EQ. 0      ) &
                 .OR. ( BS          .GE. ES     ) &
                 .OR. ( TEMP(BS:BS) .EQ. '}'    ) &
                 .OR. ( ES          .GT. ENDOFG ) ) THEN

               GROUP = .FALSE.
            END IF

!
!           Finally, if FEWEST is 1 or 0,
!           remove the '}' that has index equal to the index
!           of the '}' that is the terminator of this group.
!
            IF ( FEWEST .LE. 1 ) THEN
               LOC = BSRCHI ( ENDOFG+2, CARDI(INDXES), INDXES(1) )

               IF ( LOC .NE. 0 ) THEN
                  CALL REMLAI ( 1, LOC, INDXES(1), CARDI(INDXES) )
                  CALL REMLAC ( 1, LOC, TERMS(1),  CARDC(TERMS)  )

                  CALL SCARDC ( CARDC(TERMS)  - 1, TERMS      )
                  CALL SCARDI ( CARDI(INDXES) - 1, INDXES     )
               END IF
            END IF

            DO WHILE ( GROUP )

!
!              We've got a viable simple template for this group.
!
!              If it ends with a variable template find out what the
!              possible terminating words are.
!
               A = 0
               B = 0

               CALL FNDPTK ( TEMP, ' ', ES + 1, BLSTWD, ELSTWD       )
               CALL M2BEGR ( TEMP,              BLSTWD, ELSTWD, A, B )

               CLSTWD = MIN ( ELSTWD, BLSTWD + 8 )
               VTEMPL =      A .NE. B &
                      .OR. (.NOT. M2KEYW        ( TEMP(BLSTWD:ELSTWD) ) &
                            .AND.'@calendar' .EQ. TEMP(BLSTWD:CLSTWD) )


               IF ( VTEMPL ) THEN
!
!                 There is a variable length template, the keywords
!                 that might terminate this template are given
!                 in TERMS up to the first occurance of a '}'.
!
                  IF ( GMATCH .LT. MOST-1 ) THEN
                     I = LSTLTI ( BEGOFG, CARDI(INDXES), INDXES(1) ) + 1
                  ELSE
                     I = LSTLTI ( AFTERG, CARDI(INDXES), INDXES(1) ) + 1
                  END IF

                  J = 0

                  DO WHILE ( TERMS(I) .NE. '}' )

!
!                    Keep only those keywords that are not the initial
!                    keyword of this template.
!

                     IF ( INDXES(I) .NE. BS ) THEN
                        J          = J + 1
                        CALL M2TRIM ( TERMS(I), KEYWDS(J) )
                     END IF

                     I             = I + 1
                  END DO

                  CALL SCARDC ( J, KEYWDS )

               ELSE

                  CALL SCARDC ( 0, KEYWDS )

               END IF

!
!              Check the current template with M2MTCH.
!

               IF ( TEMP(BS:ES) .EQ. '@options' ) THEN

                  T1CODE = -1
                  TSCORE = -1

               ELSE

!
!                 Dump the temporary parse table.
!
                  CALL M2TCLR
                  CALL M2MTCH ( TEMP(BS:ES), 1,       KEYWDS, &
                                STRING,      SBEG,    .FALSE., CUTOFF, &
                                T1CODE,      TSCORE,  CAUSE           )

               END IF
!
!              If the attempt at a match succeeded ...
!
               IF ( T1CODE .EQ. 0 ) THEN

!
!                 Increment the number of group matches by 1.
!                 Increment the score for this template.
!                 Set the best score obtained thus far to zero
!                 in preparation for the next pass through the
!                 group.
!
                  GMATCH = GMATCH + 1
                  SCORE  = SCORE  + TSCORE
                  BSCORE = -1

!
!                 Move the temporary parse table to the keepers
!                 parse table.
!
                  CALL M2KEEP
!
!                 The current template should be taken off the viable
!                 list.
!
                  IF ( ES .LT. ENDOFG ) THEN
                     TEMP (BS:ES+2) = ' '
                  ELSE
                     CALL FNDPTK ( TEMP, ' ',  BS, A, B )

                     IF ( TEMP(A:B) .EQ. '|' ) THEN
                        TEMP (  A:ES ) = ' '
                     ELSE
                        TEMP ( BS:ES ) = ' '
                     END IF

                  END IF


!
!                 Reset ES to be the one before the beginning of
!                 the group template (BS will be set to ES + 1
!                 at the end of the group loop).
!
                  ES = BEGOFG - 1

!
!                 Adjust the possible terminating keyword set.
!                 (remove the initial keyword of the simple template
!                 just matched from the collection).
!
                  LOC    = BSRCHI ( BS, CARDI(INDXES), INDXES(1) )

                  CALL REMLAI ( 1, LOC, INDXES(1), CARDI(INDXES) )
                  CALL REMLAC ( 1, LOC, TERMS(1),  CARDC(TERMS)  )

                  CALL SCARDC ( CARDC(TERMS)  - 1, TERMS      )
                  CALL SCARDI ( CARDI(INDXES) - 1, INDXES     )


!
!                 Finally, if we have now exactly matched FEWEST-1,
!                 remove the '}' that has index equal to the index
!                 of the '}' that is the terminator of this group.
!
                  IF ( GMATCH .EQ. FEWEST - 1 ) THEN
                     LOC = BSRCHI ( ENDOFG+2, CARDI(INDXES), INDXES(1) )

                     IF ( LOC .NE. 0 ) THEN
                        CALL REMLAI ( 1, LOC, INDXES(1), CARDI(INDXES) )
                        CALL REMLAC ( 1, LOC, TERMS(1),  CARDC(TERMS)  )

                        CALL SCARDC ( CARDC(TERMS)  - 1, TERMS      )
                        CALL SCARDI ( CARDI(INDXES) - 1, INDXES     )
                     END IF
                  END IF

               ELSE

!
!                 Record the score if this is higher than a previous
!                 value.
!
                  IF ( TSCORE .GT. BSCORE ) THEN

                     BSCORE = TSCORE
                     BDIAGS = BS
                     EDIAGS = ES
                     BCODE  = T1CODE

                     CALL COPYC ( KEYWDS, SWORDS )

                  END IF

               END IF

!
!              Remove all introductory '@options' directives.
!
               OPTDIR = .TRUE.

               DO WHILE ( OPTDIR )

                  BO = NCPOS (        TEMP, ' ',    BEGOFG )
                  EO = MIN   ( UPTO ( TEMP, ' | ',  BEGOFG ), &
                               UPTO ( TEMP, ' } ',  BEGOFG ) )

                  IF ( BO .LT. EO ) THEN

                     OPTDIR = TEMP(BO:EO) .EQ. '@options'

                     IF ( OPTDIR ) THEN
                        TEMP(BO:EO) = ' '
                        EO          = EO+2

                        IF ( TEMP(EO:EO) .EQ. '|' ) THEN
                           TEMP(EO:EO) = ' '
                        END IF
                     END IF

                  ELSE
                     OPTDIR = .FALSE.
                  END IF
               END DO
!
!              Should we stay in this group? Only if you can answer yes
!              to  all of the following:
!
!                  1.) Are more matches allowed for this group.
!
!                  2.) Is there another template in this group that
!                      hasn't been checked.
!
               IF ( GMATCH .GE. MOST ) THEN

                  GROUP = .FALSE.

               ELSE

!
!                 Make sure there is a viable simple template within
!                 this group.
!

                  BS = NCPOS( TEMP,  ' |',    ES + 1        )
                  ES = MIN  ( UPTO ( TEMP, ' | ', BS      ), &
                              UPTO ( TEMP, ' } ', BS      )  )

                  IF (      ( BS          .EQ. 0      ) &
                       .OR. ( BS          .GE. ES     ) &
                       .OR. ( TEMP(BS:BS) .EQ. '}'    ) &
                       .OR. ( ES          .GT. ENDOFG ) ) THEN

                     GROUP = .FALSE.

                  END IF

               END IF


            END DO

!
!           When we leave the group, see if we had a sufficient number
!           of matches.  If we did, jump past the end of the group.
!           If we didn't, this is an error---head for home.
!
            OPTDIR = INDEX ( TEMP(BEGOFG:ENDOFG), ' @options ' ) .NE. 0

            IF (  .NOT. OPTDIR &
                  .AND. GMATCH .GE. FEWEST ) THEN

               TBEG = AFTERG

            ELSE IF ( OPTDIR .AND. ( GMATCH .GE. MOST  )  ) THEN

               IF ( REASON ) THEN

                  CALL CMPRSS ( ' ', 1, TEMP(BEGOFG:ENDOFG), &
                                        TEMP(BEGOFG:ENDOFG)    )

                  B = BEGOFG - 1
                  E = INDEX ( TEMP(BEGOFG:ENDOFG), ' @options ' ) &
                    + 1

                  CAUSE(1) = 'I had already matched the maximum '     // &
                             'number of allowed simple templates in ' // &
                             'a group without matching the  '         // &
                             'following REQUIRED templates./cr/cr(3:3)'
                  CALL SUFFIX ( TEMP(B:E),        1, CAUSE(1) )
                  CALL SUFFIX ('} /cr/cr(-3:-3)', 1, CAUSE(1) )

                  M2CODE =  11000
                  MORE   = .FALSE.

               END IF

            ELSE IF ( OPTDIR .AND. ( GMATCH .GE. FEWEST ) ) THEN

               SCORE = SCORE + BSCORE

!
!              If diagnostics are requested then see what went wrong
!              with the best fitting simple template.
!
               IF ( REASON ) THEN

                  BS = BDIAGS
                  ES = EDIAGS

                  CALL M2MTCH ( TEMP(BS:ES), 1,       SWORDS, &
                                STRING,      SBEG,    REASON, CUTOFF, &
                                T1CODE,      TSCORE,  CAUSE           )

                  CAUSE(2) = CAUSE(1)

                  B        = BEGOFG - 1
                  E        = ENDOFG + 2

                  CALL CMPRSS ( ' ', 1, TEMP(B:E), SUBTMP )

                  IF (       INDEX( SUBTMP, ' | ' ) &
                       .EQ.  INDEX( SUBTMP, ' @options ' ) - 2 ) &
                  THEN

                     CALL PREFIX ( '/cr/cr(-3:)',   1, CAUSE(2) )
                     CALL PREFIX ( SUBTMP,          1, CAUSE(2) )
                     CALL PREFIX ( 'Although I had matched a required'// &
                                   ' number of expressions in the '   // &
                                   'group below, I had not yet match' // &
                                   'ed the explicitely required '     // &
                                   'expression that appears prior to' // &
                                   ' the META/2 "@options" directive '// &
                                   'in the group shown here./cr(3:) ', &
                                    1, CAUSE(2)            )

                     K = POS( SUBTMP, ' | ', INDEX(SUBTMP,' @options '))

                     IF (     ( K                         .GT. 0 ) &
                        .AND. ( NCPOS (STRING, ' ', SBEG) .NE. 0 )) THEN

                        CALL SUFFIX ( '/cr/cr Of the remaining simple'// &
                                      ' templates '                   // &
                                      '(including the optional ones) '// &
                                      'the one that comes closest '   // &
                                      'to matching is: /cr/cr(3:) ', &
                                       1, CAUSE(2)      )
                        CALL SUFFIX ( TEMP(BDIAGS:EDIAGS), &
                                      1, CAUSE(2)      )
                        CALL SUFFIX ( '/cr/cr(-3:)', 0, CAUSE(2)      )

                     END IF

                  ELSE

                     CALL PREFIX ( '/cr/cr(-3:)',     1, CAUSE(2) )
                     CALL PREFIX ( SUBTMP,            0, CAUSE(2) )
                     CALL PREFIX ( 'Although I had matched a required'// &
                                   ' number of expressions in the '   // &
                                   'group below, I had not yet match' // &
                                   'ed the explicitely required '     // &
                                   'expressions that appear prior to' // &
                                   ' the META/2 "@options" directive '// &
                                   'in the group shown here. '        // &
                                   './cr/cr(3:) ',   1, CAUSE(2) )

                     IF ( NCPOS (STRING, ' ', SBEG) .NE. 0 ) THEN

                        CALL SUFFIX ( '/cr/crOf the remaining simple' // &
                                      ' templates, '                  // &
                                      'the one that comes closest '   // &
                                      'to matching is: /cr/cr(3:) ', &
                                                           1, CAUSE(2) )
                        CALL SUFFIX ( TEMP(BDIAGS:EDIAGS), 1, CAUSE(2) )
                        CALL SUFFIX ( '/cr/cr(-3:)',       0, CAUSE(2) )

                     END IF

                  END IF

               END IF

               M2CODE = BCODE
               MORE   = .FALSE.

            ELSE IF ( GMATCH .LT. FEWEST ) THEN

               SCORE = SCORE + BSCORE

!
!              If diagnostics are requested then see what went wrong
!              with the best fitting simple template.
!
               IF ( REASON ) THEN

                  BS = BDIAGS
                  ES = EDIAGS

                  CALL M2MTCH ( TEMP(BS:ES), 1,       SWORDS, &
                                STRING,      SBEG,    REASON, CUTOFF, &
                                T1CODE,      TSCORE,  CAUSE           )

                  CAUSE(2) = CAUSE(1)

                  B        = BEGOFG - 1
                  E        = ENDOFG + 2

                  CALL CMPRSS ( ' ', 1, TEMP(B:E), SUBTMP )

                  IF ( INDEX( SUBTMP, ' | ' ) .NE. 0 ) THEN

                     CALL PREFIX ( '''./cr/cr(-3:)', &
                                   1,         CAUSE (2)               )
                     CALL PREFIX ( TEMP ( BDIAGS:EDIAGS ), &
                                   0,         CAUSE (2)               )
                     CALL PREFIX ( 'I was trying to match part of '   // &
                                   'the input string with one of '    // &
                                   'the expressions listed here:'     // &
                                   '/cr/cr(3:) '                      // &
                                   SUBTMP(1:RTRIM(SUBTMP))           // &
                                   './cr/cr(-3:) The expression '    // &
                                   'that came '// &
                                   'closest was: /cr/cr(3:)''', &
                                   0,         CAUSE (2)               )
                  ELSE

                     CALL PREFIX ( '''./cr/cr(-3:)', &
                                   1,         CAUSE (2)                )
                     CALL PREFIX ( TEMP ( BDIAGS:EDIAGS ), &
                                   0,         CAUSE (2)                )
                     CALL PREFIX ( 'I was trying to match part of '   // &
                                   'the input string with the '       // &
                                   'expression: /cr/cr(3:) ''', &
                                    0,         CAUSE (2)              )

                  END IF

               END IF

               M2CODE = BCODE
               MORE   = .FALSE.

            END IF
!
         ELSE IF  ( SIMPLE ) THEN

            TEND   = MIN (  UPTO ( TEMP, ' @then', TBEG       ), &
                            UPTO ( TEMP, '){ ',    TBEG       ) &
                         ) + 1

            CALL FNDPTK  (  TEMP,  ' ',  TEND, BLSTWD, ELSTWD )

            TEND = ELSTWD
!
!           See if the simple template ends with a variable template.
!           If it does, find out what the possible terminating words
!           are.
!
            A = 0
            B = 0

            CALL M2BEGR ( TEMP, BLSTWD, ELSTWD, A, B )

            CLSTWD = MIN ( ELSTWD, BLSTWD + 8 )
            VTEMPL =      A .NE. B &
                   .OR. (.NOT. M2KEYW        ( TEMP(BLSTWD:ELSTWD) ) &
                         .AND.'@calendar' .EQ. TEMP(BLSTWD:CLSTWD) )

            IF ( VTEMPL ) THEN

!
!              There is a variable length template, the keywords
!              that might terminate this template are given
!              in TERMS up to the first occurance of a '}'.
!
               I = LSTLTI( ELSTWD, CARDI(INDXES), INDXES(1) ) + 1
               J = 0

!
!              Just load keywords onto the list until we hit a '}'
!              (We are guarenteed that this will happen, because
!              we put a '}' on the end of the list at the beginning
!              of this routine.)
!
               DO WHILE ( TERMS(I) .NE. '}' )

                  J          = J + 1
                  CALL M2TRIM ( TERMS(I), KEYWDS(J) )
                  I          = I + 1

               END DO

               CALL SCARDC ( J , KEYWDS )

            ELSE

               CALL SCARDC ( 0,     KEYWDS )

            END IF

!
!           Check the current template with M2MTCH.
!
               CALL M2TCLR
               CALL M2MTCH ( TEMP(TBEG:TEND), 1,       KEYWDS, &
                             STRING,          SBEG,    .FALSE., CUTOFF, &
                             T1CODE,          TSCORE,  CAUSE           )

!
!           If the attempt at a match succeeded ...
!
            IF ( T1CODE .EQ. 0 ) THEN

               SCORE  = SCORE  + TSCORE
               TBEG   = TEND   + 1
               CALL     M2KEEP

            ELSE

               CALL M2MTCH ( TEMP(TBEG:TEND), 1,       KEYWDS, &
                             STRING,          SBEG,    REASON, CUTOFF, &
                             T1CODE,          TSCORE,  CAUSE           )

               SCORE  = SCORE + TSCORE
               M2CODE = T1CODE
               MORE   = .FALSE.

            END IF

         END IF

      END DO


!
!     If there were no THNWDS and there is stuff left in the string and
!     we haven't already noticed, we've got an error dude.
!
      IF (       ( CARDC(THNWDS) .EQ. 0                ) &
           .AND. ( SBEG          .LT. QLSTNB( STRING ) ) &
           .AND. ( M2CODE        .EQ. 0                ) ) THEN


!
!        Until we have evidence to justify looking for probable causes
!        of the current overage of input string, we assume that we
!        are not interested in offering conjectures about what the
!        problem is.  We'll just say there is extra stuff.
!
         REDIAG = .FALSE.

!
!        Now look for justification of fancier diagnostics.
!
!        Was the last thing we attempted to match part of a group
!        template?
!
         IF (       ( LAST   .EQ. 'GROUP' ) &
              .AND. ( GMATCH .LT.  MOST   ) ) THEN

!
!           We are going to see if one of the options of an ending group
!           template looks like it was the intention of the user.
!
            IF ( BCODE .LT. 100 ) THEN

!
!              We had a probable spelling error, set the flag to
!              diagnose the problem.
!
               REDIAG = .TRUE.

            ELSE

!
!              Look at what the score could have been for the
!              simple template that was the closest match.
!
               I      = 1
               J      = BDIAGS
               TSCORE = 0

               CALL FNDNWD ( TEMP, J, I, TMPJ )
               J = TMPJ

               DO WHILE ( ( I .NE. 0 ) .AND. ( I .LT. EDIAGS ) )

                  A = 1
                  B = 1

                  CALL M2BEGR (  TEMP, I, J, A, B )

                  IF ( M2KEYW( TEMP(I:J) ) ) THEN
                     TSCORE = TSCORE + 100
                  ELSE
                     TSCORE = TSCORE + A*15
                  END IF

                  CALL FNDNWD ( TEMP, J, I, TMPJ )
                  J = TMPJ

               END DO

!
!              If the score actually recorded made it at least a quarter
!              of the way, we will guess that this may have been the
!              root of the problem.
!
               REDIAG = BSCORE .GT. MAX( CUTOFF, TSCORE/4 )

            END IF
         END IF

!
!        If there was sufficient grounds to warrant second guessing,
!        run the best guess template through M2MTCH to get a diagnostic.
!
         IF ( REDIAG ) THEN

            IF ( REASON ) THEN

               BS = BDIAGS
               ES = EDIAGS

               CALL M2MTCH ( TEMP(BS:ES), 1,       KEYWDS, &
                             STRING,      SBEG,    REASON, CUTOFF, &
                             T1CODE,      TSCORE,  CAUSE           )

               CAUSE(2) = CAUSE(1)

               B = BEGOFG - 1
               E = ENDOFG + 2

               CALL CMPRSS ( ' ', 1, TEMP(B:E), SUBTMP )

               IF ( INDEX( SUBTMP, ' | ' ) .NE. 0 ) THEN

                  CALL PREFIX ( '''./cr/cr(-3:)', &
                                1,         CAUSE(2)                 )
                  CALL PREFIX ( TEMP ( BDIAGS:EDIAGS ), &
                                0,         CAUSE(2)                 )
                  CALL PREFIX ( 'Extra words appear in the input ' // &
                                'string that are not part of a '   // &
                                'valid expression. I think you '   // &
                                'may have been trying to supply '  // &
                                'one of the optional expressions ' // &
                                'listed here:/cr/cr(3:) '          // &
                                SUBTMP(1:RTRIM(SUBTMP))            // &
                                '/cr/cr(-3:). '                    // &
                                'The expression that came '        // &
                                'closest was: /cr/cr(3:) ''', &
                                0,         CAUSE(2)                 )
               ELSE

                  CALL PREFIX ( '''./cr/cr(-3:)', &
                                1,         CAUSE(2)                 )
                  CALL PREFIX ( TEMP ( BDIAGS:EDIAGS ), &
                                0,         CAUSE(2)                 )
                  CALL PREFIX ( 'Extra words appear in the input ' // &
                                'string that are not part of a '   // &
                                'valid expression. I think you '   // &
                                'may have been trying to supply '  // &
                                'the optional expression:/cr/cr(3:)''', &
                                 0,         CAUSE(2)                 )

               END IF

            END IF

!
!           Whatever error code we got back, add 10000 so that this
!           routine will have its stamp on it to indicate we are second
!           level guessing at what went wrong.
!
            M2CODE = 10000 + T1CODE

         ELSE

!
!           Sorry, we couldn't guess why there was extra stuff in the
!           command.  Maybe just happy fingers.  Anyway, just say there
!           was extra stuff and hit the road.
!
            IF ( REASON ) THEN
               CAUSE(1) = 'The input string contains extra words that'// &
                          ' are not recognized as part of a valid '   // &
                          'command.'

               CALL M2MARK ( STRING, SBEG, QLSTNB(STRING), CAUSE(1) )
            END IF

            M2CODE = 10200

         END IF

      END IF

      RETURN
      END
