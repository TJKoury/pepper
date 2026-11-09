!$Procedure M2GETD (META/2 --- select a named word, double precision )

      SUBROUTINE M2GETD ( NAME, STRING, FOUND, DP   )
      IMPLICIT NONE

! $ Abstract
!
!     Select the Nth substring associated with a matched, named META/2
!     template word and parse it as a double precision number.
!
! $ Required_Reading
!
!     META/2 --- a language specification language.
!
! $ Keywords
!
!     META/2
!     PARSING
!
! $ Declarations

      CHARACTER*(*)         NAME
      CHARACTER*(*)         STRING
      LOGICAL               FOUND
      DOUBLE PRECISION      DP

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NAME       I   the name of some matched META/2 template word.
!     STRING     I   the string that matched the META/2 template.
!     FOUND      O   returned TRUE if the request could be fulfilled.
!     DP         O   matching d.p. extracted and parsed from STRING.
!
! $ Detailed_Input
!
!     NAME       is the name of some named META/2 template word that
!                may have matched some portion of STRING.
!
!     STRING     is a string that successfully matched a META/2 template
!                containing the template word specified by NAME.
!
! $ Detailed_Output
!
!     FOUND      will be returned .TRUE. if the requested information
!                specified by NAME and STRING could be retrieved.
!                Otherwise it will be returned with a value of .FALSE.
!
!     DP         is the double precision number represented by the word
!                of STRING that was the first match with the NAMEd
!                META/2 template word.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the portion of STRING extracted is NOT a word, the error
!        'META/2(CORRUPTEDINPUTSTRING)' will be signalled.
!
!     2) If the portion of STRING extracted cannot be parsed as a
!        a double precision number, the error 'META/2(CORRUPTEDNUMBER)'
!         will be signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Users of META/2 need not only to be sure that strings match
!     anticipated syntax of the language they design, but they also
!     need to be able to extract the meaning of syntactically correct
!     commands or statements.  The routines
!
!        M2GETC  --- get a character string word
!        M2GETI  --- get and parse an integer
!        M2GETD  --- get and parse a double precision number
!        M2GETA  --- get all that matched
!        M2SELC  --- select the n'th character string word
!        M2SELI  --- select and parse the n'th integer
!        M2SELD  --- select and parse the n'th double precision number
!
!     exist to aid in the extraction of meaning from syntactically
!     correct strings.
!
!     To make use of this feature, you must add parsing information
!     to the language you design.  To do this you simply "name" template
!     words by appending to the syntax portion of the word a name
!     of your choosing surrounded by the square brackets '[' and ']'.
!     For example you might have a language template of the form:
!
!         OPEN @word
!
!     That would open the contents of a text file.  This statement
!     my itself can be used to make sure that a statement has
!     a recognizable form.  However, if the program is to take any
!     action corresponding in an expected way to such a statement
!     entered into a program, you must eventually find out what
!     "@word" matched.  To do this simply append a name to @word,
!     in this case a good name might be:
!
!        OPEN @word[textfile]
!
!     (Note that case is significant for named template words).
!     The template word "@word" in this syntax specification now
!     has a name: "textfile".  Once it is recognized that a string
!     has matched a template, you can now easily find the name
!     of the text file that a user specified by making the call
!
!        CALL M2GETC ( 'textfile', STRING, FOUND, FILE )
!
!     where STRING is the original, unaltered string that matched
!     the template "OPEN @word[textfile]".
!
!     FOUND will indicate whether or not a match for a template
!     word having name "textfile" was recorded (in this case it
!     will return with a value of .TRUE) and FILE will contain
!     the word of string that matched "@word[textfile]".
!
!     For many uses of META/2 you can ignore the FOUND flag.  Often
!     you know from the fact that the string matched the template
!     FOUND must be .TRUE.  However, in some cases the syntax will
!     not force a match to exist.  For example a statement that
!     matches the template below my not have values for "to"
!     or "from".  One will be present, but one might be absent.
!
!        SET LIMITS (1:2){ FROM @calendar[from]
!                        | TO   @calendar[to]    }
!
!     In such cases, may want to assign default values to the strings
!     you use to retrieve the calendar strings corresponding to
!     "to" and "from".  Or you may wish to examine the FOUND flag
!     after making the calls below.
!
!        CALL M2GETT ( 'from', STRING, FOUNDF, FROM )
!        CALL M2GETT ( 'to',   STRING, FOUNDT, TO   )
!
!     Note that if the logical flag returned is false, the value of
!     the output (in these examples FROM and TO) will not change from
!     the values they had upon input.  In this way you may assign
!     defaults to items that might be missing from a matched
!     string.  However, you should probably note that you are
!     assigning the defaults with a comment.  Without doing this
!     your intent will likely be unclear to another person who might
!     eventually need to read and understand your code.
!
! $ Examples
!
!     Suppose that a string matched the META/2 template
!
!        FIND @name[window] SEPARATION
!
!                      (2:2){ OF   @int[body1]    @int[body2]
!                           | FROM @int[observer]    }
!
!                      (1:1){ LESS[less]      THAN @number[bound]
!                           | GREATER[greater THAN @number[bound] }
!
!                      (0:1){ WITHIN INTERVAL[restricted]
!                             FROM @calendar[from] TO @calendar[to]    }
!
!
!      Then to extract the information in the string the following
!      sequence of calls will suffice.
!
!            CALL M2GETC ( 'window',    STRING, FOUND,  WINDOW )
!            CALL M2GETI ( 'body1',     STRING, FOUND,  BODY1  )
!            CALL M2GETI ( 'body2',     STRING, FOUND,  BODY2  )
!            CALL M2GETI ( 'observer',  STRING, FOUND,  OBS    )
!            CALL M2GETD ( 'bound',     STRING, FOUND,  BOUND  )
!            CALL M2GETA ( 'from',      STRING, FOUND,  FROM   )
!            CALL M2GETA ( 'to',        STRING, FOUND,  TO     )
!
!            LESS   = M2XIST ( 'less'        )
!            GREATR = M2XIST ( 'greater'     )
!            RSTRCT = M2XIST ( 'restriction' )
!
!       C
!       C    If they were supplied parse the bounds of the search
!       C    interval, otherwise just use the next decade.
!       C
!            IF ( RSTRCT ) THEN
!
!               CALL UTC2ET ( FROM, LOWER )
!               CALL UTC2ET ( TO,   UPPER )
!
!            ELSE
!
!               CALL UTC2ET ( '1 JAN 1990',  LOWER )
!               CALL UTC2ET ( '1 JAN 2000',  UPPER )
!
!            END IF
!
!       C
!       C    If we want the separation to be less than BOUND use
!       C    the next block.  Otherwise we will look for separation
!       C    greater than BOUND
!       C
!            IF ( LESS ) THEN
!
!                 search for "less than" separation
!
!            ELSE
!
!                 search for "greater than" separation
!
!            END IF
!
!       C
!       C    Finally, store the result of our computation in the
!       C    specified window.
!       C
!            CALL STORE_WINDOW ( RESULTS, WINDOW )
!
! $ Restrictions
!
!     It is vital that the string that matched a META/2 template
!     not be altered prior to calling any of the extraction routines.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber     (JPL)
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
!-    Beta Version 1.0.0, 27-NOV-1991 (WLT)
!
!-&
!
! $ Index_Entry
!
!     Extract first number matching a named template word
!
!-&


!
!     Local variables
!
      CHARACTER*(80)        ERROR

      DOUBLE PRECISION      MYDP

      INTEGER               B
      INTEGER               E
      INTEGER               F
      INTEGER               L
      INTEGER               P
      INTEGER               PNTER



!
!     First look up the beginning and endings of the requested word.
!
      CALL M2VGET ( NAME, 1, FOUND, B, E )

      IF ( .NOT. FOUND ) THEN
         RETURN
      END IF

!
!     First make sure there is nothing pathological about the string
!     we are dealing with.
!
      P = B - 1
      F = E + 1
      L = LEN(STRING)

      IF ( P .GT. 0 ) THEN
         IF ( STRING(P:P) .NE. ' ' ) THEN
            CALL CHKIN  ( 'M2GETD' )
            CALL SETMSG ( 'The input string has been modified since ' // &
                          'it passed syntax validation in META/2. '   )
            CALL SIGERR ( 'META/2(CORRUPTEDINPUTSTRING)'              )
            CALL CHKOUT ( 'M2GETD' )
            RETURN
         END IF
      END IF

      IF ( F .LT. L ) THEN
         IF ( STRING(F:F) .NE. ' ' ) THEN
            CALL CHKIN  ( 'M2GETD' )
            CALL SETMSG ( 'The input string has been modified since ' // &
                          'it passed syntax validation in META/2. '   )
            CALL SIGERR ( 'META/2(CORRUPTEDINPUTSTRING)'              )
            CALL CHKOUT ( 'M2GETD' )
            RETURN
         END IF
      END IF


      IF (      ( STRING(B:B) .EQ. ' ' ) &
           .OR. ( STRING(E:E) .EQ. ' ' ) ) THEN
         CALL CHKIN  ( 'M2GETD' )
         CALL SETMSG ( 'The input string has been modified since '    // &
                       'it passed syntax validation in META/2. '      )
         CALL SIGERR ( 'META/2(CORRUPTEDINPUTSTRING)'                 )
         CALL CHKOUT ( 'M2GETD' )
         RETURN
      END IF



!
!     This is supposed to be an integer double precision number.
!     Parse it.
!
      CALL NPARSD ( STRING(B:E), MYDP,  ERROR, PNTER )

      IF ( ERROR .NE. ' ' ) THEN
         CALL CHKIN  ( 'M2GETD' )
         CALL SETMSG ( 'The item requested could not be parsed '      // &
                       'as an integer. a number.'                     )
         CALL SIGERR ( 'META/2(CORRUPTEDNUMBER)'                      )
         CALL CHKOUT ( 'M2GETD' )
         RETURN
      END IF

!
!     Now do the actual assignment
!
      DP   = MYDP

      RETURN
      END
