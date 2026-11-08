!$Procedure      ZZEKSCAN ( EK, scan query )

      SUBROUTINE ZZEKSCAN (  QUERY,  MAXNTK, MAXNUM, NTOKEN, TOKENS, &
                             LXBEGS, LXENDS, VALUES, NUMVLS, CHRBUF, &
                             CHBEGS, CHENDS, SCNERR, ERRMSG         )

! $ Abstract
!
!     Scan tokens in an EK query.
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
!     EK
!
! $ Keywords
!
!     EK
!     PARSE
!
! $ Declarations


      INCLUDE 'ekkeyw.inc'
      INCLUDE 'ektokn.inc'
      INCLUDE 'ekqlimit.inc'

      CHARACTER*(*)         QUERY
      INTEGER               MAXNTK
      INTEGER               MAXNUM
      INTEGER               NTOKEN
      INTEGER               TOKENS ( * )
      INTEGER               LXBEGS ( * )
      INTEGER               LXENDS ( * )
      INTEGER               VALUES ( * )
      DOUBLE PRECISION      NUMVLS ( * )
      CHARACTER*(*)         CHRBUF
      INTEGER               CHBEGS ( * )
      INTEGER               CHENDS ( * )
      LOGICAL               SCNERR
      CHARACTER*(*)         ERRMSG

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     QUERY      I   Query specifying events to be found.
!     MAXNTK     I   Maximum number of tokens to return.
!     MAXNUM     I   Maximum number of numeric tokens allowed.
!     NTOKEN     O   Number of tokens returned.
!     TOKENS     O   Code numbers of identified tokens.
!     LXBEGS,
!     LXENDS     O   Start and end locations of lexemes in query.
!     VALUES     O   Token values or value pointers, as needed.
!     NUMVLS     O   Buffer containing values of numeric tokens.
!     CHRBUF     O   Buffer containing string tokens and identifiers.
!     CHBEGS     O   Begin locations of string tokens in CHRBUF.
!     CHENDS     O   End locations of string tokens in CHRBUF.
!     SCNERR     O   Flag indicating whether query parsed correctly.
!     ERRMSG     O   Scan error description.
!
! $ Detailed_Input
!
!     QUERY          is character string containing an EK query.  See
!                    the header of the subroutine EKFIND for a
!                    detailed description of the EK query language.
!
!     MAXNTK         is the maximum number of tokens that may occur
!                    in QUERY.
!
!     MAXNUM         is the maximum number of tokens representing
!                    numeric values that may occur in QUERY.
!
! $ Detailed_Output
!
!     NTOKEN         is the number of tokens found in the input QUERY.
!                    This number will be less than or equal to MAXNTK.
!
!     TOKENS         is an array of codes for the tokens found in QUERY.
!                    The parameter values for these codes are not part
!                    of the EKSCAN specification; however, these values
!                    must be kept consistent with those used by EKPARS.
!                    The caller of EKSCAN should declare TOKENS with
!                    dimension MAXNTK.
!
!     LXBEGS,
!     LXENDS         are, respectively, arrays of begin and end pointers
!                    for the lexemes occurring in QUERY. Lexemes are
!                    the strings in QUERY that correspond to tokens. For
!                    example, '4.9D0' and '3' are both lexemes that map
!                    to the token <number>.
!
!     VALUES         is an array of token values.  The Ith element of
!                    VALUES refers to the Ith token.
!
!                    If the Ith token is a number, the Ith element of
!                    VALUES is a pointer into the NUMVLS array where
!                    the value of the number is stored.  The Ith token
!                    code indicates whether the number was a signed
!                    integer or d.p. number.
!
!                    If the Ith token is a keyword, the Ith element of
!                    VALUES is the code for that keyword.
!
!                    If the Ith token is a quoted string, the Ith
!                    element of VALUES is the common index in the arrays
!                    CHBEGS and CHENDS where the begin and end positions
!                    in CHRBUF of the parsed identifier are stored.
!                    Identifiers are converted to upper case when they
!                    are scanned.
!
!                    If the Ith token is an identifier, the Ith element
!                    of VALUES has the same role as in the case of a
!                    quoted string.
!
!                    If the Ith token is a special character, the Ith
!                    element of values is undefined; the value of
!                    TOKENS is the value of ICHAR() applied to the
!                    character.
!
!                    The caller of EKSCAN should declare VALUES with
!                    dimension MAXNTK.
!
!
!     NUMVLS         is an array of numeric values of parsed numeric
!                    tokens.  The caller of EKSCAN should declare
!                    NUMVLS with dimension at least MAXNUM.
!
!     CHRBUF         is a character string used to contain the values
!                    of literal string tokens and identifiers.  The
!                    value MAXQRY is guaranteed to be a safe length for
!                    CHRBUF, though the caller of EKSCAN can probably
!                    get away with less.
!
!                    The reason for the existence of CHRBUF is that
!                    the lexemes representing quoted strings may contain
!                    doubled quote characters representing embedded
!                    quotes; these characters are undoubled when the
!                    lexemes are parsed.  Hence the parsed quoted
!                    strings are not necessarily substrings of the
!                    original lexemes from which they are derived.
!
!     CHBEGS,
!     CHENDS         are, respectively, arrays of begin and end pointers
!                    for parsed quoted strings and identifiers stored in
!                    CHRBUF.
!
!     SCNERR         is a logical flag which is set to .TRUE. if a
!                    scanning error is detected, and is set to .FALSE.
!                    otherwise.  If SCNERR is returned .TRUE., all
!                    outputs save ERRMSG are undefined.
!
!     ERRMSG         is an error message that describes the cause of
!                    a scanning error, if such an error is detected.
!                    When SCNERR is returned .FALSE., ERRMSG is set to
!                    blank.
!
! $ Parameters
!
!     See the include files.
!
! $ Exceptions
!
!     Error free.
!
!     This routine set the error flag ERROR to .TRUE. and returns an
!     error message in the event that a syntax error precludes scanning
!     the input string.  Note that incorrect queries may scan
!     successfully; it is the responsibility of the caller to ensure
!     syntactic and semantic correctness of queries.
!
!     The following error messages are returned by this routine:
!
!        'No table list preceded first keyword.'
!        'Column clause and WHERE keyword are missing.'
!        'WHERE keyword is missing.'
!        'Too many tokens in query; max allowed is #.'
!        'Column list was empty.'
!        'Quoted string in positions #:# is empty.'
!        'Unexpected token found in query: #'
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine breaks up a valid EK query into an array of
!     individual tokens in order to facilitate parsing.
!
!     Time values and quoted strings are treated as single tokens.
!
! $ Examples
!
!     1)  Examples of strings containing lexically valid queries
!         are:
!
!            FROM TIME * WHERE TIME LT 'MO SCLK 15328997.121'
!
!            from time, event_type where event_type eq "MOC_EVENT"
!
!            FROM * WHERE TIME GE "1994 MAR 1" AND  IDCODE EQ -94030
!
!            FROM  *   WHERE
!                  TIME        GE   "1994 MAR 1"
!            AND   TIME        LE   '1-MAR-1994 18:4:1'
!            AND   EVENT_TYPE  LIKE '*PMIRR*'
!
!            FROM * WHERE TIME LT "MO SCLK 15328997.121" ORDER BY TIME
!
!            from col_1 col_2 col_3 where time lt '2010'
!
!            from col_1 col_2 col_3
!
!            from *
!
!            from * order by event_type
!
!         For a query to be semantically valid, all of the column names
!         referenced in the query must be present in at least one
!         loaded E-kernel.
!
!
!     2)  Examples of lexically invalid queries are:
!
!            from time where time lt
!            1991 jan 1                         {time string is not
!                                                quoted}
!
!            from time * where time
!             .lt. 1991 jan 1                   {operator should be lt}
!
!
!            from event_type * where
!            event_type eq ""                   {quoted string is empty}
!
!            from event_type ^ where
!            event_type eq "cmd"                {unexpected token}
!
!            from column1 where
!            column1 eq  3c                     {invalid numeric token}
!
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.2, 02-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    SPICELIB Version 3.0.1, 22-OCT-1996 (NJB)
!
!        Corrected miscellaneous errors in the header.
!
!-    SPICELIB Version 3.0.0, 14-NOV-1995 (NJB)
!
!        Complete re-write for architecture 3.
!
!-&


! $ Index_Entries
!
!     scan EK query
!     find tokens in EK query
!
!-&


! $ Revisions
!
!-    Beta Version 3.0.0, 14-NOV-1995 (NJB)
!
!        Complete re-write for architecture 3.
!
!-&

!
!     SPICELIB functions
!
      INTEGER               BSRCHC
      INTEGER               FRSTPC
      INTEGER               ISRCHC
      INTEGER               RTRIM

      LOGICAL               BEINT
      LOGICAL               RETURN


!
!     Local parameters
!
      CHARACTER*(*)         DQUOTE
      PARAMETER           ( DQUOTE  = '"' )

      CHARACTER*(*)         SQUOTE
      PARAMETER           ( SQUOTE  = '''' )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE =   80 )

      INTEGER               NXTTOK
      PARAMETER           ( NXTTOK = 1 )

      INTEGER               NEWTOK
      PARAMETER           ( NEWTOK = NXTTOK + 1 )

      INTEGER               TERM
      PARAMETER           ( TERM   = NEWTOK + 1 )

      INTEGER               QSTR
      PARAMETER           ( QSTR   = TERM   + 1 )

      INTEGER               DOT
      PARAMETER           ( DOT    = QSTR   + 1 )

      INTEGER               NUMBER
      PARAMETER           ( NUMBER = DOT    + 1 )

      INTEGER               IDENT
      PARAMETER           ( IDENT  = NUMBER + 1 )

      INTEGER               SPCIAL
      PARAMETER           ( SPCIAL = IDENT + 1 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MXSPEC
      PARAMETER           ( MXSPEC = 512 )

!
!     Number of tokens made up of special characters:
!
      INTEGER               NSPEC
      PARAMETER           ( NSPEC  = 13 )

!
!     Max length of any such token:
!
      INTEGER               SPCLEN
      PARAMETER           ( SPCLEN = 2 )

!
!     Local variables
!
      CHARACTER*(1)         CHR
      CHARACTER*(SPCLEN)    SPCSTR ( NSPEC )
      CHARACTER*(LNSIZE)    HDCHRS
      CHARACTER*(KWLEN)     KEYWDS ( NKEYWD )
      CHARACTER*(LNSIZE)    TLCHRS
      CHARACTER*(MAXQRY)    TQUERY

      INTEGER               CHCARD
      INTEGER               CPTR
      INTEGER               I
      INTEGER               IDSPEC ( LBCELL : MXSPEC )
      INTEGER               J
      INTEGER               KWVALS ( NKEYWD )
      INTEGER               L
      INTEGER               LAST
      INTEGER               LENGTH
      INTEGER               NCHARS
      INTEGER               NNUMS
      INTEGER               NSTRS
      INTEGER               PTR
      INTEGER               ROOM
      INTEGER               STATE
      INTEGER               SPCTOK ( NSPEC )
      INTEGER               SPCVAL ( NSPEC )

      LOGICAL               PASS1

!
!     Statement Functions
!
      LOGICAL               ISALPH
      LOGICAL               ISDIGT
      LOGICAL               ISDOT
      LOGICAL               ISSIGN
      LOGICAL               ISNUM
      LOGICAL               ISQUOT


!
!     Saved variables
!
      SAVE

!
!     Initial values
!

!
!     These keyword declarations must be made in alphabetical order!
!
      DATA  (  KEYWDS(I), KWVALS(I), I = 1, 10 )      / &
                 'ALL',          KWALL, &
                 'AND',          KWAND, &
                 'ASC ',         KWASND, &
                 'AVG ',         KWAVG, &
                 'BETWEEN',      KWBETW, &
                 'BY',           KWBY, &
                 'COUNT',        KWCNT, &
                 'DESC',         KWDSND, &
                 'DISTINCT',     KWDSTN, &
                 'EQ',           KWEQ                 /

      DATA  (  KEYWDS(I), KWVALS(I), I = 11, 20 )     / &
                 'FROM',         KWFROM, &
                 'GE',           KWGE, &
                 'GROUP',        KWGRP, &
                 'GT',           KWGT, &
                 'HAVING',       KWHAV, &
                 'IS',           KWIS, &
                 'LE',           KWLE, &
                 'LIKE',         KWLIKE, &
                 'LT',           KWLT, &
                 'MAX',          KWMAX                /

      DATA  (  KEYWDS(I), KWVALS(I), I = 21, NKEYWD ) / &
                 'MIN',          KWMIN, &
                 'NE',           KWNE, &
                 'NOT',          KWNOT, &
                 'NULL',         KWNULL, &
                 'OR',           KWOR, &
                 'ORDER',        KWORDR, &
                 'SELECT',       KWSEL, &
                 'SUM',          KWSUM, &
                 'WHERE',        KWWHER               /

!
!     The following tokens are sequences of special characters.  Some
!     of these are synonyms for keywords; some have other meanings.  In
!     this data statement, the longer sequences must precede the shorter
!     ones, in order for the matching algorithm to work properly.
!
      DATA  (  SPCSTR(I), SPCTOK(I), SPCVAL(I), I = 1, NSPEC )  / &
                 '!=',      TKKEY,    KWNE, &
                 '^=',      TKKEY,    KWNE, &
                 '<>',      TKKEY,    KWNE, &
                 '<=',      TKKEY,    KWLE, &
                 '>=',      TKKEY,    KWGE, &
                 '<',       TKKEY,    KWLT, &
                 '>',       TKKEY,    KWGT, &
                 '=',       TKKEY,    KWEQ, &
                 '(',       TKLPAR,   0, &
                 ')',       TKRPAR,   0, &
                 ',',       TKCOMA,   0, &
                 '.',       TKDOT,    0, &
                 '*',       TKSTAR,   0                         /


      DATA        PASS1              /  .TRUE.  /


!
!     Statement Function Definitions
!
      ISALPH ( CHR )  =       (      ( ICHAR(CHR) .GE. ICHAR('A') ) &
                               .AND. ( ICHAR(CHR) .LE. ICHAR('Z') ) ) &
                        .OR.  (      ( ICHAR(CHR) .GE. ICHAR('a') ) &
                               .AND. ( ICHAR(CHR) .LE. ICHAR('z') ) )


      ISDIGT ( CHR )  =       (      ( ICHAR(CHR) .GE. ICHAR('0') ) &
                               .AND. ( ICHAR(CHR) .LE. ICHAR('9') ) )


      ISDOT  ( CHR )  =     ( CHR .EQ. '.' )


      ISSIGN ( CHR )  =     ( CHR .EQ. '+' )    .OR. ( CHR .EQ. '-' )


      ISNUM  ( CHR )  =       ISDIGT( CHR )     .OR.   ISSIGN ( CHR ) &
                                                .OR.  ( CHR .EQ. '.' )


      ISQUOT ( CHR )  =     ( CHR .EQ. SQUOTE ) .OR. ( CHR .EQ. DQUOTE )


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKSCAN' )
      END IF

!
!     The first time through, set up our identifier character set.
!
      IF ( PASS1 ) THEN
!
!        Each identifier must start with a letter (of either case).
!        The subsequent characters must be letters, numbers, dollar
!        signs or underscores.
!
         HDCHRS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' // &
                  'abcdefghijklmnopqrstuvwxyz'

         TLCHRS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' // &
                  'abcdefghijklmnopqrstuvwxyz' // &
                  '0123456789'                 // &
                  '$_'


         CALL SSIZEI ( MXSPEC,         IDSPEC )
         CALL LXCSID ( HDCHRS, TLCHRS, IDSPEC )

         PASS1  =  .FALSE.

      END IF

!
!     We'll work with a local copy of the query.
!
      L       =  RTRIM ( QUERY )
      TQUERY  =  QUERY(:L)

!
!     Initialize pointers and counts.
!
      CPTR   =  1
      NNUMS  =  0
      NSTRS  =  0
      CHCARD =  0
      NTOKEN =  0

!
!     Start out in the token search state.
!
      STATE  =  NXTTOK

      DO WHILE ( STATE .NE. TERM )


         IF ( STATE .EQ. NXTTOK ) THEN
!
!           In our initial state, we're looking for a new token.
!           We stop when we have enough characters to determine
!           which kind of token we have, or if we run out of
!           characters.
!
!           Set our character pointer to the beginning of the next
!           token.
!
            IF ( NTOKEN .GT. 0 ) THEN
              CPTR =  LXENDS(NTOKEN) + 1
            END IF


            IF ( CPTR .GT. L ) THEN

               STATE  =  TERM

            ELSE


               DO WHILE (       ( TQUERY(CPTR:CPTR) .EQ. ' ' ) &
                          .AND. ( CPTR              .LT. L   )  )

                  CPTR  =  CPTR + 1
               END DO


               IF ( TQUERY(CPTR:CPTR) .EQ. ' ' ) THEN
!
!                 We're out of non-blank characters to look at.
!
                  STATE  =  TERM

               ELSE

                  CHR    =  TQUERY(CPTR:CPTR)
                  STATE  =  NEWTOK

               END IF


            END IF
!
!           STATE is in the set {NEWTOK, TERM}.
!


         ELSE IF ( STATE .EQ. NEWTOK ) THEN
!
!           If we got this far, we have the initial character of
!           something that could be a valid token.  We test for
!
!              - quoted strings
!              - numbers
!              - identifiers
!              - special symbols
!
!           in that order.  Of course, we must have room in our output
!           arrays for the token.
!
            IF ( NTOKEN .EQ. MAXNTK ) THEN

               ERRMSG  =  'Maximum allowed number of tokens is #; at '// &
                          'least # tokens are present in QUERY.'
               CALL REPMI ( ERRMSG, '#', MAXNTK,   ERRMSG )
               CALL REPMI ( ERRMSG, '#', MAXNTK+1, ERRMSG )

               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN

            END IF


            IF ( ISQUOT(CHR) ) THEN

               STATE = QSTR

            ELSE IF ( ISDOT(CHR) ) THEN

               STATE = DOT

            ELSE IF ( ISNUM(CHR) ) THEN

               STATE = NUMBER

            ELSE IF ( ISALPH(CHR) ) THEN

               STATE = IDENT

            ELSE

               STATE = SPCIAL

            END IF

!
!           At this point, the next value of STATE has been determined.
!           STATE is in the set
!
!              {QSTR, NUMBER, IDENT, SPCIAL}
!


         ELSE IF ( STATE .EQ. QSTR ) THEN
!
!           Look for a quoted string starting at location CPTR.
!           Use the current character as the quote character.
!
            CALL LXQSTR ( TQUERY, CHR, CPTR, LAST, NCHARS )

            IF ( NCHARS .EQ. 0 ) THEN

               ERRMSG = 'Invalid quoted string at location #.'
               CALL REPMI ( ERRMSG, '#', CPTR, ERRMSG )

               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN

            END IF

!
!           We've located a quoted string lexeme.  Parse the lexeme
!           and obtain the corresponding string value.  First make
!           sure we have enough room for the parsed string.
!
            ROOM  =  LEN(CHRBUF)-CHCARD

            IF ( NCHARS .GT. ROOM )  THEN

               ERRMSG  =  'Insufficient space to store quoted '    // &
                          'string at location #; # chars needed; ' // &
                          'only # are available.'

               CALL REPMI  ( ERRMSG, '#', CPTR,    ERRMSG )
               CALL REPMI  ( ERRMSG, '#', NCHARS,  ERRMSG )
               CALL REPMI  ( ERRMSG, '#', ROOM,    ERRMSG )

               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN

            END IF


            CALL PARSQS ( TQUERY(CPTR:CPTR+NCHARS-1), &
                          CHR, &
                          CHRBUF(CHCARD+1:), &
                          LENGTH, &
                          SCNERR, &
                          ERRMSG, &
                          PTR              )


            IF ( SCNERR ) THEN

               CALL PREFIX ( '#', 2, ERRMSG )

               CALL REPMC  ( ERRMSG, &
                             '#', &
                             'Error occurred while parsing '    // &
                             'quoted string token at location #:', &
                             ERRMSG                        )

               CALL REPMI  ( ERRMSG, '#', CPTR, ERRMSG )

               SCNERR = .TRUE.

               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN

            END IF

!
!           We've found a valid quoted string.  Set our outputs.
!
            NTOKEN          =  NTOKEN + 1
            TOKENS(NTOKEN)  =  TKQSTR
            NSTRS           =  NSTRS  + 1
            VALUES(NTOKEN)  =  NSTRS
            CHBEGS(NSTRS)   =  CHCARD + 1
            CHENDS(NSTRS)   =  CHCARD + LENGTH
            CHCARD          =  CHENDS(NSTRS)
            LXBEGS(NTOKEN)  =  CPTR
            LXENDS(NTOKEN)  =  LAST

            STATE           =  NXTTOK

!
!           STATE is now NXTTOK.
!


         ELSE IF ( STATE .EQ. DOT ) THEN
!
!           The token begins with a period.  We could be looking at
!           a floating point number, or we could be looking at a
!           period in a compound identifier.
!
!           Look for a number starting at location CPTR.
!
            CALL LX4NUM ( TQUERY, CPTR, LAST, NCHARS )

            IF ( NCHARS .GT. 0 ) THEN
               STATE  =  NUMBER
            ELSE
               STATE  =  SPCIAL
            END IF

!
!           STATE has been set to NUMBER or SPCIAL.  CPTR and NTOKEN
!           remain unchanged.
!

         ELSE IF ( STATE .EQ. NUMBER ) THEN
!
!           Look for a number starting at location CPTR.
!
            CALL LX4NUM ( TQUERY, CPTR, LAST, NCHARS )


            IF ( NCHARS .EQ. 0 ) THEN

               ERRMSG = 'Invalid numeric token at location #.'
               CALL REPMI ( ERRMSG, '#', CPTR, ERRMSG )

               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN

            END IF

!
!           Parse the token, but only do so if there's enough
!           room to store the result.
!
            ROOM  =  MAXNUM - NNUMS

            IF ( ROOM .LT. 1 ) THEN

               ERRMSG  =  'Insufficient space to store value of ' // &
                          'number at location #; # elements are ' // &
                          'available in the NUMVLS array; # are ' // &
                          'required.'

               CALL REPMI  ( ERRMSG, '#', CPTR,     ERRMSG )
               CALL REPMI  ( ERRMSG, '#', MAXNUM,   ERRMSG )
               CALL REPMI  ( ERRMSG, '#', MAXNUM+1, ERRMSG )

               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN

            END IF


            CALL NPARSD ( TQUERY(CPTR:LAST), &
                          NUMVLS(NNUMS+1), &
                          ERRMSG, &
                          PTR                 )


            IF ( ERRMSG .NE. ' ' ) THEN
!
!              This check is done for safety; by construction, we
!              should always have a valid number if LX4NUM
!              thinks we have a valid number, so in fact ERRMSG
!              should always be blank.
!

               CALL PREFIX ( '#', 2, ERRMSG )

               CALL REPMC  ( ERRMSG, &
                             '#', &
                             'Error found in numeric ' // &
                             'token at location #:', &
                             ERRMSG                        )

               CALL REPMI  ( ERRMSG, '#', CPTR+PTR-1, ERRMSG )

               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN

            END IF

!
!           We found a valid numeric token.  We distinguish
!           between integers and d.p. numbers; set the token
!           to the most restrictive category possible.
!
            NTOKEN  =  NTOKEN + 1

            IF (   BEINT( TQUERY(CPTR:LAST) )    ) THEN
               TOKENS(NTOKEN) =  TKINT
            ELSE
               TOKENS(NTOKEN) =  TKDP
            END IF

!
!           Set the rest of our outputs.
!
            NNUMS           =  NNUMS  + 1
            VALUES(NTOKEN)  =  NNUMS
            LXBEGS(NTOKEN)  =  CPTR
            LXENDS(NTOKEN)  =  LAST

            STATE           =  NXTTOK

!
!           STATE is now NXTTOK.
!


         ELSE IF ( STATE .EQ. IDENT ) THEN
!
!           Look for an identifier starting at location CPTR.
!
            CALL LXIDNT ( IDSPEC, TQUERY, CPTR, LAST, NCHARS )


            IF ( NCHARS .EQ. 0 ) THEN
!
!              This check is done for safety; by construction, we
!              should always have a valid identifier of at least one
!              character if we get to the IDENT state, so in fact
!              NCHARS should never equal zero.
!
               ERRMSG = 'Invalid identifier at location #.'
               CALL REPMI ( ERRMSG, '#', CPTR, ERRMSG )

               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN

            END IF

!
!           We've located an identifier lexeme.  Make sure we have
!           enough room for the string.
!
            ROOM  =  LEN(CHRBUF)-CHCARD

            IF ( NCHARS .GT. ROOM )  THEN

               ERRMSG  =  'Insufficient space to store identifier '// &
                          'string at location #; # chars needed; ' // &
                          'only # are available.'

               CALL REPMI  ( ERRMSG, '#', CPTR,   ERRMSG )
               CALL REPMI  ( ERRMSG, '#', NCHARS, ERRMSG )
               CALL REPMI  ( ERRMSG, '#', ROOM,   ERRMSG )

               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN

            END IF

!
!           We've found a valid identifier or keyword.  Set our
!           outputs.  Convert the string to upper case.
!
            NTOKEN =  NTOKEN + 1

            CALL UCASE (  TQUERY( CPTR:LAST ), &
                          CHRBUF( CHCARD+1:CHCARD+NCHARS )  )

            I      =  BSRCHC ( CHRBUF( CHCARD+1 : CHCARD+NCHARS ), &
                               NKEYWD, &
                               KEYWDS                            )


            IF ( I .GT. 0 ) THEN
!
!              It's a keyword.
!
               TOKENS(NTOKEN)  =  TKKEY
               VALUES(NTOKEN)  =  KWVALS(I)
               LXBEGS(NTOKEN)  =  CPTR
               LXENDS(NTOKEN)  =  LAST

               STATE           =  NXTTOK

            ELSE
!
!              It's an identifier.
!
               NSTRS           =  NSTRS  + 1
               CHBEGS(NSTRS)   =  CHCARD + 1
               CHENDS(NSTRS)   =  CHCARD + NCHARS
               CHCARD          =  CHENDS(NSTRS)
               TOKENS(NTOKEN)  =  TKID
               VALUES(NTOKEN)  =  NSTRS
               LXBEGS(NTOKEN)  =  CPTR
               LXENDS(NTOKEN)  =  LAST

               STATE           =  NXTTOK

!
!              We finished scanning an identifier.
!
!              STATE is set to NXTTOK.
!
            END IF
!
!           We scanned a keyword or an identifier.
!
!           STATE is set to NXTTOK.
!


         ELSE IF ( STATE .EQ. SPCIAL ) THEN
!
!           Look for a valid token starting with a special character at
!           location CPTR. We attempt to match the longest possible
!           special token.
!
            I  =  MIN ( SPCLEN,   L - CPTR + 1 )
            J  =  0

            DO WHILE (  ( I .GE. 1 ) .AND. ( J .EQ. 0 ) )

               LAST =  CPTR + I - 1
               J    =  ISRCHC ( TQUERY(CPTR:LAST), NSPEC, SPCSTR )

               IF ( J .EQ. 0 ) THEN
                  I  =  I - 1
               END IF

            END DO


            IF ( J .GT. 0 ) THEN
!
!              We've identified a valid token.
!
               NTOKEN =  NTOKEN + 1

               TOKENS(NTOKEN)  =  SPCTOK(J)
               VALUES(NTOKEN)  =  SPCVAL(J)
               LXBEGS(NTOKEN)  =  CPTR
               LXENDS(NTOKEN)  =  CPTR - 1 + RTRIM( SPCSTR(J) )

               STATE           =  NXTTOK

            ELSE

               ERRMSG = 'Invalid character found at location #. '
               CALL REPMI  ( ERRMSG, '#', CPTR, ERRMSG )
!
!              If the offending character is printable, include it
!              in the error message.  Otherwise, include the integer
!              code for the character.
!
               IF ( FRSTPC(CHR) .GT. 0 ) THEN

                  CALL SUFFIX ( '<character> = ''#''',   2, ERRMSG )
                  CALL REPMC  ( ERRMSG, '#', CHR,           ERRMSG )

               ELSE

                  CALL SUFFIX ( 'ICHAR(<character>) = #', 2, ERRMSG )
                  CALL REPMI  ( ERRMSG, '#', ICHAR(CHR),     ERRMSG )

               END IF

               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN

            END IF
!
!           STATE is now NXTTOK.
!

         END IF

      END DO

!
!     If we got this far, we've found the tokens in the query.
!
      SCNERR  =  .FALSE.
      ERRMSG  =  ' '

      CALL CHKOUT ( 'ZZEKSCAN' )
      RETURN
      END
