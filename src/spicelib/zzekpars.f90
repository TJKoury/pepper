!$Procedure      ZZEKPARS ( EK, parse tokenized EK query )

      SUBROUTINE ZZEKPARS (  QUERY,   NTOKEN,  LXBEGS,  LXENDS, &
                             TOKENS,  VALUES,  NUMVLS,  CHRBUF, &
                             CHBEGS,  CHENDS,  EQRYI,   EQRYC, &
                             EQRYD,   ERROR,   PRSERR          )

      IMPLICIT NONE

! $ Abstract
!
!     Parse an EK query that has been scanned and tokenized.
!     Represent the result as an encoded EK query.
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

      INCLUDE 'ektokn.inc'
      INCLUDE 'ekkeyw.inc'
      INCLUDE 'ekbool.inc'
      INCLUDE 'ektype.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'ekqlimit.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         QUERY
      INTEGER               NTOKEN
      INTEGER               LXBEGS ( * )
      INTEGER               LXENDS ( * )
      INTEGER               TOKENS ( * )
      INTEGER               VALUES ( * )
      DOUBLE PRECISION      NUMVLS ( * )
      CHARACTER*(*)         CHRBUF
      INTEGER               CHBEGS ( * )
      INTEGER               CHENDS ( * )
      INTEGER               EQRYI  ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      DOUBLE PRECISION      EQRYD  ( * )
      LOGICAL               ERROR
      CHARACTER*(*)         PRSERR

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     QUERY      I   Query in string form.
!     NTOKEN     I   Number of tokens in query.
!     LXBEGS,
!     LXENDS     I   Lexeme begin and end positions in QUERY.
!     TOKENS     I   Token codes.
!     VALUES     I   Values associated with tokens.
!     NUMVLS     I   Buffer containing numeric token values.
!     CHRBUF     I   Buffer containing string token values.
!     CHBEGS,
!     CHENDS     I   String token begin and end character positions.
!     EQRYI,
!     EQRYC,
!     EQRYD      O   Parsed query and string and number value buffers.
!     ERROR      O   Flag indicating whether query parsed correctly.
!     PRSERR     O   Parse error description.
!
! $ Detailed_Input
!
!     QUERY          is a string containing the original input query.
!                    QUERY is used only for creating error messages.
!
!     NTOKEN         is the number of tokens in the input query.
!
!     LXBEGS,
!     LXENDS         are lexeme begin and end pointers; the Ith
!                    lexeme in the query is
!
!                       QUERY ( LXBEGS(I) : LXENDS(I) )
!
!                    (Lexemes are strings that correspond to tokens
!                     in the language.)
!
!     TOKENS         is an array of token codes.  The Ith element of
!                    TOKENS represents the Ith token in the scanned
!                    query.
!
!     VALUES         is an array of values associated with tokens; the
!                    Ith element of VALUES corresponds to the Ith
!                    token.  Keywords, for example, are distinguished
!                    by codes in the VALUES array.  Literal numeric
!                    and string tokens use the VALUES array to point
!                    to elements of NUMVLS or CHBEGS and CHENDS,
!                    respectively.  Some tokens don't need to use
!                    VALUES, but to simplify indexing, each token gets
!                    an element of this array.
!
!     NUMVLS         is an array of double precision numbers used to
!                    store the values corresponding to literal numeric
!                    tokens.
!
!     CHRBUF         is a string used to store the values of literal
!                    string tokens.
!
!     CHBEGS,
!     CHENDS         are pairs of begin and end pointers into CHRBUF.
!                    These pointers delimit character values
!                    associated with literal string tokens.
!
!
! $ Detailed_Output
!
!     EQRYI,
!     EQRYC,
!     EQRYD          are the integer, character, and numeric portions
!                    of an encoded form of the input query.  The
!                    SELECT, FROM, WHERE, and ORDER BY clauses of the
!                    input query are all represented in this encoding.
!                    WHERE clause constraints have been normalized.
!
!                    Normalized queries have their constraints grouped
!                    into a disjunction of conjunctions of relational
!                    expressions, as symbolized below:
!
!                          ( <rel_exp_1_1> and <rel_exp_1_2> and ... )
!                       or ( <rel_exp_2_1> and <rel_exp_2_2> and ... )
!                                     .
!                                     .
!                                     .
!                       or ( <rel_exp_N_1> and <rel_exp_N_2> and ... )
!
!     ERROR,
!     PRSERR         are, respectively, a flag indicating whether the
!                    input query parsed correctly, and a message
!                    describing the parse error, if one occurred.  If
!                    no error occurred, ERROR is .FALSE. and PRSERR
!                    is blank.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  Most of the exceptions that can occur on a call to
!         ZZEKPARS are caused by errors in the input query.  ZZEKPARS
!         attempts to diagnose these via the output error flag and
!         error message, instead of signaling errors.  The
!         error messages that ZZEKPARS can return are listed below.
!         In the messages shown, the symbol # is used to designate
!         a marker for which a value can be substituted in an actual
!         message.
!
!
!            The BY keyword was not found following the
!            ORDER keyword.
!
!            Invalid keyword at location #.
!            Actual token was: #
!
!            Table or column name expected at location
!            #. Actual token was: #
!
!            Table name expected at location #.
!            Actual token was: #
!
!            Column name expected at location #.
!            Actual token was: #
!
!            Table alias, comma, or keyword expected at
!            location #. Actual token was: #
!
!            Comma or keyword expected at
!            location #. Actual token was: #
!
!            Comma expected at location #. Actual token was: #
!
!            PRSERR  =  More tokens were expected in query.
!
!            The keyword # was expected at location
!            #. Actual token was: #
!
!            Invalid token at location #. Token was: #
!
!            PRSERR  =  Number of tables in "FROM" clause exceeds
!            allowed maximum of #.
!
!            PRSERR  =  Number of order-by columns exceeds allowed
!            maximum of #.
!
!            PRSERR  =  Number of SELECT columns exceeds allowed
!            maximum of #.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine determines whether a query is syntactically correct;
!     it transforms correct queries into the EK system's encoded query
!     representation.
!
!     The encoded queries output by this routine are not ready for
!     execution; they still must undergo name resolution, time value
!     conversion, and semantic checking.  See EKFIND for an example of
!     the normal sequence of query processing.
!
! $ Examples
!
!     See the header of EKFIND for examples of valid and invalid
!     queries.
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
!-    SPICELIB Version 4.1.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    SPICELIB Version 4.1.0, 15-OCT-1996 (NJB)
!
!        Bug fix: default order sense was not encoded when ORDER-BY
!        clause was not the last clause of the query.
!
!-    SPICELIB Version 4.0.0, 17-NOV-1995 (NJB)
!
!        Complete re-write for architecture 3.
!
!-&


! $ Index_Entries
!
!     parse EK query
!
!-&


! $ Revisions
!
!-    SPICELIB Version 4.1.0, 15-OCT-1996 (NJB)
!
!        Bug fix: default order sense was not encoded when ORDER-BY
!        clause was not the last clause of the query.  The old algorithm
!        assumed that no clauses followed the ORDER-BY clause, which
!        at one time was a limitation of the EK query language.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               CARDI

      LOGICAL               FAILED
      LOGICAL               RETURN


!
!     Local parameters
!

!
!     State parameters
!
      INTEGER               FROM
      PARAMETER           ( FROM    =            0 )

      INTEGER               FRMTAB
      PARAMETER           ( FRMTAB  =   FROM   + 1 )

      INTEGER               FRMCNT
      PARAMETER           ( FRMCNT  =   FRMTAB + 1 )

      INTEGER               FRMALS
      PARAMETER           ( FRMALS  =   FRMCNT + 1 )

      INTEGER               WHERE
      PARAMETER           ( WHERE   =   FRMALS + 1 )

      INTEGER               ORDER
      PARAMETER           ( ORDER   =   WHERE  + 1 )

      INTEGER               ORDRBY
      PARAMETER           ( ORDRBY  =   ORDER  + 1 )

      INTEGER               ORDTAB
      PARAMETER           ( ORDTAB  =   ORDRBY + 1 )

      INTEGER               ORDNAM
      PARAMETER           ( ORDNAM  =   ORDTAB + 1 )

      INTEGER               ORDCOL
      PARAMETER           ( ORDCOL  =   ORDNAM + 1 )

      INTEGER               ORDSNS
      PARAMETER           ( ORDSNS  =   ORDCOL + 1 )

      INTEGER               SELKEY
      PARAMETER           ( SELKEY  =   ORDSNS + 1 )

      INTEGER               SELECT
      PARAMETER           ( SELECT  =   SELKEY + 1 )

      INTEGER               SELTAB
      PARAMETER           ( SELTAB  =   SELECT + 1 )

      INTEGER               SELNAM
      PARAMETER           ( SELNAM  =   SELTAB + 1 )

      INTEGER               SELCOL
      PARAMETER           ( SELCOL  =   SELNAM + 1 )

      INTEGER               TERM
      PARAMETER           ( TERM    =   SELCOL + 1 )

!
!     Other local parameters
!
      INTEGER               MAXREL
      PARAMETER           ( MAXREL =  200 )

      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN =   32 )

      INTEGER               KEYLEN
      PARAMETER           ( KEYLEN =   32 )

!
!     Local variables
!
      CHARACTER*(TYPLEN)    ERRTYP
      CHARACTER*(KEYLEN)    EXPKEY

      INTEGER               ALSDSC ( EQVDSZ )
      INTEGER               B
      INTEGER               COLDSC ( EQVDSZ )
      INTEGER               E
      INTEGER               I
      INTEGER               J
      INTEGER               L
      INTEGER               LXB
      INTEGER               LXE
      INTEGER               NAMDSC ( EQVDSZ )
      INTEGER               NCNSTR
      INTEGER               NORDER
      INTEGER               NSEL
      INTEGER               NTABS
      INTEGER               STATE
      INTEGER               TABDSC ( EQVDSZ )
      INTEGER               TOKEN
      INTEGER               TOKNUM
      INTEGER               VALDSC ( EQVDSZ )

      LOGICAL               FND

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKPARS' )
      END IF

!
!     Initialize the encoded query each time, for safety.
!
      CALL ZZEKQINI ( EQIMIN, MAXQNM, EQRYI, EQRYC, EQRYD )

      IF ( FAILED() ) THEN
         ERROR  =  .TRUE.
         PRSERR =  'SPICE(BUG):  encoded query init failed.'
         CALL CHKOUT ( 'ZZEKPARS' )
         RETURN
      END IF

!
!     The structure of a query is
!
!        <QUERY>                 =>    <SELECT clause> <FROM clause>
!                                      <WHERE clause> <ORDER BY clause>
!
!        <SELECT clause>         =>    SELECT <select list>
!
!        <select list>           =>    <column entry>
!                                    | <select list>, <column entry>
!
!        <column entry>          =>    <table name>.<column name>
!                                    | <column name>
!
!        <FROM clause>           =>    <table name list>
!
!        <table name list>       =>    <table entry>
!                                    | <table name list>, <table entry>
!
!        <table entry>           =>    <table name>
!                                    | <table name> <table alias>
!
!        <WHERE clause>          =>    WHERE <constraint expression>
!                                    | <NIL>
!
!        <ORDER BY clause>       =>    ORDER BY <order-by list>
!                                    | <NIL>
!
!        <order-by list>         =>    <order-by column entry>
!                                    | <order-by list>,
!                                      <order-by column entry>
!
!        <order-by column entry> =>    <column entry> <order>
!
!        <order>                 =>    ASC
!                                    | DESC
!                                    | <NIL>
!
!
!
!     We'll parse the clauses of the query in the following order:
!
!        FROM
!        WHERE     (if present)
!        ORDER BY  (if present)
!        SELECT
!
!
      CALL ZZEKTLOC ( TKKEY,   KWFROM,  NTOKEN,  TOKENS,  VALUES, &
                      TOKNUM,  FND                                )

      IF ( .NOT. FND ) THEN

         ERROR   =  .TRUE.
         ERRTYP  =  'FROM_NOT_FOUND'
         STATE   =   TERM

      ELSE

         STATE   =  FROM
         NTABS   =  0
         NSEL    =  0
         NCNSTR  =  0
         NORDER  =  0
         ERROR   =  .FALSE.
         PRSERR  =  ' '
         ERRTYP  =  ' '

      END IF


      DO WHILE ( STATE .NE. TERM )
!
!        Advance to the next token, if there is one.
!
         TOKNUM  =  TOKNUM + 1

         IF ( TOKNUM .GT. NTOKEN ) THEN
!
!           We're out of tokens.  Set the token value to indicate
!           `end of query'.
!
            TOKEN  =  TKEOQ

         ELSE

            TOKEN  =  TOKENS(TOKNUM)

         END IF

!
!        Perform semantic actions based on the state and current token.
!
         IF ( STATE .EQ. FROM ) THEN
!
!           We expect to see an identifier representing a table name.
!           No other tokens are allowed.
!
            IF ( TOKEN .EQ. TKID ) THEN
!
!              We've found a table name (as far as we can tell at
!              this point).  Make sure we haven't exceeded the limit
!              for table names; if not, add the appropriate information
!              to the encoded query.
!
               NTABS  =   NTABS  +  1


               IF ( NTABS .GT. MAXTAB ) THEN

                  ERROR   =  .TRUE.
                  ERRTYP  =  'TOO_MANY_TABLES'
                  STATE   =  TERM

               ELSE

                  I       =   VALUES(TOKNUM)
                  B       =   CHBEGS(I)
                  E       =   CHENDS(I)
                  L       =   E  -  B   +  1
                  LXB     =   LXBEGS(TOKNUM)
                  LXE     =   LXENDS(TOKNUM)


                  CALL ZZEKINQC ( CHRBUF( B : E ), &
                                  L, &
                                  LXB, &
                                  LXE, &
                                  EQRYI, &
                                  EQRYC, &
                                  TABDSC          )

!
!                 Append the table descriptor to the integer part of the
!                 query.
!
                  DO J = 1, EQVDSZ
                     CALL APPNDI ( TABDSC(J), EQRYI )
                  END DO

!
!                 Add a place-holder value descriptor to reserve
!                 space for an alias descriptor for this table.  If an
!                 actual alias is supplied, we'll update this
!                 descriptor.
!
                  CALL CLEARI ( EQVDSZ, ALSDSC )
                  ALSDSC(EQDTYP)  =  CHR

                  DO J = 1, EQVDSZ
                     CALL APPNDI ( ALSDSC(J), EQRYI )
                  END DO

!
!                 Update the table count in the encoded query.
!
                  CALL ZZEKWEQI ( 'NUM_TABLES', NTABS, EQRYI )


                  STATE  =  FRMTAB

               END IF


            ELSE IF ( TOKEN .EQ. TKEOQ ) THEN

               ERROR   =  .TRUE.
               ERRTYP  =  'MORE_TOKENS_EXP'
               STATE   =  TERM


            ELSE
!
!              We've got the wrong kind of token here.
!
               ERROR   =  .TRUE.
               ERRTYP  =  'TABLE_EXP'
               STATE   =  TERM

            END IF
!
!           State is a member of {FRMTAB, TERM}.
!

         ELSE IF ( STATE .EQ. FRMTAB ) THEN
!
!           We should see a comma, an alias, one of the SELECT,
!           WHERE or ORDER keywords, or the end of the query.
!
            IF ( TOKEN .EQ. TKEOQ ) THEN
!
!              We're out of tokens.  It's time to parse the
!              WHERE clause.
!
               STATE   =  WHERE


            ELSE IF ( TOKEN .EQ. TKCOMA ) THEN
!
!              It's time to look for another table name.
!
               STATE  =  FROM


            ELSE IF ( TOKEN .EQ. TKID ) THEN
!
!              We've got an alias.  Add this string to the encoded
!              query.
!
               I   =   VALUES(TOKNUM)
               B   =   CHBEGS(I)
               E   =   CHENDS(I)
               L   =   E  -  B   +  1
               LXB =   LXBEGS(TOKNUM)
               LXE =   LXENDS(TOKNUM)

               CALL ZZEKINQC ( CHRBUF( B : E ), &
                               L, &
                               LXB, &
                               LXE, &
                               EQRYI, &
                               EQRYC, &
                               ALSDSC                        )

!
!              Update the place-holder alias descriptor in the integer
!              part of the query.
!
               CALL MOVEI ( ALSDSC, &
                            EQVDSZ, &
                            EQRYI(  CARDI(EQRYI) - EQVDSZ + 1  )   )


               STATE  =  FRMALS


            ELSE IF (  TOKEN .EQ.  TKKEY  )  THEN
!
!              The last table name in the FROM clause is followed by
!              a keyword.  SELECT, WHERE and ORDER are the only valid
!              possibilities.
!
               IF (       ( VALUES(TOKNUM) .NE. KWWHER ) &
                    .AND. ( VALUES(TOKNUM) .NE. KWSEL  ) &
                    .AND. ( VALUES(TOKNUM) .NE. KWORDR )  ) THEN
!
!                 We've got a keyword we don't want here.
!
                  ERROR  =  .TRUE.
                  ERRTYP =  'BAD_KEYWORD'
                  STATE  =  TERM

               ELSE
!
!                 Parse the WHERE clause.
!
                  STATE  =  WHERE

               END IF


            ELSE
!
!              We've got the wrong kind of token altogether.
!
               ERROR  =  .TRUE.
               ERRTYP =  'ALIAS_EXP'
               STATE  =  TERM

            END IF

!
!           STATE is a member of {FROM, FRMALS, WHERE, TERM}.
!



         ELSE IF ( STATE .EQ. FRMALS ) THEN
!
!           We should see a comma, the SELECT, WHERE or ORDER
!           keywords, or the end of the query.
!
            IF ( TOKEN .EQ. TKEOQ ) THEN
!
!              We're out of tokens.  It's time to parse the
!              WHERE clause.
!
               STATE   =  WHERE


            ELSE IF ( TOKEN .EQ. TKCOMA ) THEN
!
!              It's time to look for another table name.
!
               STATE  =  FROM


            ELSE IF (  TOKEN .EQ.  TKKEY  )  THEN
!
!              The last table name in the FROM clause is followed by
!              a keyword.  SELECT, WHERE and ORDER are the only valid
!              possibilities.
!
               IF (       ( VALUES(TOKNUM) .NE. KWWHER ) &
                    .AND. ( VALUES(TOKNUM) .NE. KWSEL  ) &
                    .AND. ( VALUES(TOKNUM) .NE. KWORDR )  ) THEN
!
!                 We've got a keyword we don't want here.
!
                  ERROR  =  .TRUE.
                  ERRTYP =  'BAD_KEYWORD'
                  STATE  =  TERM

               ELSE
!
!                 Parse the WHERE clause.
!
                  STATE  =  WHERE

               END IF


            ELSE
!
!              We've got the wrong kind of token altogether.
!
               ERROR  =  .TRUE.
               ERRTYP =  'COMMA_OR_KEY_EXP'
               STATE  =  TERM

            END IF
!
!           STATE is a member of {FROM, WHERE, TERM}.
!


         ELSE IF ( STATE .EQ. SELKEY ) THEN
!
!           It's time to parse the SELECT clause.  We'll need to
!           locate the SELECT keyword.
!
            CALL ZZEKTLOC ( TKKEY,   KWSEL,  NTOKEN,  TOKENS,  VALUES, &
                            TOKNUM,  FND                               )

            IF ( .NOT. FND ) THEN

               ERROR   =  .TRUE.
               ERRTYP  =  'SELECT_NOT_FOUND'
               STATE   =   TERM

            ELSE

               STATE   =  SELECT

            END IF


         ELSE IF ( STATE .EQ. SELECT ) THEN
!
!           We must see either the * token, the ALL keyword,
!           or an identifier here.  The identifier may be a lone
!           column name, or it may be a column name qualified by a
!           table name or alias.
!
!           For the moment, we don't support the * or ALL options.
!
            IF ( TOKEN .EQ. TKID ) THEN
!
!              We've found a name (as far as we can tell at this point).
!              Make sure we haven't exceeded the limit for SELECT
!              column names; if not, store the name string in the
!              encoded query, and save the descriptor until we've
!              figured out whether we're looking at a column name or
!              table name.
!
               NSEL  =  NSEL  +  1

               IF ( NSEL.GT. MAXSEL ) THEN

                  ERROR   =  .TRUE.
                  ERRTYP  =  'TOO_MANY_SEL_COLS'
                  STATE   = TERM

               ELSE

                  I       =   VALUES(TOKNUM)
                  B       =   CHBEGS(I)
                  E       =   CHENDS(I)
                  L       =   E  -  B   +  1
                  LXB     =   LXBEGS(TOKNUM)
                  LXE     =   LXENDS(TOKNUM)


                  CALL ZZEKINQC ( CHRBUF( B : E ), &
                                  L, &
                                  LXB, &
                                  LXE, &
                                  EQRYI, &
                                  EQRYC, &
                                  NAMDSC          )

!
!                 Add a place-holder value descriptor to reserve
!                 space for a table descriptor for this name.  If it
!                 turns out that the current name is a table name, we'll
!                 update this descriptor.
!
                  CALL CLEARI ( EQVDSZ, VALDSC )
                  VALDSC(EQDTYP)  =  CHR

                  DO J = 1, EQVDSZ
                     CALL APPNDI ( VALDSC(J), EQRYI )
                  END DO

!
!                 Update the SELECT column count in the encoded query.
!
                  CALL ZZEKWEQI ( 'NUM_SELECT_COLS', NSEL, EQRYI )


                  STATE  =  SELNAM

               END IF


            ELSE IF ( TOKEN .EQ. TKEOQ ) THEN

               ERROR  =  .TRUE.
               ERRTYP =  'MORE_TOKENS_EXP'
               STATE  =  TERM


            ELSE
!
!              We've got the wrong kind of token here.
!
               ERROR  =  .TRUE.
               ERRTYP =  'TABLE_OR_COLUMN_EXP'
               STATE  =  TERM

            END IF
!
!           State is a member of {SELNAM, TERM}.
!


         ELSE IF ( STATE .EQ. SELNAM ) THEN
!
!           We've seen a SELECT column name, or else the name
!           of a table qualifying a SELECT column name.
!
            IF ( TOKEN .EQ. TKEOQ ) THEN
!
!              The name we picked up was an unqualified column
!              name.  Append the saved name descriptor to the encoded
!              query.
!
               DO J = 1, EQVDSZ
                  CALL APPNDI ( NAMDSC(J), EQRYI )
               END DO

               STATE   =   TERM


            ELSE IF ( TOKEN .EQ. TKCOMA ) THEN
!
!              The name we picked up was an unqualified column
!              name.  Append the saved name descriptor to the encoded
!              query.  Another name should follow.
!
               DO J = 1, EQVDSZ
                  CALL APPNDI ( NAMDSC(J), EQRYI )
               END DO

               STATE  =  SELECT


            ELSE IF ( TOKEN .EQ. TKDOT ) THEN
!
!              The name we picked up was a table name or alias.  A
!              column name should follow.
!
               STATE  =  SELTAB


            ELSE IF ( TOKEN .EQ. TKKEY ) THEN
!
!              We have the last column name in the SELECT clause.
!
!              Append the saved name descriptor to the encoded
!              query.
!
               DO J = 1, EQVDSZ
                  CALL APPNDI ( NAMDSC(J), EQRYI )
               END DO

!
!              The last column name in the SELECT clause is followed by
!              a keyword.  FROM, WHERE and ORDER are the only valid
!              possibilities.
!
               IF (      ( VALUES(TOKNUM) .EQ. KWWHER ) &
                    .OR. ( VALUES(TOKNUM) .EQ. KWFROM ) &
                    .OR. ( VALUES(TOKNUM) .EQ. KWORDR )  ) THEN
!
!                 We're done with the SELECT clause.
!
                  STATE  =  TERM

               ELSE
!
!                 We've got a keyword we don't want here.
!
                  ERROR  =  .TRUE.
                  ERRTYP =  'BAD_KEYWORD'
                  STATE  =  TERM

               END IF

            ELSE
!
!              We've got the wrong kind of token here.
!
               ERROR  =  .TRUE.
               ERRTYP =  'BAD_TOKEN'
               STATE  =  TERM

            END IF
!
!           STATE is a member of {SELECT, SELTAB, TERM}.
!


         ELSE IF ( STATE .EQ. SELTAB ) THEN
!
!           We've picked up a qualifying table name for a SELECT
!           column.  We must see a column name here.
!
            IF ( TOKEN .EQ. TKID ) THEN
!
!              Update the place-holder table name descriptor in the
!              encoded query.
!
               CALL MOVEI ( NAMDSC, &
                            EQVDSZ, &
                            EQRYI(  CARDI(EQRYI) - EQVDSZ + 1  )   )

!
!              Add the column name to the character part of the
!              encoded query.
!
               I   =   VALUES(TOKNUM)
               B   =   CHBEGS(I)
               E   =   CHENDS(I)
               L   =   E  -  B   +  1
               LXB =   LXBEGS(TOKNUM)
               LXE =   LXENDS(TOKNUM)

               CALL ZZEKINQC ( CHRBUF( B : E ), &
                               L, &
                               LXB, &
                               LXE, &
                               EQRYI, &
                               EQRYC, &
                               COLDSC           )

!
!              Add the descriptor for the column name to the encoded
!              query.
!
               DO J = 1, EQVDSZ
                  CALL APPNDI ( COLDSC(J), EQRYI )
               END DO


               STATE  =  SELCOL


            ELSE IF ( TOKEN .EQ. TKEOQ ) THEN

               ERROR  =  .TRUE.
               ERRTYP =  'MORE_TOKENS_EXP'
               STATE  =  TERM

            ELSE

               ERROR  =  .TRUE.
               ERRTYP =  'COLUMN_EXP'
               STATE  =  TERM

            END IF
!
!           STATE is a member of {SELCOL, TERM}.
!


         ELSE IF ( STATE .EQ. SELCOL ) THEN
!
!           We've picked up a qualified column name.  At this point,
!           we should see a keyword, a comma, or the end of the
!           query.
!
            IF ( TOKEN .EQ. TKKEY ) THEN
!
!              The last column name in the SELECT clause is followed by
!              a keyword.  FROM, WHERE and ORDER are the only valid
!              possibilities.
!
               IF (      ( VALUES(TOKNUM) .EQ. KWWHER ) &
                    .OR. ( VALUES(TOKNUM) .EQ. KWFROM ) &
                    .OR. ( VALUES(TOKNUM) .EQ. KWORDR )  ) THEN
!
!                 We're done with the SELECT clause.
!
                  STATE  =  TERM

               ELSE
!
!                 We've got a keyword we don't want here.
!
                  ERROR  =  .TRUE.
                  ERRTYP =  'BAD_KEYWORD'
                  STATE  =  TERM

               END IF


            ELSE IF ( TOKEN .EQ. TKCOMA ) THEN
!
!              We expect another SELECT column.
!
               STATE  =  SELECT


            ELSE IF ( TOKEN .EQ. TKEOQ ) THEN
!
!              We're done with the SELECT clause.
!
               STATE  =  TERM

            ELSE

               ERROR  =  .TRUE.
               ERRTYP =  'COMMA_OR_KEY_EXP'
               STATE  =  TERM

            END IF
!
!           STATE is a member of {SELECT, TERM}.
!


         ELSE IF ( STATE .EQ. WHERE ) THEN
!
!           The WHERE clause is optional.  See whether we have one.  The
!           clause is started by a WHERE keyword.
!
            CALL ZZEKTLOC ( TKKEY,   KWWHER,  NTOKEN,  TOKENS,  VALUES, &
                            TOKNUM,  FND                               )


            IF ( FND ) THEN
!
!              We're going to hand off the list of tokens that comprise
!              the WHERE clause of the query to a routine that will
!              parse the tokens and form a list of relational
!              constraints.  Once this is done, all we have to do here
!              is check the validity of the column names and the values
!              used in the constraints.
!
               CALL ZZEKNRML ( QUERY, &
                               NTOKEN, &
                               LXBEGS, &
                               LXENDS, &
                               TOKENS, &
                               VALUES, &
                               NUMVLS, &
                               CHRBUF, &
                               CHBEGS, &
                               CHENDS, &
                               EQRYI, &
                               EQRYC, &
                               EQRYD, &
                               ERROR, &
                               PRSERR )


               IF ( ERROR ) THEN

                  ERRTYP  =   'WHERE_ERROR'
                  STATE   =   TERM

               ELSE
!
!                 Parse the ORDER BY clause, if one is present.
!
                  STATE   =   ORDER

               END IF


            ELSE
!
!              Parse the ORDER BY clause, if one is present.
!
               STATE   =   ORDER

            END IF
!
!           STATE is a member of {ORDER, TERM}.
!


         ELSE IF ( STATE .EQ. ORDER ) THEN
!
!           The ORDER BY clause is optional.  See whether we have one.
!           The clause is started by an ORDER keyword.
!
            CALL ZZEKTLOC ( TKKEY,   KWORDR,  NTOKEN,  TOKENS,  VALUES, &
                            TOKNUM,  FND                               )


            IF ( FND ) THEN
!
!              The BY keyword should follow the ORDER keyword.
!
               IF ( TOKNUM .LT. NTOKEN ) THEN

                  TOKNUM  =  TOKNUM + 1

                  IF (       (  TOKENS(TOKNUM) .EQ. TKKEY ) &
                       .AND. (  VALUES(TOKNUM) .EQ. KWBY  )  ) THEN
!
!                    We're ready to parse the ORDER BY clause.
!
                     STATE  =  ORDRBY

                  ELSE
!
!                    No BY keyword followed the ORDER keyword.
!
                     ERROR  =  .TRUE.
                     ERRTYP =  'BY_EXPECTED'
                     STATE  =  TERM

                  END IF


               ELSE
!
!                 We're out of tokens where we shouldn't be.
!
                  ERROR  =  .TRUE.
                  ERRTYP =  'BY_EXPECTED'
                  STATE  =  TERM

               END IF


            ELSE
!
!              We're ready to go on to the SELECT clause.
!
               STATE  =  SELKEY

            END IF
!
!           STATE is a member of {ORDRBY, SELKEY, TERM}.
!



         ELSE IF ( STATE .EQ. ORDRBY ) THEN
!
!           We must see a name in the order column list here.
!           The name may be a lone column name, or it may be a column
!           name qualified by a table name or alias.
!
            IF ( TOKEN .EQ. TKID ) THEN
!
!              We've found a name (as far as we can tell at this point).
!              Make sure we haven't exceeded the limit for order-by
!              column names; if not, store the name string in the
!              encoded query, and save the descriptor until we've
!              figured out whether we're looking at a column name or
!              table name.
!
               NORDER  =  NORDER  +  1

               IF ( NORDER .GT. MAXORD ) THEN

                  ERROR   =  .TRUE.
                  ERRTYP  =  'TOO_MANY_ORD_COLS'
                  STATE   = TERM

               ELSE

                  I       =   VALUES(TOKNUM)
                  B       =   CHBEGS(I)
                  E       =   CHENDS(I)
                  L       =   E  -  B   +  1
                  LXB     =   LXBEGS(TOKNUM)
                  LXE     =   LXENDS(TOKNUM)


                  CALL ZZEKINQC ( CHRBUF( B : E ), &
                                  L, &
                                  LXB, &
                                  LXE, &
                                  EQRYI, &
                                  EQRYC, &
                                  NAMDSC          )

!
!                 Add a place-holder value descriptor to reserve
!                 space for a table descriptor for this name.  If it
!                 turns out that the current name is a table name, we'll
!                 update this descriptor.
!
                  CALL CLEARI ( EQVDSZ, VALDSC )
                  VALDSC(EQDTYP)  =  CHR

                  DO J = 1, EQVDSZ
                     CALL APPNDI ( VALDSC(J), EQRYI )
                  END DO

!
!                 Update the order-by column count in the encoded query.
!
                  CALL ZZEKWEQI ( 'NUM_ORDERBY_COLS', NORDER, EQRYI )


                  STATE  =  ORDNAM

               END IF


            ELSE IF ( TOKEN .EQ. TKEOQ ) THEN

               ERROR  =  .TRUE.
               ERRTYP =  'MORE_TOKENS_EXP'
               STATE  =  TERM


            ELSE
!
!              We've got the wrong kind of token here.
!
               ERROR  =  .TRUE.
               ERRTYP =  'TABLE_OR_COLUMN_EXP'
               STATE  =  TERM

            END IF
!
!           State is a member of {ORDNAM, TERM}.
!


         ELSE IF ( STATE .EQ. ORDNAM ) THEN
!
!           We've seen an order-by column name, or else the name
!           of a table qualifying an order-by column name.
!
            IF ( TOKEN .EQ. TKEOQ ) THEN
!
!              The name we picked up was an unqualified column
!              name.  Append the saved name descriptor to the encoded
!              query.
!
               DO J = 1, EQVDSZ
                  CALL APPNDI ( NAMDSC(J), EQRYI )
               END DO

!
!              Since no ASCENDING or DESCENDING sense keyword was
!              supplied, append the default value ASCENDING to the
!              order-by column descriptor in the encoded query.
!
               CALL APPNDI ( EQASND, EQRYI )

!
!              We're done with the ORDER BY clause; go on to parse the
!              SELECT clause.
!
               STATE   =  SELKEY


            ELSE IF ( TOKEN .EQ. TKCOMA ) THEN
!
!              The name we picked up was an unqualified column
!              name.  Append the saved name descriptor to the encoded
!              query.  Another name should follow.
!
               DO J = 1, EQVDSZ
                  CALL APPNDI ( NAMDSC(J), EQRYI )
               END DO

!
!              Since no ASCENDING or DESCENDING sense keyword was
!              supplied, append the default value ASCENDING to the
!              order-by column descriptor in the encoded query.
!
               CALL APPNDI ( EQASND, EQRYI )

               STATE  = ORDRBY


            ELSE IF ( TOKEN .EQ. TKDOT ) THEN
!
!              The name we picked up was a table name or alias.  A
!              column name should follow.
!
               STATE = ORDTAB


            ELSE IF ( TOKEN .EQ. TKKEY ) THEN
!
!              We have a column name, which may be followed by a
!              keyword indicating the sense of the ordering, or may
!              be followed by a keyword starting a new clause.
!
!              Append the saved name descriptor to the encoded
!              query.
!
               DO J = 1, EQVDSZ
                  CALL APPNDI ( NAMDSC(J), EQRYI )
               END DO

!
!              Set the sense descriptor according to the keyword we've
!              picked up.  After this, we're ready to look for another
!              order-by column.
!
               IF ( VALUES(TOKNUM) .EQ. KWASND ) THEN

                  CALL APPNDI ( EQASND, EQRYI )

                  STATE = ORDSNS


               ELSE IF ( VALUES(TOKNUM) .EQ. KWDSND ) THEN

                  CALL APPNDI ( EQDSND, EQRYI )

                  STATE = ORDSNS


               ELSE IF (      ( VALUES(TOKNUM) .EQ. KWWHER ) &
                         .OR. ( VALUES(TOKNUM) .EQ. KWFROM ) &
                         .OR. ( VALUES(TOKNUM) .EQ. KWSEL  )  ) THEN

!
!                 Since no ASCENDING or DESCENDING sense keyword was
!                 supplied, append the default value ASCENDING to the
!                 order-by column descriptor in the encoded query.
!
                  CALL APPNDI ( EQASND, EQRYI )

!
!                 We're done with the ORDER BY clause.  Go on to
!                 parse the SELECT clause.
!
                  STATE  =  SELKEY

               ELSE
!
!                 We've got a keyword we don't want here.
!
                  ERROR  =  .TRUE.
                  ERRTYP =  'BAD_KEYWORD'
                  STATE  =  TERM

               END IF


            ELSE
!
!              We've got the wrong kind of token here.
!
               ERROR  =  .TRUE.
               ERRTYP =  'BAD_TOKEN'
               STATE  =  TERM

            END IF
!
!           STATE is a member of {ORDRBY, ORDTAB, ORDSNS, SELKEY, TERM}.
!


         ELSE IF ( STATE .EQ. ORDTAB ) THEN
!
!           We've picked up a qualifying table name for an order-by
!           column.  We must see a column name here.
!
            IF ( TOKEN .EQ. TKID ) THEN
!
!              Update the place-holder table name descriptor in the
!              encoded query.
!
               CALL MOVEI ( NAMDSC, &
                            EQVDSZ, &
                            EQRYI(  CARDI(EQRYI) - EQVDSZ + 1  )   )

!
!              Add the column name to the character part of the
!              encoded query.
!
               I   =   VALUES(TOKNUM)
               B   =   CHBEGS(I)
               E   =   CHENDS(I)
               L   =   E  -  B   +  1
               LXB =   LXBEGS(TOKNUM)
               LXE =   LXENDS(TOKNUM)

               CALL ZZEKINQC ( CHRBUF( B : E ), &
                               L, &
                               LXB, &
                               LXE, &
                               EQRYI, &
                               EQRYC, &
                               COLDSC           )

!
!              Add the descriptor for the column name to the encoded
!              query.
!
               DO J = 1, EQVDSZ
                  CALL APPNDI ( COLDSC(J), EQRYI )
               END DO


               STATE  =  ORDCOL


            ELSE IF ( TOKEN .EQ. TKEOQ ) THEN

               ERROR  =  .TRUE.
               ERRTYP =  'MORE_TOKENS_EXP'
               STATE  =  TERM

            ELSE

               ERROR  =  .TRUE.
               ERRTYP =  'COLUMN_EXP'
               STATE  =  TERM

            END IF
!
!           STATE is a member of {ORDCOL, TERM}.
!


         ELSE IF ( STATE .EQ. ORDCOL ) THEN
!
!           We've picked up a qualified column name.  At this point,
!           we should see a sense keyword, a comma, the end of the
!           query, or one of the FROM, SELECT, or WHERE keywords.
!
            IF ( TOKEN .EQ. TKKEY ) THEN

               IF ( VALUES(TOKNUM) .EQ. KWASND ) THEN
!
!                 The ASCENDING keyword has been supplied.  After this,
!                 look for another column.
!
                  CALL APPNDI ( EQASND, EQRYI )

                  STATE  =  ORDSNS


               ELSE IF ( VALUES(TOKNUM) .EQ. KWDSND ) THEN
!
!                 The DESCENDING keyword has been supplied.  After this,
!                 look for another column.
!
                  CALL APPNDI ( EQDSND, EQRYI )

                  STATE  =  ORDSNS


               ELSE IF (      ( VALUES(TOKNUM) .EQ. KWWHER ) &
                         .OR. ( VALUES(TOKNUM) .EQ. KWFROM ) &
                         .OR. ( VALUES(TOKNUM) .EQ. KWSEL  )  ) THEN
!
!                 We're done with the ORDER BY clause.  Go on to
!                 parse the SELECT clause.
!
                  STATE  =  SELKEY

               ELSE

                  ERROR  =  .TRUE.
                  ERRTYP =  'BAD_KEYWORD'
                  STATE  =  TERM

               END IF


            ELSE IF ( TOKEN .EQ. TKCOMA ) THEN
!
!              The ASCENDING keyword is implied.
!
               CALL APPNDI ( EQASND, EQRYI )

               STATE  =  ORDRBY


            ELSE IF ( TOKEN .EQ. TKEOQ ) THEN
!
!              The ASCENDING keyword is implied.
!
               CALL APPNDI ( EQASND, EQRYI )

!
!              We're done with the ORDER BY clause.  Parse the SELECT
!              clause.
!
               STATE  =  SELKEY

            ELSE

               ERROR  =  .TRUE.
               ERRTYP =  'COMMA_OR_KEY_EXP'
               STATE  =  TERM

            END IF
!
!           STATE is a member of {ORDRBY, ORDSNS, SELKEY, TERM}.
!


         ELSE IF ( STATE .EQ. ORDSNS ) THEN
!
!           We've picked up an order sense keyword.  At this point,
!           we should see comma or the end of the query, or one of the
!           FROM, SELECT, or WHERE keywords.
!
            IF ( TOKEN .EQ. TKCOMA ) THEN
!
!              We're ready to look for another column.
!
               STATE  =  ORDRBY


            ELSE IF ( TOKEN .EQ. TKEOQ ) THEN
!
!              We're done with the ORDER BY clause.  Parse the SELECT
!              clause.
!
               STATE  =  SELKEY


            ELSE IF ( TOKEN .EQ. TKKEY ) THEN

               IF (      ( VALUES(TOKNUM) .EQ. KWWHER ) &
                    .OR. ( VALUES(TOKNUM) .EQ. KWFROM ) &
                    .OR. ( VALUES(TOKNUM) .EQ. KWSEL  )  ) THEN
!
!                 We're done with the ORDER BY clause.  Go on to
!                 parse the SELECT clause.
!
                  STATE  =  SELKEY

               ELSE

                  ERROR  =  .TRUE.
                  ERRTYP =  'BAD_KEYWORD'
                  STATE  =  TERM

               END IF


            ELSE

               ERROR  =  .TRUE.
               ERRTYP =  'COMMA_EXP'
               STATE  =  TERM

            END IF
!
!           STATE is a member of {ORDRBY, SELKEY, TERM}.
!


         ELSE
!
!           Somehow, we've reached an invalid state.
!
            ERROR  = .TRUE.
            PRSERR = 'SPICE(BUG) -- Invalid state reached in EK parser.'
            STATE  =  TERM

         END IF
!
!           STATE is a member of {ORDRBY, TERM}.
!

      END DO
!
!     At this point, either an error has been detected, or the query
!     has been parsed, and the query is represented in encoded form
!     in the outputs EQRYI, EQRYC, and EQRYD.
!

!
!     We centralize construction of error messages in the following
!     section.
!
      IF ( ERROR ) THEN

         IF ( ERRTYP .EQ. 'FROM_NOT_FOUND' ) THEN

            PRSERR = 'Every query must contain a FROM clause. ' // &
                     'The FROM keyword was not found.'


         ELSE IF ( ERRTYP .EQ. 'SELECT_NOT_FOUND' ) THEN

            PRSERR = 'Every query must contain a SELECT clause. ' // &
                     'The SELECT keyword was not found.'


         ELSE IF ( ERRTYP .EQ. 'BY_EXPECTED' ) THEN

            PRSERR = 'The BY keyword was not found following the ' // &
                     'ORDER keyword.'


         ELSE IF ( ERRTYP .EQ. 'BAD_KEYWORD' ) THEN

            LXB    =  LXBEGS(TOKNUM)
            LXE    =  LXENDS(TOKNUM)
            PRSERR = 'Invalid keyword at location #. '  // &
                     'Actual token was: #'
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )


         ELSE IF ( ERRTYP .EQ. 'TABLE_OR_COLUMN_EXP' ) THEN

            LXB    =  LXBEGS(TOKNUM)
            LXE    =  LXENDS(TOKNUM)
            PRSERR = 'Table or column name expected at location ' // &
                     '#. Actual token was: #'
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )


         ELSE IF ( ERRTYP .EQ. 'TABLE_EXP' ) THEN

            LXB    =  LXBEGS(TOKNUM)
            LXE    =  LXENDS(TOKNUM)
            PRSERR = 'Table name expected at location #. '  // &
                     'Actual token was: #'
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )


         ELSE IF ( ERRTYP .EQ. 'COLUMN_EXP' ) THEN

            LXB    =  LXBEGS(TOKNUM)
            LXE    =  LXENDS(TOKNUM)
            PRSERR = 'Column name expected at location #. '  // &
                     'Actual token was: #'
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )


         ELSE IF ( ERRTYP .EQ. 'ALIAS_EXP' ) THEN

            LXB    =  LXBEGS(TOKNUM)
            LXE    =  LXENDS(TOKNUM)
            PRSERR = 'Table alias, comma, or keyword expected at ' // &
                     'location #. Actual token was: #'
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )


         ELSE IF ( ERRTYP .EQ. 'COMMA_OR_KEY_EXP' ) THEN

            LXB    =  LXBEGS(TOKNUM)
            LXE    =  LXENDS(TOKNUM)
            PRSERR = 'Comma or keyword expected at ' // &
                     'location #. Actual token was: #'
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )


         ELSE IF ( ERRTYP .EQ. 'COMMA_EXP' ) THEN

            LXB    =  LXBEGS(TOKNUM)
            LXE    =  LXENDS(TOKNUM)
            PRSERR = 'Comma expected at location #. Actual token was: #'
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )


         ELSE IF ( ERRTYP .EQ. 'MORE_TOKENS_EXP' ) THEN

            PRSERR  =  'More tokens were expected in query.'


         ELSE IF ( ERRTYP .EQ. 'KEYWORD_EXP' ) THEN

            PRSERR = 'The keyword # was expected at location ' // &
                     '#. Actual token was: #'
            CALL REPMC ( PRSERR, '#', EXPKEY,         PRSERR )
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )


         ELSE IF ( ERRTYP .EQ. 'BAD_TOKEN' ) THEN

            LXB    =  LXBEGS(TOKNUM)
            LXE    =  LXENDS(TOKNUM)
            PRSERR = 'Invalid token at location #. Token was: #'
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )


         ELSE IF ( ERRTYP .EQ. 'TOO_MANY_TABLES' ) THEN

            PRSERR  =  'Number of tables in "FROM" clause exceeds ' // &
                       'allowed maximum of #.'
            CALL REPMI  ( PRSERR, '#', MAXTAB, PRSERR )


         ELSE IF ( ERRTYP .EQ. 'TOO_MANY_ORD_COLS' ) THEN

            PRSERR  =  'Number of order-by columns exceeds allowed ' // &
                       'maximum of #.'
            CALL REPMI  ( PRSERR, '#', MAXTAB, PRSERR )


         ELSE IF ( ERRTYP .EQ. 'TOO_MANY_SEL_COLS' ) THEN

            PRSERR  =  'Number of SELECT columns exceeds allowed ' // &
                       'maximum of #.'
            CALL REPMI  ( PRSERR, '#', MAXSEL, PRSERR )


         ELSE IF ( ERRTYP .NE. 'WHERE_ERROR' ) THEN

            PRSERR  =  'SPICE(BUG)--Unrecognized error type.  Type ' // &
                       'was #.'
            CALL REPMC ( PRSERR, '#', ERRTYP, PRSERR )

         END IF



      ELSE
!
!        Indicate that parsing is complete.
!
         CALL ZZEKWEQI ( 'PARSED', ITRUE, EQRYI )

      END IF


      CALL CHKOUT ( 'ZZEKPARS' )
      RETURN
      END
