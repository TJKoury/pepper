!$Procedure      ZZEKNRML ( EK, normalize WHERE clause )

      SUBROUTINE ZZEKNRML ( QUERY,   NTOKEN,  LXBEGS,  LXENDS,  TOKENS, &
                            VALUES,  NUMVLS,  CHRBUF,  CHBEGS,  CHENDS, &
                            EQRYI,   EQRYC,   EQRYD,   ERROR,   PRSERR )

! $ Abstract
!
!     Convert the WHERE clause of an EK query to a normalized form.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      INCLUDE 'ekkeyw.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'ektokn.inc'
      INCLUDE 'ektype.inc'

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
!     QUERY      I   Input EK query.
!     NTOKEN     I   Number of tokens in query.
!     LXBEGS     I   Start positions of lexemes comprising WHERE clause.
!     LXENDS     I   End positions of lexemes comprising WHERE clause.
!     TOKENS     I   Tokens comprising query.
!     VALUES     I   Values associated with tokens.
!     NUMVLS     I   Buffer containing numeric token values.
!     CHRBUF     I   Buffer containing string token values.
!     CHBEGS,
!     CHENDS     I   String token begin and end character positions.
!     EQRYI,
!     EQRYC,
!     EQRYD      O   Parsed query and string and number value buffers.
!     ERROR      O   Parse error flag.
!     PRSERR     O   Parse error message.
!
! $ Detailed_Input
!
!     QUERY          is an EK query to be parsed.  The tokens of the
!                    query have been found already.  See the header
!                    of the subroutine EKFIND for a detailed
!                    description of the EK query language.
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
! $ Detailed_Output
!
!     EQRYI,
!     EQRYC,
!     EQRYD          are the integer, character, and numeric portions
!                    of an encoded form of the input query.  The WHERE
!                    clause of the input query is represented in this
!                    encoding.  The WHERE clause constraints have been
!                    normalized.
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
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     No matter how ridiculous the query passed to ZZEKNRML, the
!     routine diagnoses errors via the output arguments ERROR and
!     PRSERR.  No errors are signaled.  The possible error messages
!     returned by this routine are:
!
!
!        Conjunction table is full.
!
!        Disjunction table is full.
!
!        Empty WHERE clause.
!
!        Missing WHERE keyword.
!
!        More tokens expected.
!
!        NULL values are not allowed in BETWEEN or NOT BETWEEN clauses.
!
!        NULL values can only be used with the operators
!        "IS NULL", "NOT NULL" or equivalents.
!
!        Relation table is full.
!
!        Stack is full.
!
!        Syntax error:  badly formed WHERE clause.
!
!        Token following BETWEEN operator is invalid.
!
!        Token following NOT operator was invalid.
!
!        Token must be followed by a comparison operator.
!
!        Token must be followed by the AND operator.
!
!        Token sequence must be followed by a value.
!
!        Tokens were missing from comparison relation.
!
!        Tokens were missing from logical expression.
!
!        Too few tokens in WHERE clause.
!
!        Too many tokens in query; max allowed is #.
!
!        Unexpected keyword # found at location #.
!
!        Unexpected right parenthesis found.
!
!        Unexpected token # found at location #.
!
!        Unexpected token found following valid expression.
!
!        Unexpected token found.
!
!        WHERE clause ran out of tokens unexpectedly.
!        This may be due to an extra left parenthesis.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Here is the grammar for the EK WHERE clause:
!
!        <WHERE clause>           =>   WHERE <relational expression>
!
!
!        <relational expression>  =>   <simple expression>
!
!                                      <NULL value expression>
!
!                                    | NOT <relational expression>
!
!                                    |   ( <relational expression> )
!
!                                    |     <relational expression>
!                                      AND <relational expression>
!
!                                    |     <relational expression>
!                                      OR  <relational expression>
!
!
!        <simple expression>      =>   <LHS> <operator> <RHS>
!
!                                    | <LHS> BETWEEN     <RHS> AND <RHS>
!
!                                    | <LHS> NOT BETWEEN <RHS> AND <RHS>
!
!
!        <NULL value expression>  =>   <LHS> <Null operator> NULL
!
!
!        <LHS>                    =>   <name>
!
!
!        <RHS>                    =>   <name>
!                                    | <value>
!
!
!        <name>                   =>   <identifier> . <identifier>
!                                    | <identifier>
!
!
!        <operator>               =>   EQ
!                                    | GE
!                                    | GT
!                                    | LE
!                                    | LT
!                                    | NE
!                                    | LIKE
!                                    | NOT LIKE
!                                    | =
!                                    | >=
!                                    | >
!                                    | <=
!                                    | <
!                                    | !=
!                                    | <>
!
!
!        <NULL operator>         =>    IS
!                                    | IS NOT
!                                    | EQ
!                                    | NE
!                                    | =
!                                    | !=
!                                    | <>
!
!
!        <value>                 =>    <character value>
!                                    | <d.p. value>
!                                    | <integer value>
!
! $ Examples
!
!     1)  This routine breaks down the constraints of the WHERE clause
!
!             WHERE        ( ( COL1 EQ VAL1 ) OR ( COL2 NE VAL2 ) )
!                     AND  ( ( COL3 LE VAL3 ) OR ( COL4 GT VAL4 ) )
!
!         as
!
!                          (  ( COL1 EQ VAL1 ) AND ( COL3 LE VAL3 ) )
!                     OR   (  ( COL1 EQ VAL1 ) AND ( COL4 GT VAL4 ) )
!                     OR   (  ( COL2 NE VAL2 ) AND ( COL3 LE VAL3 ) )
!                     OR   (  ( COL2 NE VAL2 ) AND ( COL4 GT VAL4 ) )
!
!
!
!     2)  This routine breaks down the constraints of the WHERE clause
!
!             WHERE  NOT ( ( COL1 EQ VAL1 ) OR ( COL2 NE VAL2 ) )
!
!          as
!                          ( COL1 NE VAL1 ) AND ( COL3 EQ VAL3 )
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
!-    SPICELIB Version 3.0.1, 02-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 3.0.0, 17-NOV-1995 (NJB)
!
!        Significantly re-written for architecture 3.
!
!-&

!
!
!     SPICELIB functions
!
      INTEGER               CARDI
      INTEGER               ISRCHI
      INTEGER               LNKHL
      INTEGER               LNKNFN
      INTEGER               LNKNXT
      INTEGER               LNKPRV
      INTEGER               LNKTL

      LOGICAL               RETURN

!
!
!     Local parameters
!
      INTEGER               NIL
      PARAMETER           ( NIL   =  0 )

!
!     Data structure bounds:
!

!
!     MAXREL is the maximum number of relations that can be handled
!     by this routine.
!
      INTEGER               MAXREL
      PARAMETER           ( MAXREL =   10*MAXTOK  )

!
!     MAXMET is the maximum number of meta-tokens making up any
!     expression.
!
      INTEGER               MAXMET
      PARAMETER           ( MAXMET =   10*MAXTOK )

!
!     LBPOOL is the lower bound of the second index of a linked list
!     pool array.
!
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

!
!
!     Stack parameters:
!
!
      INTEGER               MAXSTK
      PARAMETER           ( MAXSTK = 500 )


!
!     Operator parameters:
!
!
!     NLOGOP is the number of recognized logical operators.  These
!     are AND, OR, and NOT.
!
      INTEGER               NLOGOP
      PARAMETER           ( NLOGOP =  3 )

!
!     NRELOP is the number of arithmetic and character comparison
!     operators.
!
      INTEGER               NRELOP
      PARAMETER           ( NRELOP = 8 )

!
!
!     Meta-token codes, excluding codes for relational operators:
!
      INTEGER               RGROUP
      PARAMETER           ( RGROUP = -1 )

      INTEGER               LGROUP
      PARAMETER           ( LGROUP = RGROUP - 1 )

      INTEGER               BTWEEN
      PARAMETER           ( BTWEEN = LGROUP - 1 )

      INTEGER               NOTBTW
      PARAMETER           ( NOTBTW = BTWEEN - 1 )

      INTEGER               BTWEXP
      PARAMETER           ( BTWEXP = NOTBTW - 1 )

      INTEGER               NAME
      PARAMETER           ( NAME   = BTWEXP - 1 )

      INTEGER               IDENT
      PARAMETER           ( IDENT  = NAME   - 1 )

      INTEGER               VALUE
      PARAMETER           ( VALUE  = IDENT  - 1 )

      INTEGER               PERIOD
      PARAMETER           ( PERIOD = VALUE  - 1 )

      INTEGER               AND
      PARAMETER           ( AND    = PERIOD - 1 )

      INTEGER               OR
      PARAMETER           ( OR     = AND    - 1 )

      INTEGER               NOT
      PARAMETER           ( NOT    = OR     - 1 )

      INTEGER               EXPR
      PARAMETER           ( EXPR   = NOT    - 1 )

!
!     Number of keywords that can terminate a WHERE clause.
!
      INTEGER               NENDKW
      PARAMETER           ( NENDKW = 3 )

!
!
!     State parameters:
!
      INTEGER               PUSH
      PARAMETER           ( PUSH   =          0 )

      INTEGER               POP
      PARAMETER           ( POP    = PUSH   + 1 )

      INTEGER               PARSE
      PARAMETER           ( PARSE  = POP    + 1 )

      INTEGER               REDUCD
      PARAMETER           ( REDUCD = PARSE  + 1 )

      INTEGER               TERM
      PARAMETER           ( TERM   = REDUCD + 1 )


!
!
!     'Pop condition' codes:
!
      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )

      INTEGER               REDUCE
      PARAMETER           ( REDUCE = 1 )

      INTEGER               ENDGRP
      PARAMETER           ( ENDGRP = REDUCE + 1 )

!
!     Token descriptor size:
!
      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = EQVDSZ + 1 )

!
!     Local variables
!

!
!     Each comparison relation is expressed by three tokens, so the
!     comparison relations are represented by a 3 x MAXREL array.  The
!     first and third elements of each row of RELS are array indices
!     that point into the input array TOKENS; the middle element
!     of each row is an operator code.  The set of triples representing
!     comparison relations is indexed by a doubly linked list pool.
!     Each conjunction of comparison relations is represented by a
!     linked list of pointers to entries in the RELS array.  These
!     pointers are contained in the CJPTRS array.  The pointers are
!     linked via entries in the double linked list pool CJPOOL.
!
      INTEGER               RELS   ( 3,  MAXREL          )
      INTEGER               RLPOOL ( 2,  LBPOOL : MAXREL )

      INTEGER               CJPOOL ( 2,  LBPOOL : MAXREL )
      INTEGER               CJPTRS (              MAXREL )

!
!     Each normalized expression is a disjunction of conjunctions.  Each
!     such disjunction is represented by a linked list of nodes
!     associated with pointers to entries in the CJPOOL array.  DJPTRS
!     is the parallel array used to associate each node of a disjunction
!     with the head node of a conjunction list in CJPOOL.
!
      INTEGER               DJPOOL ( 2,  LBPOOL : MAXREL )
      INTEGER               DJPTRS (              MAXREL )

!
!     Meta-tokens are groups of tokens that comprise syntactic units
!     in a query.  Each symbol that appears on the left hand side of
!     a production rule in the grammar corresponds to a type of
!     meta-token.
!
!     Throughout the parsing process, the meta-tokens representing the
!     query are organized as a linked list.  Each meta-token is also
!     associated with a more detailed classification MTCODE.
!
!     For each meta-token that represents an identifier, a value,
!     a name, or an expression, there is a corresponding element of
!     MTEXPR.  This element contains a pointer to a token or to a
!     normalized expression.  In the latter case, the pointer is the
!     head node of a list in the disjunction table.
!
      INTEGER               MTPOOL ( 2,  LBPOOL : MAXTOK )
      INTEGER               MTCODE (              MAXTOK )
      INTEGER               MTEXPR (              MAXTOK )

!
!     Stack variables
!
!     These variables have the following meanings:
!
!        MSTART is the node number of the first meta-token of
!        the current expression being parsed.
!
!        NMETA is the number of meta-tokens in the query.
!
!        POPCND is the `pop condition'.  This is a code indicating
!        what event must occur to trigger popping the current state.
!        The two events that can cause the state to be popped are
!        the execution of a reduction and encountering a right grouper.
!
      INTEGER               MSTART ( MAXSTK )
      INTEGER               POPCND ( MAXSTK )
      INTEGER               NMETA

!
!
!     Other local variables
!
      INTEGER               B
      INTEGER               CJ(4)
      INTEGER               CJNODE
      INTEGER               COLPTR
      INTEGER               DJ(2)
      INTEGER               DJNODE
      INTEGER               DJTAIL
      INTEGER               DSPOOL ( 2, LBPOOL : MAXREL )
      INTEGER               DSCBUF ( DSCSIZ,     MAXREL )
      INTEGER               E
      INTEGER               ENDKW  ( NENDKW )
      INTEGER               ENDLOC
      INTEGER               EXPRHD
      INTEGER               FIRST
      INTEGER               FOURTH
      INTEGER               HEAD1
      INTEGER               HEAD2
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               LEVEL
      INTEGER               LXB
      INTEGER               LXE
      INTEGER               METAHD
      INTEGER               NCONJ
      INTEGER               NEWCJ
      INTEGER               NEWDJ
      INTEGER               NEWREL
      INTEGER               NEXT
      INTEGER               NODE
      INTEGER               NRELS
      INTEGER               OP
      INTEGER               PREV
      INTEGER               REL(4)
      INTEGER               RELPTR
      INTEGER               RELSET ( LBCELL : MAXREL )
      INTEGER               RETCND
      INTEGER               RHSPTR
      INTEGER               SECOND
      INTEGER               SKIP
      INTEGER               SIZES  ( MAXCON )
      INTEGER               START
      INTEGER               STATE
      INTEGER               TABPTR
      INTEGER               TAIL
      INTEGER               THIRD
      INTEGER               TYPE
      INTEGER               WHRBEG
      INTEGER               WHREND
      INTEGER               WHRSIZ
      INTEGER               CMPCDE ( NRELOP     )
      INTEGER               CMPNEG ( NRELOP     )
      INTEGER               CMPOPS ( NRELOP - 1 )
      INTEGER               LOGCDE ( NLOGOP     )
      INTEGER               LOGOPS ( NLOGOP     )

      LOGICAL               DONOW
      LOGICAL               FND
      LOGICAL               QUAL

!
!     Saved variables
!
      SAVE

!
!
!     Initial values
!
      DATA  (  LOGOPS(I), LOGCDE(I),  I = 1, NLOGOP )    / &

               KWAND,   AND, &
               KWOR,    OR, &
               KWNOT,   NOT   /

!
!     Note:  there is no "UNLIKE" keyword, but there is an UNLIKE
!     operator, which is the complement of the LIKE operator.
!
      DATA  (  CMPOPS(I), CMPCDE(I), CMPNEG(I), I = 1, NRELOP-1 )    / &

               KWEQ,      EQ,       NE, &
               KWGE,      GE,       LT, &
               KWGT,      GT,       LE, &
               KWLE,      LE,       GT, &
               KWLT,      LT,       GE, &
               KWNE,      NE,       EQ, &
               KWLIKE,    LIKE,     UNLIKE      /

      DATA     CMPCDE(NRELOP), CMPNEG(NRELOP)   / &
                          UNLIKE,   LIKE        /


      DATA     ENDKW   /  KWFROM, KWORDR, KWSEL /

!
!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKNRML' )
      END IF

!
!     No error at this point.
!
      ERROR  = .FALSE.
      PRSERR = ' '

      IF ( NTOKEN .GT. MAXTOK ) THEN
         ERROR  = .TRUE.
         PRSERR = 'Too many tokens in query; max allowed is #.'
         CALL REPMI  ( PRSERR, '#', MAXTOK, PRSERR )
         CALL CHKOUT ( 'ZZEKNRML'                  )
         RETURN
      END IF

!
!     Find out the start and end indices of the tokens comprising the
!     WHERE clause.  If there are no tokens in the WHERE clause, we may
!     as well go home.
!
      CALL ZZEKTLOC ( TKKEY,   KWWHER,  NTOKEN,  TOKENS,  VALUES, &
                      WHRBEG,  FND                                )

      WHRBEG  =  WHRBEG + 1

      IF ( .NOT. FND ) THEN
         ERROR   =  .TRUE.
         PRSERR  =  'Missing WHERE keyword.'
         CALL CHKOUT ( 'ZZEKNRML' )
         RETURN
      END IF

!
!     The WHERE clause is terminated by the end of the query or by
!     the first keyword of the set {SELECT, FROM, ORDER} that follows
!     the WHERE keyword.
!
      WHREND  =  NTOKEN

      DO I = 1, 3

         CALL ZZEKTLOC ( TKKEY,   ENDKW(I),  NTOKEN,  TOKENS,  VALUES, &
                         ENDLOC,  FND                                 )

         IF ( FND ) THEN

            IF (       ( ENDLOC .LT. WHREND ) &
                 .AND. ( ENDLOC .GT. WHRBEG )  ) THEN

               WHREND  =  ENDLOC-1
            END IF

         END IF

      END DO


      WHRSIZ  =  WHREND  -  WHRBEG  +  1

      IF ( WHRSIZ .EQ. 0 ) THEN
         ERROR   =  .TRUE.
         PRSERR  =  'Empty WHERE clause.'
         CALL CHKOUT ( 'ZZEKNRML' )
         RETURN
      END IF

!
!     Initialize the pools.
!
      CALL LNKINI ( MAXREL, RLPOOL )
      CALL LNKINI ( MAXREL, CJPOOL )
      CALL LNKINI ( MAXREL, DJPOOL )
      CALL LNKINI ( MAXTOK, MTPOOL )
      CALL LNKINI ( MAXREL, DSPOOL )

!
!     Loop through our token list and classify the tokens.  Initialize
!     the meta-token list.
!
      NMETA  =  0
      TAIL   =  NIL

      I      =  WHRBEG

      DO WHILE ( I .LE. WHREND )
!
!        Allocate a node and link it in at the tail of the meta-token
!        list.
!
         CALL LNKAN  ( MTPOOL, NODE         )
         CALL LNKILA ( TAIL,   NODE, MTPOOL )
         TAIL         = NODE

!
!        Each meta-token's expression pointer points to its original
!        token index, by default.
!
         MTEXPR(NODE) = I


         IF ( TOKENS(I) .EQ. TKLPAR  ) THEN

            MTCODE(NODE) = LGROUP


         ELSE IF ( TOKENS(I) .EQ. TKRPAR ) THEN

            MTCODE(NODE) = RGROUP


         ELSE IF (       ( TOKENS(I) .EQ. TKINT  ) &
                   .OR.  ( TOKENS(I) .EQ. TKDP   )  ) THEN
!
!           Numeric values must be added to the encoded query.  We
!           allocate a descriptor from the descriptor pool for
!           each identifier.  The expression pointer for the
!           identifier points to the descriptor.  Note:  the
!           allocation should be safe, since we've checked the total
!           number of tokens in the query.
!
            MTCODE(NODE) = VALUE

            CALL LNKAN ( DSPOOL, MTEXPR(NODE) )

            IF ( TOKENS(I) .EQ. TKINT ) THEN
               TYPE = INT
            ELSE
               TYPE = DP
            END IF

            CALL ZZEKINQN ( NUMVLS(VALUES(I)), &
                            TYPE, &
                            LXBEGS(I), &
                            LXENDS(I), &
                            EQRYI, &
                            EQRYD, &
                            DSCBUF( 1, MTEXPR(NODE) )  )

!
!           Set the descriptor to indicate that it represents a value.
!
            DSCBUF( DSCSIZ, MTEXPR(NODE) )  =  VALUE


         ELSE IF ( TOKENS(I) .EQ. TKQSTR ) THEN
!
!           The treatment of strings is analogous to that of numbers.
!
            MTCODE(NODE) = VALUE

            CALL LNKAN ( DSPOOL, MTEXPR(NODE) )

            B  =  CHBEGS( VALUES(I) )
            E  =  CHENDS( VALUES(I) )

            CALL ZZEKINQC (  CHRBUF(B:E), &
                             E - B + 1, &
                             LXBEGS(I), &
                             LXENDS(I), &
                             EQRYI, &
                             EQRYC, &
                             DSCBUF( 1, MTEXPR(NODE) )  )

!
!           Set the descriptor to indicate that it represents a value.
!
            DSCBUF( DSCSIZ, MTEXPR(NODE) )  =  VALUE


         ELSE IF ( TOKENS(I) .EQ. TKID ) THEN
!
!           Identifiers must be added to the encoded query.  We
!           allocate a descriptor from the descriptor pool for
!           each identifier.  The expression pointer for the
!           identifier points to the descriptor.
!
            MTCODE(NODE) = IDENT

            CALL LNKAN ( DSPOOL, MTEXPR(NODE) )

            B  =  CHBEGS( VALUES(I) )
            E  =  CHENDS( VALUES(I) )

            CALL ZZEKINQC (  CHRBUF(B:E), &
                             E - B + 1, &
                             LXBEGS(I), &
                             LXENDS(I), &
                             EQRYI, &
                             EQRYC, &
                             DSCBUF( 1, MTEXPR(NODE) )  )

!
!           Set the descriptor to indicate that it represents an
!           identifier.
!
            DSCBUF( DSCSIZ, MTEXPR(NODE) )  =  IDENT


         ELSE IF ( TOKENS(I) .EQ. TKDOT ) THEN

            MTCODE(NODE) = PERIOD


         ELSE IF ( TOKENS(I) .EQ. TKKEY  ) THEN
!
!           We have a keyword.  Identify it and locate the corresponding
!           code.
!
            J  =  ISRCHI ( VALUES(I), NLOGOP,   LOGOPS )
            K  =  ISRCHI ( VALUES(I), NRELOP-1, CMPOPS )


            IF ( J .GT. 0 ) THEN
!
!              We have a logical operator, unless we have the NOT LIKE
!               or NOT BETWEEN sequence.
!
               IF ( LOGCDE(J) .NE. NOT ) THEN

                  MTCODE(NODE) = LOGCDE(J)

               ELSE

                  IF ( I .LE. WHREND ) THEN

                     IF (       ( TOKENS(I+1) .EQ. TKKEY  ) &
                          .AND. ( VALUES(I+1) .EQ. KWLIKE )  ) THEN
!
!                       Replace the NOT LIKE sequence with the
!                       UNLIKE operator.  Skip over the LIKE token.
!
                        MTCODE(NODE)  =  UNLIKE
                        I             =  I + 1

                     ELSE IF (       ( TOKENS(I+1) .EQ. TKKEY  ) &
                               .AND. ( VALUES(I+1) .EQ. KWBETW )  ) THEN
!
!                       Replace the NOT BETWEEN sequence with the
!                       NOTBTW operator.  Skip over the BETWEEN token.
!
                        MTCODE(NODE)  =  NOTBTW
                        I             =  I + 1

                     ELSE
                        MTCODE(NODE)  =  NOT
                     END IF

                  ELSE
                     MTCODE(NODE) = NOT
                  END IF

               END IF


            ELSE IF ( K .GT. 0 ) THEN
               MTCODE(NODE) = CMPCDE(K)


            ELSE IF ( VALUES(I) .EQ. KWBETW ) THEN
               MTCODE(NODE) = BTWEEN


            ELSE IF ( VALUES(I) .EQ. KWIS ) THEN
!
!              The token IS translates to EQ; the token sequence
!              IS NOT translates to NE.
!
               IF ( I .LT. WHREND ) THEN

                  IF (       ( TOKENS(I+1) .EQ. TKKEY ) &
                       .AND. ( VALUES(I+1) .EQ. KWNOT )  ) THEN
!
!                    We have an IS NOT sequence.  Skip over the NOT
!                    token; indicate the sequence with a single NE
!                    meta-token.
!
                     MTCODE(NODE) = NE
                     I            = I + 1
                  ELSE
                     MTCODE(NODE) = EQ
                  END IF

               ELSE
                  MTCODE(NODE) = EQ
               END IF


            ELSE IF ( VALUES(I) .EQ. KWNULL ) THEN
!
!              The expression pointer for null values is NIL.
!
               MTCODE(NODE) = VALUE
               MTEXPR(NODE) = NIL

            ELSE
!
!              Sorry, that was the last chance for valid keywords.
!
               LXB     =  LXBEGS(I)
               LXE     =  LXENDS(I)
               ERROR   =  .TRUE.
               PRSERR  =  'Unexpected keyword # found at location #.'
               CALL REPMC  ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )
               CALL REPMI  ( PRSERR, '#', LXB,            PRSERR )
               CALL CHKOUT ( 'ZZEKNRML'                          )
               RETURN

            END IF

         ELSE
!
!           Sorry, that was the last chance, period.
!
            LXB     =  LXBEGS(I)
            LXE     =  LXENDS(I)
            ERROR   =  .TRUE.
            PRSERR  =  'Unexpected token # found at location #.'
            CALL REPMC  ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )
            CALL REPMI  ( PRSERR, '#', LXB,            PRSERR )
            CALL CHKOUT ( 'ZZEKNRML'                          )
            RETURN

         END IF
!
!        At this point, we've classified the Ith token.  MTCODE(NODE)
!        is the meta-token code for this token.
!
         I     = I     + 1
         NMETA = NMETA + 1

      END DO

!
!     Initialize the head of the meta-token list.
!
      METAHD = LNKHL( TAIL, MTPOOL )

!
!     Filter out extraneous parentheses around column names or
!     values.
!
      NODE = METAHD

      DO WHILE ( NODE .GT. 0 )

         IF (      ( MTCODE(NODE) .EQ. NAME  ) &
              .OR. ( MTCODE(NODE) .EQ. VALUE )  )  THEN
!
!           If the current metatoken is bracketed by parentheses,
!           remove them and update the metatoken count accordingly.
!
            PREV  =  LNKPRV( NODE, MTPOOL )
            NEXT  =  LNKNXT( NODE, MTPOOL )

            IF (  ( PREV .GT. 0 ) .AND. ( NEXT .GT. 0 )  ) THEN

               IF (       ( MTCODE( PREV ) .EQ. LGROUP ) &
                    .AND. ( MTCODE( NEXT ) .EQ. RGROUP )   ) THEN

                  CALL LNKFSL ( PREV, PREV, MTPOOL )
                  CALL LNKFSL ( NEXT, NEXT, MTPOOL )
                  METAHD  =   LNKHL ( NODE, MTPOOL )
                  NMETA   =   NMETA - 2
!
!                 We don't advance the current token in this case
!                 because there may be more parentheses to remove.
!
               ELSE
!
!                 This token is not bracketed by parentheses; look at
!                 the next metatoken.
!
                  NODE  =  NEXT

               END IF

            ELSE
!
!              This token is not bracketed by tokens on both sides; look
!              at the next metatoken.  It's ok for the next token to be
!              NIL.
!
               NODE  =  NEXT

            END IF

         ELSE
!
!           The current token is not a name or value; look at the next
!           token.
!
            NODE  =  LNKNXT( NODE, MTPOOL )

         END IF

      END DO


!
!
!     Now it's time to parse our expression.  We will validate the
!     expression by using our grammar rules to condense groups of
!     meta-tokens that correspond to the right-hand sides of grammatical
!     rules into meta-tokens that correspond to the left-hand sides
!     of those same rules.  Each such application of a grammar rule
!     is called a `reduction.'  When we're left with a single
!     meta-token of type <relational expression>, we're done.
!
!     If, before reaching the desired final state, we get to a point
!     where no reductions can be performed, we have a syntax error.
!
!     As parsing advances, we'll start to get meta-tokens that are
!     logical expressions.  Each logical expression will be represented
!     by a data structure that organizes the expression in a way that
!     we'll refer to as `normalized':  the expression will be
!     represented as a disjunction of conjunctions, for example
!
!        ( A AND B AND C ) OR ( D AND E ) OR ( F ) OR ( G AND H AND I )
!
!     Each metatoken that represents a logical expression will
!     refer to it through a pointer which is a member of the MTEXPR
!     array.
!

      IF ( WHRSIZ .LT. 3 ) THEN

         ERROR  = .TRUE.
         PRSERR = 'Too few tokens in WHERE clause.'
         CALL CHKOUT ( 'ZZEKNRML' )
         RETURN

      ELSE

         LEVEL          =  1
         MSTART(LEVEL)  =  METAHD
         POPCND(LEVEL)  =  NONE
         STATE          =  PARSE

      END IF



      DO WHILE ( STATE .NE. TERM )


         IF ( STATE .EQ. PARSE ) THEN
!
!           If the input query is valid, we're looking at the leftmost
!           meta-token of an expression that matches the right-hand
!           side of one of the grammar rules.  Referring back to the
!           rules, we see that there are only a few meta-tokens that are
!           valid as the first token of such an expression:
!
!              - A left grouper
!              - An identifier
!              - A name
!              - An expression
!              - A unary operator (`NOT' )
!
!           We'll see if we can perform a reduction.  The reductions
!           that are possible depend on how many meta-tokens are
!           present in the expression we're looking at.
!
!           FIRST is the node number of the first meta-token to look
!           at, in an attempt to perform a reduction.  SECOND, THIRD,
!           and FOURTH have the obvious meanings; some of these may
!           be 0.
!
            FIRST   =  MSTART ( LEVEL )

            IF ( FIRST .GT. 0 ) THEN
               SECOND  =  LNKNXT ( FIRST,  MTPOOL )
            ELSE
               SECOND  =  NIL
            END IF

            IF ( SECOND .GT. 0 ) THEN
               THIRD   =  LNKNXT ( SECOND, MTPOOL )
            ELSE
               THIRD   =  NIL
            END IF

            IF ( THIRD .GT. 0 ) THEN
               FOURTH  =  LNKNXT ( THIRD,  MTPOOL )
            ELSE
               FOURTH  =  NIL
            END IF



            IF ( FIRST .LE. 0 ) THEN
!
!              This never happens to good commands.
!
               ERROR  = .TRUE.
               PRSERR = 'WHERE clause ran out of tokens '            // &
                        'unexpectedly.  This may be due to an '      // &
                        'extra left parenthesis.'
               CALL CHKOUT ( 'ZZEKNRML' )
               RETURN

            END IF

!
!           We have at least one meta-token to work with.  We'll
!           take different actions depending on its type.
!

            IF ( MTCODE(FIRST) .EQ. IDENT ) THEN
!
!              This is a simple case to deal with:  in valid queries,
!              we have either the sequence
!
!                 <identifier> . <identifier>
!
!              or
!
!                 <identifier>
!
!              Both of these token sequences represent a column name;
!              in the former case, the name is qualified by a table
!              name, in the latter, the column name is unqualified.
!              If the table name is absent, we'll simply save a null
!              descriptor for it.  The descriptors will be linked, with
!              the table descriptor coming first, and the NAME token
!              resulting from reducing this token sequence will point to
!              the list of descriptors via the MTEXPR pointer.
!
!
               IF ( THIRD .GT. 0 ) THEN
!
!                 We can look at the following two tokens.
!
                  IF (       ( MTCODE(SECOND) .EQ. PERIOD ) &
                       .AND. ( MTCODE(THIRD)  .EQ. IDENT  )  ) THEN

                     QUAL  =  .TRUE.
                  ELSE
                     QUAL  =  .FALSE.
                  END IF

               ELSE
!
!                 There aren't enough tokens for this name to be
!                 qualified.
!
                  QUAL  =  .FALSE.

               END IF


               IF ( QUAL ) THEN
!
!                 We have a fully qualified column name.  Hook up the
!                 table and column name descriptors.
!
                  TABPTR  =  MTEXPR(FIRST)
                  COLPTR  =  MTEXPR(THIRD)

                  CALL LNKILA ( TABPTR, COLPTR, DSPOOL )

!
!                 Reduce the expression to a <name> metatoken.
!
                  MTCODE(FIRST)  =  NAME

                  CALL LNKFSL( SECOND, THIRD, MTPOOL )

                  NMETA          =  NMETA - 2

               ELSE
!
!                 We have an unqualified column name.  Allocate a table
!                 descriptor.  Set the table descriptor to indicate a
!                 null character descriptor.  Link this descriptor in
!                 before the column descriptor.
!
                  CALL LNKAN ( DSPOOL, TABPTR )

                  CALL CLEARI ( DSCSIZ, DSCBUF(1,TABPTR) )
                  DSCBUF(EQCTYP,TABPTR)  =  CHR
                  DSCBUF(DSCSIZ,TABPTR)  =  IDENT

                  COLPTR  =  MTEXPR(FIRST)

                  CALL LNKILA ( TABPTR, COLPTR, DSPOOL )

!
!                 Reduce the expression to a <name> metatoken.
!                 The reduction doesn't change the number of metatokens.
!
                  MTEXPR(FIRST)  =  TABPTR
                  MTCODE(FIRST)  =  NAME

               END IF

!
!              Decide the next state.
!
               STATE  =  REDUCD



            ELSE IF ( MTCODE(FIRST) .EQ. VALUE ) THEN
!
!              If the query is valid, the sequence of meta-tokens
!              should be one of
!
!                 <value>  AND  <name>
!                 <value>  AND  <value>
!
!              Both of these reduce to the symbol <BETWEEN expr>.
!
!
               IF (  THIRD .LE. 0 ) THEN

                  ERROR  = .TRUE.
                  PRSERR = 'Tokens were missing from comparison '   // &
                           'relation.'
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN

               END IF

!
!              Null values are not allowed in BETWEEN expressions.
!
               IF (      ( MTEXPR(FIRST) .EQ. NIL ) &
                    .OR. ( MTEXPR(THIRD) .EQ. NIL )  )  THEN

                  ERROR  = .TRUE.
                  PRSERR = 'NULL values are not allowed in BETWEEN ' // &
                           'or NOT BETWEEN clauses.'
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN

               END IF


               IF ( MTCODE(THIRD)  .EQ. IDENT ) THEN
!
!                 We'll need to reduce the IDENT before proceeding.
!
                  START   =  THIRD
                  RETCND  =  REDUCE
                  STATE   =  PUSH


               ELSE IF (      (       MTCODE(SECOND) .EQ. AND      ) &
                        .AND. (     ( MTCODE(THIRD)  .EQ. NAME  ) &
                               .OR. ( MTCODE(THIRD)  .EQ. VALUE )  )   ) &
               THEN
!
!                 This sequence of tokens, when seen in the PARSE
!                 state, is a set of value bounds for a BETWEEN or
!                 NOT BETWEEN expression.  Note that this token sequence
!                 can occur elsewhere, but not in the PARSE state.
!                 This is because the meta-token sequences
!
!                    <value>  AND  <name>
!                    <value>  AND  <value>
!
!                 occur at the start of the RHS of only two
!                 productions, namely
!
!                    <BETWEEN expr>  =>  <value>  AND  <name>
!                    <BETWEEN expr>  =>  <value>  AND  <value>
!
!
!                 Hook up the name or value descriptors.
!
                  CALL LNKILB ( MTEXPR(FIRST), MTEXPR(THIRD), DSPOOL )

                  MTCODE(FIRST)  =  BTWEXP

                  CALL LNKFSL ( SECOND, THIRD, MTPOOL )

                  NMETA          =  NMETA - 2

!
!                 Decide the next state.
!
                  STATE  =  REDUCD


               ELSE IF ( MTCODE(SECOND) .GT. 0 ) THEN
!
!                 The third meta-token is in the wrong place at the
!                 wrong time.
!
                  ERROR  =  .TRUE.
                  PRSERR =  'Token sequence must be followed '      // &
                            'by a value.'

                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN


               ELSE
!
!                 The second meta-token is supposed to be the AND token,
!                 but it's actually something else.
!
                  ERROR  =  .TRUE.
                  PRSERR =  'Token must be followed by the AND ' // &
                            'operator.'

                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN

               END IF




            ELSE IF ( MTCODE(FIRST) .EQ. NAME ) THEN
!
!              If the query is valid, the sequence of meta-tokens
!              should be any of
!
!                 <name>  <comparison operator> <value>
!                 <name>  <comparison operator> <name>
!                 <name>  <comparison operator> <ident>
!
!              or
!
!                 <name>  AND  <name>
!                 <name>  AND  <value>
!                 <name>  AND  <ident>
!
!              or
!
!                 <name>  BETWEEN  <BETWEEN expr>
!                 <name>  BETWEEN  <name>
!                 <name>  BETWEEN  <value>
!                 <name>  BETWEEN  <ident>
!
!              or
!
!                 <name>  <NOT BETWEEN>  <BETWEEN expr>
!                 <name>  <NOT BETWEEN>  <name>
!                 <name>  <NOT BETWEEN>  <value>
!                 <name>  <NOT BETWEEN>  <ident>
!
!              There must be at least three meta-tokens here.
!
!
               IF (  THIRD .LE. 0 ) THEN

                  ERROR  = .TRUE.
                  PRSERR = 'Tokens were missing from comparison '   // &
                           'relation.'
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN

               END IF


               IF ( MTCODE(THIRD)  .EQ. IDENT ) THEN
!
!                 We'll need to reduce the IDENT before proceeding.
!
                  START   =  THIRD
                  RETCND  =  REDUCE
                  STATE   =  PUSH


               ELSE IF (      (       MTCODE(SECOND) .EQ. AND      ) &
                        .AND. (     ( MTCODE(THIRD)  .EQ. NAME  ) &
                               .OR. ( MTCODE(THIRD)  .EQ. VALUE )  )   ) &
               THEN
!
!                 This sequence of tokens, when seen in the PARSE
!                 state, is a set of value bounds for a BETWEEN or
!                 NOT BETWEEN expression.  Note that this token sequence
!                 can occur elsewhere, but not in the PARSE state.
!                 This is because the meta-token sequences
!
!                    <name>  AND  <name>
!                    <name>  AND  <value>
!
!                 occur at the start of the RHS of only two
!                 productions, namely
!
!                    <BETWEEN expr>  =>  <name>   AND  <name>
!                    <BETWEEN expr>  =>  <name>   AND  <value>
!
!
!                 Null values are not allowed in BETWEEN expressions.
!
                  IF (      ( MTEXPR(FIRST) .EQ. NIL ) &
                       .OR. ( MTEXPR(THIRD) .EQ. NIL )  )  THEN

                     ERROR  = .TRUE.
                     PRSERR = 'NULL values are not allowed in ' // &
                              'BETWEEN or NOT BETWEEN clauses.'
                     CALL CHKOUT ( 'ZZEKNRML' )
                     RETURN

                  END IF

!
!                 Hook up the name or value descriptors.
!
                  CALL LNKILB ( MTEXPR(FIRST), MTEXPR(THIRD), DSPOOL )

                  MTCODE(FIRST)  =  BTWEXP

                  CALL LNKFSL ( SECOND, THIRD, MTPOOL )

                  NMETA          =  NMETA - 2

!
!                 Decide the next state.
!
                  STATE  =  REDUCD



               ELSE IF (      (        MTCODE(SECOND) .GT. 0       ) &
                        .AND. (      ( MTCODE(THIRD)  .EQ. NAME  ) &
                                .OR. ( MTCODE(THIRD)  .EQ. VALUE ) )   ) &
               THEN
!
!                 Positive meta-token codes denote comparison
!                 operators.
!
!                 We have an arithmetic, string, or column comparison
!                 expression.  This is a trivial normalized
!                 relational expression.  All we have to do
!                 is store the expression in the relation table,
!                 and free the second and third meta-tokens.
!
                  IF ( LNKNFN(RLPOOL) .LT. 1 ) THEN
                     ERROR  = .TRUE.
                     PRSERR = 'Relation table is full.'
                     CALL CHKOUT ( 'ZZEKNRML' )
                     RETURN
                  END IF

                  CALL LNKAN ( RLPOOL, NEWREL )

                  RELS(1,NEWREL) = MTEXPR(FIRST )
                  RELS(2,NEWREL) = MTCODE(SECOND)
                  RELS(3,NEWREL) = MTEXPR(THIRD )


                  CALL LNKFSL ( SECOND, THIRD, MTPOOL )
                  NMETA  =  NMETA - 2

!
!                 Now allocate an entry in the conjunction pool
!                 and make this entry point to the relation table
!                 entry.
!
                  IF ( LNKNFN(CJPOOL) .LT. 1 ) THEN
                     ERROR  = .TRUE.
                     PRSERR = 'Conjunction table is full.'
                     CALL CHKOUT ( 'ZZEKNRML' )
                     RETURN
                  END IF

                  CALL LNKAN ( CJPOOL, NEWCJ )


                  CJPTRS(NEWCJ)  =  NEWREL

!
!                 Now allocate an entry in the disjunction pool
!                 and make this entry point to the conjunction pool
!                 entry.
!
                  IF ( LNKNFN(DJPOOL) .LT. 1 ) THEN
                     ERROR  = .TRUE.
                     PRSERR = 'Disjunction table is full.'
                     CALL CHKOUT ( 'ZZEKNRML' )
                     RETURN
                  END IF

                  CALL LNKAN ( DJPOOL, NEWDJ )

                  DJPTRS(NEWDJ)  =  NEWCJ

!
!                 Change the type of the first meta-token to EXPR and
!                 have that meta-token point to this table entry.  Bag
!                 the other two meta-tokens.
!
                  MTCODE(FIRST)  =  EXPR
                  MTEXPR(FIRST)  =  NEWDJ

!
!                 Decide the next state.
!
                  STATE  =  REDUCD


               ELSE IF (      ( MTCODE(SECOND) .EQ. BTWEEN ) &
                         .OR. ( MTCODE(SECOND) .EQ. NOTBTW )  ) THEN
!
!                 If the command is syntactically correct, the
!                 meta-token sequence should be one of:
!
!                    <name>  <BETWEEN>      <BETWEEN expr>
!                    <name>  <BETWEEN>      <value>
!                    <name>  <BETWEEN>      <name>
!                    <name>  <NOT BETWEEN>  <BETWEEN expr>
!                    <name>  <NOT BETWEEN>  <value>
!                    <name>  <NOT BETWEEN>  <name>
!
!
                  IF (       ( MTCODE(SECOND) .EQ. BTWEEN ) &
                       .AND. ( MTCODE(THIRD)  .EQ. BTWEXP )  ) THEN
!
!                    It's a BETWEEN comparison.  We treat this as a
!                    disjunction of conjunctions of comparison
!                    relations:
!                                      <name>   >=   <item1>
!                              AND     <name>   <=   <item2>
!
!                         OR
!                                      <name>   <=   <item1>
!                              AND     <name>   >=   <item2>
!
!                    where item1 and item2 are specified by the
!                    descriptors belonging to the third meta-token.
!
                     DO I = 1, 4

                        IF (  ( I .EQ. 1 ) .OR. ( I .EQ. 3 )  ) THEN

                           K  =  MTEXPR(THIRD)

                        ELSE
!
!                          We need the descriptor pointer for the RHS
!                          item.  This descriptor is linked to the tail
!                          of the descriptor for the LHS item.  The
!                          number of nodes to skip over depends on
!                          whether the LHS item is a name or value.
!
                           K  =  MTEXPR(THIRD)

                           IF ( DSCBUF(DSCSIZ,K) .EQ. IDENT ) THEN
                              SKIP = 1
                           ELSE
                              SKIP = 0
                           END IF

                           DO J = 1, SKIP + 1
                              K  =  LNKNXT( K, DSPOOL )
                           END DO

                        END IF


                        IF ( LNKNFN(RLPOOL) .LT. 1 ) THEN
                           ERROR  = .TRUE.
                           PRSERR = 'Relation table is full.'
                           CALL CHKOUT ( 'ZZEKNRML' )
                           RETURN
                        END IF

                        CALL LNKAN ( RLPOOL, REL(I) )


                        RELS( 1, REL(I) ) = MTEXPR(FIRST)

                        IF ( ( I .EQ. 1 ) .OR. ( I .EQ. 4 )  ) THEN
                           RELS( 2, REL(I) ) = GE
                        ELSE
                           RELS( 2, REL(I) ) = LE
                        END IF

                        RELS( 3, REL(I) ) = K


                        IF ( LNKNFN(CJPOOL) .LT. 1 ) THEN
                           ERROR  = .TRUE.
                           PRSERR = 'Conjunction table is full.'
                           CALL CHKOUT ( 'ZZEKNRML' )
                           RETURN
                        END IF

                        CALL LNKAN ( CJPOOL, CJ(I) )

                        CJPTRS(CJ(I))  =  REL(I)

                     END DO

!
!                    Link the conjunction nodes to form the two
!                    conjunctions shown above.
!
                     CALL LNKILA ( CJ(1), CJ(2), CJPOOL )
                     CALL LNKILA ( CJ(3), CJ(4), CJPOOL )

!
!                    Allocate disjunction pool entries and make them
!                    point to the two respective conjunctions.
!
                     DO I = 1, 2

                        IF ( LNKNFN(DJPOOL) .LT. 1 ) THEN
                           ERROR  = .TRUE.
                           PRSERR = 'Disjunction table is full.'
                           CALL CHKOUT ( 'ZZEKNRML' )
                           RETURN
                        END IF

                        CALL LNKAN ( DJPOOL, DJ(I) )
                        DJPTRS(DJ(I))  =  CJ( 2*I - 1 )

                     END DO

!
!                    Finally, link the disjunction pool entries, and
!                    create an <expression> meta-token.  Free the unused
!                    meta-tokens.
!
                     CALL LNKILA ( DJ(1), DJ(2), DJPOOL )

                     MTCODE(FIRST)  =  EXPR
                     MTEXPR(FIRST)  =  DJ(1)

                     CALL LNKFSL ( SECOND, THIRD, MTPOOL )
                     NMETA  =  NMETA - 2

!
!                    Decide the next state.
!
                     STATE  =  REDUCD


                  ELSE IF (       ( MTCODE(SECOND) .EQ. NOTBTW ) &
                            .AND. ( MTCODE(THIRD)  .EQ. BTWEXP )  ) THEN
!
!                    It's a NOT BETWEEN comparison.  We treat
!                    this as a disjunction of conjunctions of comparison
!                    relations:
!
!                                      <name>   <   <item1>
!                              AND     <name>   <   <item2>
!
!                         OR
!                                      <name>   >   <item1>
!                              AND     <name>   >   <item2>
!
!                    where item1 and item2 are specified by the
!                    descriptors belonging to the third meta-token.
!
!                    The actions here are closely analogous to those
!                    for the BETWEEN case.
!
                     DO I = 1, 4

                        IF (  ( I .EQ. 1 ) .OR. ( I .EQ. 3 )  ) THEN

                           K  =  MTEXPR(THIRD)

                        ELSE
!
!                          We need the descriptor pointer for the RHS
!                          item.  This descriptor is linked to the tail
!                          of the descriptor for the LHS item.  The
!                          number of nodes to skip over depends on
!                          whether the LHS item is a name or value.
!
                           K  =  MTEXPR(THIRD)

                           IF ( DSCBUF(DSCSIZ,K) .EQ. IDENT ) THEN
                              SKIP = 1
                           ELSE
                              SKIP = 0
                           END IF

                           DO J = 1, SKIP + 1
                              K  =  LNKNXT( K, DSPOOL )
                           END DO

                        END IF


                        IF ( LNKNFN(RLPOOL) .LT. 1 ) THEN
                           ERROR  = .TRUE.
                           PRSERR = 'Relation table is full.'
                           CALL CHKOUT ( 'ZZEKNRML' )
                           RETURN
                        END IF

                        CALL LNKAN ( RLPOOL, REL(I) )


                        RELS( 1, REL(I) ) = MTEXPR(FIRST)

                        IF ( I .LE. 2 ) THEN
                           RELS( 2, REL(I) ) = LT
                        ELSE
                           RELS( 2, REL(I) ) = GT
                        END IF

                        RELS( 3, REL(I) ) = K


                        IF ( LNKNFN(CJPOOL) .LT. 1 ) THEN
                           ERROR  = .TRUE.
                           PRSERR = 'Conjunction table is full.'
                           CALL CHKOUT ( 'ZZEKNRML' )
                           RETURN
                        END IF

                        CALL LNKAN ( CJPOOL, CJ(I) )

                        CJPTRS(CJ(I))  =  REL(I)

                     END DO

!
!                    Link the conjunction nodes to form the two
!                    conjunctions shown above.
!
                     CALL LNKILA ( CJ(1), CJ(2), CJPOOL )
                     CALL LNKILA ( CJ(3), CJ(4), CJPOOL )

!
!                    Allocate disjunction pool entries and make them
!                    point to the two respective conjunctions.
!
                     DO I = 1, 2

                        IF ( LNKNFN(DJPOOL) .LT. 1 ) THEN
                           ERROR  = .TRUE.
                           PRSERR = 'Disjunction table is full.'
                           CALL CHKOUT ( 'ZZEKNRML' )
                           RETURN
                        END IF

                        CALL LNKAN ( DJPOOL, DJ(I) )
                        DJPTRS(DJ(I))  =  CJ( 2*I - 1 )

                     END DO

!
!                    Finally, link the disjunction pool entries, and
!                    create an <expression> meta-token.  Free the unused
!                    meta-tokens.
!
                     CALL LNKILA ( DJ(1), DJ(2), DJPOOL )

                     MTCODE(FIRST)  =  EXPR
                     MTEXPR(FIRST)  =  DJ(1)

                     CALL LNKFSL ( SECOND, THIRD, MTPOOL )
                     NMETA  =  NMETA - 2
!
!                    Decide the next state.
!
                     STATE  =  REDUCD


                  ELSE IF (      ( MTCODE(THIRD) .EQ. NAME  ) &
                            .OR. ( MTCODE(THIRD) .EQ. VALUE )  )  THEN
!
!                    If the third meta-token is anything other than
!                    <BETWEEN expr>, we'll have to parse the portion of
!                    the query following the BETWEEN keyword before
!                    reducing the <BETWEEN> or <NOT BETWEEN> expression.
!
                     START   =  THIRD
                     RETCND  =  REDUCE
                     STATE   =  PUSH

                  ELSE

                     ERROR  =  .TRUE.
                     PRSERR = 'Token following BETWEEN operator is ' // &
                              'invalid.'
                     CALL CHKOUT ( 'ZZEKNRML' )
                     RETURN

                  END IF


               ELSE IF ( MTCODE(SECOND) .GT. 0 ) THEN
!
!                 The third meta-token is in the wrong place at the
!                 wrong time.
!
                  ERROR  =  .TRUE.
                  PRSERR =  'Token sequence must be followed '      // &
                            'by a value.'

                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN


               ELSE
!
!                 The second meta-token is supposed to be a comparison
!                 operator, but it's actually something else.
!
                  ERROR  =  .TRUE.
                  PRSERR =  'Token must be followed by a '          // &
                            'comparison operator.'

                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN

               END IF



            ELSE IF ( MTCODE(FIRST) .EQ. EXPR ) THEN
!
!              If the query is valid, the sequence of meta-tokens
!              should be one of
!
!                 <expression>
!                 <expression>   )
!                 <expression>   OR   <expression>
!                 <expression>   OR   NAME
!                 <expression>   OR   IDENT
!                 <expression>   OR   NOT
!                 <expression>   OR   (
!                 <expression>   AND  <expression>
!                 <expression>   AND  NAME
!                 <expression>   AND  IDENT
!                 <expression>   AND  NOT
!                 <expression>   AND  (
!
               IF ( SECOND .LE. 0 ) THEN
!
!                 This is the last state we pass through
!                 before exiting the loop.  However, some syntax errors
!                 can get us here as well.
!
                  IF (  ( LEVEL .GT. 1 ) .OR. ( NMETA .GT. 1 )  ) THEN

                     ERROR  = .TRUE.
                     PRSERR = 'More tokens expected.'
                     CALL CHKOUT ( 'ZZEKNRML' )
                     RETURN

                  END IF

                  STATE = TERM


               ELSE IF ( MTCODE(SECOND) .EQ. RGROUP ) THEN
!
!                 We've reached the end of a `parenthesized'
!                 expression.
!
                  IF (       ( LEVEL         .GT. 1      ) &
                       .AND. ( POPCND(LEVEL) .EQ. ENDGRP )  ) THEN
!
!                    Time to pop the state.
!
                     STATE = POP

                  ELSE
!
!                    There should not be a right grouper here.
!
                     ERROR  =  .TRUE.
                     PRSERR = 'Unexpected right parenthesis '      // &
                              'found.'
                     CALL CHKOUT ( 'ZZEKNRML' )
                     RETURN

                  END IF


!
!              In all other cases, there must be at least three
!              meta-tokens here.  Make sure there are.
!
               ELSE IF ( THIRD .LE. 0 ) THEN

                  ERROR  =  .TRUE.
                  PRSERR =  'More tokens expected.'
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN

!
!              Take care of the cases that will require reducing a sub-
!              expression before reducing the current expression.
!
               ELSE IF (      ( MTCODE(THIRD) .EQ. IDENT  ) &
                         .OR. ( MTCODE(THIRD) .EQ. NAME   ) &
                         .OR. ( MTCODE(THIRD) .EQ. NOT    )  ) THEN

                  START   =  THIRD
                  RETCND  =  REDUCE
                  STATE   =  PUSH


               ELSE IF ( MTCODE(THIRD) .EQ. LGROUP ) THEN
!
!                 We'll have to push our state before continuing.
!
                  START   =  FOURTH
                  RETCND  =  ENDGRP
                  STATE   =  PUSH

!
!              Now continue with the interesting cases.
!
               ELSE IF (       ( MTCODE(FIRST)  .EQ. EXPR ) &
                         .AND. ( MTCODE(SECOND) .EQ. OR   ) &
                         .AND. ( MTCODE(THIRD)  .EQ. EXPR )  )  THEN
!
!                 We have a disjunction of two normalized
!                 expressions.  We're not ready to perform a
!                 reduction yet; we need to see whether there's
!                 a higher priority operator, namely AND, on the
!                 right of the second expression.
!
                  DONOW = .TRUE.

                  IF ( FOURTH .GT. 0 ) THEN

                     IF ( MTCODE(FOURTH) .EQ. AND ) THEN
!
!                       The third token is already spoken for:
!                       the expression involving the operator
!                       to its right must be processed first.
!
                        DONOW = .FALSE.

                     END IF

                  END IF

                  IF ( DONOW ) THEN
!
!                    This is an easy case to handle:
!                    we can form the resulting normalized
!                    expression by just linking together the two
!                    lists in the disjunction table.
!
                     DJ(1) = MTEXPR(FIRST)
                     DJ(2) = MTEXPR(THIRD)

                     CALL LNKILB ( DJ(1), DJ(2), DJPOOL )
!
!                    The first meta-token will point to the resulting
!                    normalized expression; we'll discard the other
!                    two meta-tokens.
!
                     CALL LNKFSL ( SECOND, THIRD, MTPOOL )
                     NMETA = NMETA - 2

!
!                    MTEXPR(FIRST) and MTCODE(FIRST) are already
!                    set correctly.  All we need to do is determine
!                    our next state.  The next state defaults to
!                    PARSE; the other possibility is POP.
!
                     STATE  =  REDUCD

                  ELSE
!
!                    We'll have to reduce the expression on the right
!                    of the third meta-token before coming back to
!                    this expression.  Get ready to push our state.
!
!                    The condition that must be met in order to pop our
!                    state will be that we've performed a reduction.
!
                     RETCND = REDUCE
                     START  = THIRD
                     STATE  = PUSH

                  END IF
!
!                 Either we've reduced an OR expression, in which case
!                 the state has been set to PARSE or POP, or we've
!                 found a sub-expression that must be reduced before
!                 we attack the current expression, in which case the
!                 state has been set to PUSH.
!

               ELSE IF (       ( MTCODE(FIRST)  .EQ. EXPR ) &
                         .AND. ( MTCODE(SECOND) .EQ. AND  ) &
                         .AND. ( MTCODE(THIRD)  .EQ. EXPR )  )  THEN
!
!                 We have the conjunction of two normalized
!                 expressions.  This case requires application of
!                 DeMorgan's laws to convert the expression to a
!                 normalized form.
!
!                 If we have two normalized expressions, say
!
!                    EXPR1 =        ( A11 and A12 and ... )
!                                or ( A21 and A22 and ... )
!                                              .
!                                              .
!                                              .
!                                or ( AM1 and AM2 and ... )
!
!
!                    EXPR2 =        ( B11 and B12 and ... )
!                                or ( B21 and B22 and ... )
!                                              .
!                                              .
!                                              .
!                                or ( BN1 and BN2 and ... )
!
!
!
!                 Then ( EXPR1 and EXPR2 ) =
!
!
!                        or       {  (     ( AI1 and AI2 and ... )
!                    I = 1,...,M       and ( BJ1 and BJ2 and ... ) )  }
!                    J = 1,...,N
!
!
!                 We have the conjunction of two normalized
!                 So, to represent the normalized expression resulting
!                 from the conjuction of the expressions represented by
!                 the meta-tokens FIRST and THIRD, we will loop through
!                 each disjunction list and form the disjunction of all
!                 conjunctions of pairs of conjunctions, one of which is
!                 from the first expression and one of which is from the
!                 second.  After doing this, we'll clean up the
!                 conjunction and disjunction pools by freeing the
!                 elements in those pools used by the original two
!                 meta-tokens FIRST and THIRD.
!
!
                  DJ(1)    = MTEXPR(FIRST)
                  DJTAIL = NIL

                  DO WHILE ( DJ(1) .GT. 0 )

                     DJ(2) = MTEXPR(THIRD)

                     DO WHILE ( DJ(2) .GT. 0 )
!
!                       Allocate a new disjunction table entry,
!                       and create a new conjunction that represents
!                       the conjunction of the conjunction lists
!                       pointed to by DJ(1) and DJ(2).
!
                        IF ( LNKNFN(DJPOOL) .LT. 1 ) THEN
                           ERROR  = .TRUE.
                           PRSERR = 'Disjunction table is full.'
                           CALL CHKOUT ( 'ZZEKNRML' )
                           RETURN
                        END IF

                        CALL LNKAN ( DJPOOL, NEWDJ )

!
!                       Make copies of the conjunction lists pointed
!                       to by DJ(1) and DJ(2).
!
                        CJ(1) = DJPTRS(DJ(1))
                        TAIL  = NIL

                        DO WHILE ( CJ(1) .GT. 0 )

                           IF ( LNKNFN(CJPOOL) .LT. 1 ) THEN
                              ERROR  = .TRUE.
                              PRSERR = 'Conjunction table is full.'
                              CALL CHKOUT ( 'ZZEKNRML' )
                              RETURN
                           END IF

                           CALL LNKAN  (  CJPOOL, NEWCJ          )
                           CALL LNKILA (  TAIL,   NEWCJ, CJPOOL  )
                           TAIL         = NEWCJ
                           CJPTRS(TAIL) = CJPTRS(CJ(1))

                           CJ(1)        = LNKNXT ( CJ(1), CJPOOL )

                        END DO

                        HEAD1 = LNKHL( TAIL, CJPOOL )


                        CJ(2) = DJPTRS(DJ(2))
                        TAIL  = NIL

                        DO WHILE ( CJ(2) .GT. 0 )

                           IF ( LNKNFN(CJPOOL) .LT. 1 ) THEN
                              ERROR  = .TRUE.
                              PRSERR = 'Conjunction table is full.'
                              CALL CHKOUT ( 'ZZEKNRML' )
                              RETURN
                           END IF

                           CALL LNKAN  (  CJPOOL, NEWCJ          )
                           CALL LNKILA (  TAIL,   NEWCJ, CJPOOL  )
                           TAIL         = NEWCJ
                           CJPTRS(TAIL) = CJPTRS(CJ(2))

                           CJ(2)        = LNKNXT ( CJ(2), CJPOOL )

                        END DO

                        HEAD2 = LNKHL( TAIL, CJPOOL )

!
!                       Now link these copies and make NEWDJ point to
!                       the resulting list.
!
                        CALL LNKILB ( HEAD1, HEAD2, CJPOOL )

                        DJPTRS(NEWDJ) = HEAD1

!
!                       Link NEWDJ in at the tail of the disjunction
!                       list.
!
                        CALL LNKILA  ( DJTAIL, NEWDJ, DJPOOL )
                        DJTAIL = NEWDJ

                        DJ(2)  = LNKNXT ( DJ(2),   DJPOOL )

                     END DO

                     DJ(1) = LNKNXT ( DJ(1), DJPOOL )

                  END DO

!
!                 We've now created the new normalized expression that
!                 represents the conjunction of our original two
!                 expressions.
!
!                 Before continuing, we should clean up the entries in
!                 the disjunction and conjunction pools used by the
!                 original expressions.  We can save a little work
!                 by linking those entries before freeing them.
!
                  CALL LNKILB ( MTEXPR(FIRST), MTEXPR(THIRD), DJPOOL )

                  DJNODE = MTEXPR(FIRST)

                  DO WHILE ( DJNODE .GT. 0 )
!
!                    Free the conjunction list pointed to by DJNODE.
!
                     CJNODE = DJPTRS(DJNODE)

                     CALL LNKFSL (  CJNODE, &
                                    LNKTL(CJNODE,CJPOOL), &
                                    CJPOOL                )

                     DJNODE = LNKNXT( DJNODE, DJPOOL )

                  END DO
!
!                 Free the disjunction list that starts with
!                 MTEXPR(FIRST).
!
                  CALL LNKFSL (  MTEXPR(FIRST), &
                                 LNKTL( MTEXPR(FIRST), DJPOOL ), &
                                 DJPOOL                          )

!
!                 NEWDJ is the tail node of the list of disjunctions
!                 we've just finished.  The first meta-token should
!                 point to the head of this disjunction list.
!
                  MTEXPR(FIRST)  =  LNKHL ( NEWDJ, DJPOOL )

!
!                 We no longer need the other two meta-tokens.
!
                  CALL LNKFSL ( SECOND, THIRD, MTPOOL )
                  NMETA = NMETA - 2

!
!                 Decide the next state.
!
                  STATE  =  REDUCD

               ELSE
!
!                 There are no other valid cases in which the first
!                 meta-token is an expression.
!
                  ERROR  =  .TRUE.
                  PRSERR =  'Unexpected token found following '       // &
                            'valid expression.'
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN

               END IF



            ELSE IF ( MTCODE(FIRST) .EQ. NOT ) THEN
!
!              There are four valid token sequences that we could
!              see here:
!
!                 NOT  <expression>
!                 NOT  IDENT
!                 NOT  NAME
!                 NOT  NOT
!                 NOT  (
!
               IF ( SECOND .LE. 0 ) THEN

                  ERROR  = .TRUE.
                  PRSERR = 'Tokens were missing from logical '        // &
                           'expression.'
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN

               ELSE IF ( MTCODE(SECOND) .EQ. LGROUP ) THEN
!
!                 We'll have to push our state before continuing.
!
                  START   =  THIRD
                  RETCND  =  ENDGRP
                  STATE   =  PUSH


               ELSE IF (      ( MTCODE(SECOND) .EQ. NOT   ) &
                         .OR. ( MTCODE(SECOND) .EQ. IDENT ) &
                         .OR. ( MTCODE(SECOND) .EQ. NAME  )  )  THEN

                  START   =  SECOND
                  RETCND  =  REDUCE
                  STATE   =  PUSH


               ELSE IF ( MTCODE(SECOND) .EQ. EXPR ) THEN
!
!                 We have the negation of a normalized expression. Since
!                 the NOT operator has higher precedence than any other,
!                 we need not concern ourselves with the token on the
!                 right of the expression.
!
!                 This case requires application of DeMorgan's laws to
!                 convert the expression to a normalized form.
!
!
!                 If we have a normalized expression, say
!
!                    EXPR  =         ( A11 and A12 and ... )
!                                 or ( A21 and A22 and ... )
!                                              .
!                                              .
!                                              .
!                                 or ( AM1 and AM2 and ... )
!
!                 Then (using the tilde to express negation):
!
!                    ~EXPR =          ( ~A11 or ~A12 or ... )
!                                 and ( ~A21 or ~A22 or ... )
!                                              .
!                                              .
!                                              .
!                                 and ( ~AM1 or ~AM2 or ... )
!
!                 Since each parenthesized expression above is a
!                 normalized expression, we can convert the conjunction
!                 of any of these expressions and a second normalized
!                 expression to normalized form using the method of the
!                 AND case above.
!
!                 We'll first build the expression
!
!                    ( ~A11 or ~A12 or ... )
!
!                 and then combine the others with it, one by one.
!                 When we're all done, we'll negate the operators used
!                 in the comparison relations.
!
!                 The pointer EXPRHD will denote the head of the
!                 combined normalized expression.
!
                  DJNODE = MTEXPR(SECOND)

                  CJNODE = DJPTRS(DJNODE)
                  TAIL   = NIL

                  DO WHILE ( CJNODE .GT. 0 )
!
!                    Create a new singleton disjunction list
!                    that points to the relation pointed to by
!                    CJNODE.
!
                     IF ( LNKNFN(DJPOOL) .LT. 1 ) THEN
                        ERROR  = .TRUE.
                        PRSERR = 'Disjunction table is full.'
                        CALL CHKOUT ( 'ZZEKNRML' )
                        RETURN
                     END IF

                     CALL LNKAN ( DJPOOL, NEWDJ )

                     IF ( LNKNFN(CJPOOL) .LT. 1  ) THEN
                        ERROR  = .TRUE.
                        PRSERR = 'Conjunction table is full.'
                        CALL CHKOUT ( 'ZZEKNRML' )
                        RETURN
                     END IF

                     CALL LNKAN ( CJPOOL, NEWCJ )

                     CJPTRS(NEWCJ) = CJPTRS(CJNODE)
                     DJPTRS(NEWDJ) = NEWCJ
!
!                    Now link the new singleton disjunction list in
!                    at the tail of the disjunction list that
!                    parallels the conjunction list we're currently
!                    traversing.
!
                     CALL LNKILA ( TAIL, NEWDJ, DJPOOL )
                     TAIL   = NEWDJ

                     CJNODE = LNKNXT ( CJNODE, CJPOOL )

                  END DO

!
!                 Keep track of the head of the new normalized
!                 expression.
!
                  EXPRHD = LNKHL ( TAIL, DJPOOL )

!
!                 Now, for every remaining conjunction in the original
!                 expression, we'll form the normalized expression
!                 resulting from the conjunction of its negation and
!                 of our cumulative normalized expression.  As mentioned
!                 before, we won't negate the comparison operators
!                 just yet.
!
!
                  DJNODE = LNKNXT ( DJNODE, DJPOOL )

                  DO WHILE ( DJNODE .GT. 0 )
!
!                    Loop through our existing cumulative
!                    expression and the latest conjunction, forming
!                    all pairwise conjunctions.
!
                     DJ(1)    = EXPRHD
                     DJTAIL = NIL

                     DO WHILE ( DJ(1) .GT. 0 )

                        CJ(2) = DJPTRS(DJNODE)

                        DO WHILE ( CJ(2) .GT. 0 )
!
!                          Make a copy of the conjunction list pointed
!                          to by DJPTRS(DJ(1)).
!
                           CJNODE = DJPTRS(DJ(1))
                           TAIL   = NIL

                           DO WHILE ( CJNODE .GT. 0 )

                              IF ( LNKNFN(CJPOOL) .LT. 1 ) THEN
                                 ERROR  = .TRUE.
                                 PRSERR = 'Conjunction table is full.'
                                 CALL CHKOUT ( 'ZZEKNRML' )
                                 RETURN
                              END IF

                              CALL LNKAN  ( CJPOOL, NEWCJ         )
                              CALL LNKILA ( TAIL,   NEWCJ, CJPOOL )

                              CJPTRS(NEWCJ)  = CJPTRS(CJNODE)
                              TAIL           = NEWCJ
                              CJNODE         = LNKNXT ( CJNODE, CJPOOL )

                           END DO

                           CJ(1) = LNKHL ( TAIL, CJPOOL )

!
!                          Allocate a new conjunction table entry for
!                          the conjunction of the expressions
!                          pointed to by CJ(1) and CJ(2).  Allocate a
!                          new disjunction table entry to point to this
!                          new conjunction.
!
                           IF ( LNKNFN(CJPOOL) .LT. 1 ) THEN
                              ERROR  = .TRUE.
                              PRSERR = 'Conjunction table is full.'
                              CALL CHKOUT ( 'ZZEKNRML' )
                              RETURN
                           END IF

                           CALL LNKAN ( CJPOOL, NEWCJ )
                           CJPTRS(NEWCJ)  =  CJPTRS(CJ(2))

                           IF ( LNKNFN(DJPOOL) .LT. 1  ) THEN
                              ERROR  = .TRUE.
                              PRSERR = 'Disjunction table is full.'
                              CALL CHKOUT ( 'ZZEKNRML' )
                              RETURN
                           END IF

                           CALL LNKAN ( DJPOOL, NEWDJ )

!
!                          Hook everything up.
!
                           CALL LNKILB ( CJ(1), NEWCJ, CJPOOL )

                           DJPTRS(NEWDJ) =  CJ(1)

                           CALL LNKILA ( DJTAIL, NEWDJ, DJPOOL )
                           DJTAIL        =  NEWDJ

                           CJ(2)         =  LNKNXT ( CJ(2), CJPOOL )

                        END DO

                        DJ(1)    = LNKNXT ( DJ(1), DJPOOL )

                     END DO

!
!                    Before going on, clean up the conjunction and
!                    disjunction pool entries used by our last
!                    version of the cumulative expression.
!
                     DJ(1) = EXPRHD

                     DO WHILE ( DJ(1) .GT. 0 )

                        CJ(1) = DJPTRS(DJ(1))
                        CJ(2) = LNKTL ( CJ(1), CJPOOL )

                        CALL LNKFSL ( CJ(1), CJ(2), CJPOOL )

                        DJ(1) = LNKNXT ( DJ(1), DJPOOL )

                     END DO

                     CALL LNKFSL (  EXPRHD, &
                                    LNKTL( EXPRHD, DJPOOL ), &
                                    DJPOOL                    )

!
!                    Set EXPRHD to be the head of our updated,
!                    cumulative expression.  Start to work on the
!                    next conjunction.
!
                     EXPRHD = LNKHL  ( DJTAIL, DJPOOL )
                     DJNODE = LNKNXT ( DJNODE, DJPOOL )

                  END DO

!
!                 EXPRHD now points to a new expression that will
!                 represent the negation of the expression pointed
!                 to by MTEXPR(SECOND), as soon as we negate the
!                 comparison operators referenced in the expression.
!                 Take care of this last step now.  To make sure that
!                 we negate each operator exactly once, we build a set
!                 of relations to be negated, then negate each relation
!                 in the set.
!
                  CALL SSIZEI ( MAXREL, RELSET )

                  DJNODE = MTEXPR(SECOND)


                  DO WHILE ( DJNODE .GT. 0 )

                     CJNODE = DJPTRS(DJNODE)

                     DO WHILE ( CJNODE .GT. 0 )

                        RELPTR = CJPTRS(CJNODE)
                        CALL INSRTI ( RELPTR, RELSET )

                        CJNODE = LNKNXT( CJNODE, CJPOOL )

                     END DO

                     DJNODE = LNKNXT ( DJNODE, DJPOOL )

                  END DO


                  DO I = 1, CARDI(RELSET)

                     RELPTR          =  RELSET( I )
                     J               =  ISRCHI( RELS(2,RELPTR), &
                                                NRELOP, &
                                                CMPCDE         )
                     RELS(2,RELPTR)  =  CMPNEG( J )

                  END DO

!
!                 Set the pointer of the first meta-token to point
!                 to our normalized expression, and change the
!                 meta-token's code to <expr>.
!
                  MTEXPR(FIRST) = EXPRHD
                  MTCODE(FIRST) = EXPR

!
!                 Get rid of the second meta-token, and determine the
!                 next state.
!
                  CALL LNKFSL ( SECOND, SECOND, MTPOOL )
                  NMETA  =  NMETA - 1

                  STATE  =  REDUCD


               ELSE
!
!                 The second token is invalid in this context.
!
                  ERROR  = .TRUE.
                  PRSERR = 'Token following NOT operator was invalid.'
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN

               END IF
!
!              This is the end of the NOT case.
!


            ELSE IF ( MTCODE(FIRST) .EQ. LGROUP ) THEN
!
!              We're looking at the start of a `parenthesized'
!              sub-expression.
!
!              Push our state, and start parsing at meta-token
!              SECOND.  The condition for popping our state will be
!              that we encounter a right grouper.
!
               RETCND = ENDGRP
               START  = SECOND
               STATE  = PUSH


            ELSE
!
!              Only a syntax error could get us here.
!
               ERROR  =  .TRUE.
               PRSERR = 'Unexpected token found.'
               CALL CHKOUT ( 'ZZEKNRML' )
               RETURN

            END IF

!
!           This is the end of the code for the PARSE state.  We've
!           determined the next parsing state at this point.
!

         ELSE IF ( STATE .EQ. REDUCD ) THEN
!
!           A reduction has been done.  Decide the next state.
!
            STATE  =  REDUCD

            IF ( POPCND(LEVEL) .EQ. REDUCE ) THEN
               STATE         = POP
            ELSE
               MSTART(LEVEL) = FIRST
               STATE         = PARSE
            END IF



         ELSE IF ( STATE .EQ. PUSH ) THEN
!
!           Increment the stack level, and save the current
!           starting point and pop condition.
!
            LEVEL  =  LEVEL + 1

            IF ( LEVEL .GT. MAXSTK ) THEN
               ERROR          = .TRUE.
               PRSERR         = 'Stack is full'
               STATE          =  TERM
            ELSE
               MSTART(LEVEL)  =  START
               POPCND(LEVEL)  =  RETCND
               STATE          =  PARSE
            END IF


         ELSE IF ( STATE .EQ. POP ) THEN
!
!           If we can, pop the state.
!
            IF ( LEVEL .GT. 1 ) THEN

               IF ( POPCND(LEVEL) .EQ. ENDGRP ) THEN
!
!                 If we're popping the state because we encountered a
!                 right grouper, we have a meta-token sequence that
!                 looks like this:
!
!                    (  EXPR  )
!
!                        ^    ^
!                     FIRST  SECOND
!
!                 We need to remove the grouping tokens, taking care to
!                 update the starting token at the next lower level, if
!                 the left grouper was the starting token.
!
                  PREV   =  LNKPRV ( FIRST, MTPOOL )

                  IF ( MSTART(LEVEL-1) .EQ. PREV ) THEN
                     MSTART(LEVEL-1) = FIRST
                  END IF

                  IF ( METAHD .EQ. PREV ) THEN
                     METAHD = FIRST
                  END IF

                  CALL LNKFSL ( PREV,   PREV,   MTPOOL )
                  CALL LNKFSL ( SECOND, SECOND, MTPOOL )

                  NMETA  =  NMETA - 2

               END IF

               LEVEL = LEVEL - 1
               STATE = PARSE

            ELSE
               ERROR  =  .TRUE.
               PRSERR =  'Syntax error:  badly formed WHERE clause.'
               CALL CHKOUT ( 'ZZEKNRML' )
               RETURN
            END IF


         END IF

!
!        We've considered all states.
!

      END DO


!
!     At this point, there should be a single meta-token of type EXPR.
!     This meta-token should point to a normalized expression.  We'll
!     set the encoded query to represent this expression.  For each
!     constraint, we'll add a constraint descriptor to the encoded
!     query.  We'll also update the count of constraints, the count of
!     conjunctions, and we'll add a list of conjunction sizes.
!
      DJNODE = MTEXPR(FIRST)
      NCONJ  = 0
      NRELS  = 0

      DO WHILE ( DJNODE .GT. 0 )

         NCONJ         =  NCONJ + 1
         SIZES(NCONJ)  =  0
         CJNODE        =  DJPTRS(DJNODE)

         DO WHILE ( CJNODE .GT. 0 )

            NRELS          =  NRELS        + 1
            SIZES (NCONJ)  =  SIZES(NCONJ) + 1
            RELPTR         =  CJPTRS(CJNODE)

            TABPTR         =  RELS(1,RELPTR)
            OP             =  RELS(2,RELPTR)
            RHSPTR         =  RELS(3,RELPTR)

!
!           Add a constraint descriptor to the encoded query.  The
!           structure of these descriptors is documented in the include
!           file for encoded query parameters.
!
!           First, save space for the constraint type.  We'll fill this
!           in after finding out what's on the right hand side.
!
            CALL APPNDI ( 0, EQRYI )
            K  =  CARDI ( EQRYI )

!
!           Next, add name descriptors for the table and column on
!           the left-hand side.  These descriptors are linked and
!           pointed to by NAMPTR.
!
            DO I = 1, EQVDSZ
               CALL APPNDI (  DSCBUF( I, TABPTR ),  EQRYI  )
            END DO

            COLPTR  =  LNKNXT( TABPTR, DSPOOL )

            DO I = 1, EQVDSZ
               CALL APPNDI (  DSCBUF( I, COLPTR ),  EQRYI  )
            END DO

!
!           What happens next depends on whether the query has a null
!           value on the right hand side.  This is indicated by the
!           relation's value pointer being NIL.
!
            IF ( RHSPTR .EQ. NIL ) THEN
!
!              For constraints involving null values, we change the
!              operator to ISNULL or NOTNUL as appropriate.
!
               IF ( OP .EQ. EQ ) THEN
                  OP  =  ISNULL

               ELSE IF ( OP .EQ. NE ) THEN
                  OP  =  NOTNUL

               ELSE

                  ERROR  =  .TRUE.
                  PRSERR =  'NULL values can only be used with the ' // &
                            'operators "IS NULL", "NOT NULL", '      // &
                            'or equivalents.'
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN

               END IF

!
!              Set the operator code.
!
               CALL APPNDI ( OP, EQRYI )

!
!              Pad the constraint descriptor up to the full length.
!
               DO I = 1, 2*EQVDSZ
                  CALL APPNDI ( 0, EQRYI )
               END DO

!
!              Set the descriptor's type by updating the reserved
!              location.
!
               EQRYI(K)  =  EQVAL


            ELSE
!
!              For `normal' constraints, that is, constraints that don't
!              involve null values, we set the operator code, then
!              fill in the information describing the RHS of the
!              constraint.
!
               CALL APPNDI ( OP, EQRYI )

               IF (  DSCBUF( DSCSIZ, RHSPTR ) .EQ. VALUE ) THEN
!
!                 The RHS contains a value.  Append the descriptor
!                 for the value, then pad the constraint descriptor.
!
                  DO I = 1, EQVDSZ
                     CALL APPNDI (  DSCBUF( I, RHSPTR ),  EQRYI  )
                  END DO

                  DO I = 1, EQVDSZ
                     CALL APPNDI (  0,  EQRYI  )
                  END DO

!
!                 Set the descriptor's type by updating the reserved
!                 location.
!
                  EQRYI(K)  =  EQVAL


               ELSE
!
!                 The RHS contains a column name.  Append the
!                 descriptors for the table and column.
!
                  DO I = 1, EQVDSZ
                     CALL APPNDI (  DSCBUF( I, RHSPTR ),  EQRYI  )
                  END DO

                  COLPTR  =  LNKNXT( RHSPTR, DSPOOL )

                  DO I = 1, EQVDSZ
                     CALL APPNDI (  DSCBUF( I, COLPTR ),  EQRYI  )
                  END DO

!
!                 Set the descriptor's type by updating the reserved
!                 location.
!
                  EQRYI(K)  =  EQCOL

               END IF

            END IF
!
!           We've updated the encoded query to reflect the current
!           constraint relation.
!
            CJNODE = LNKNXT ( CJNODE, CJPOOL )

         END DO
!
!        We've set the array element SIZES(NCONJ).
!
         DJNODE = LNKNXT ( DJNODE, DJPOOL )

      END DO

!
!     Set the counts of constraints and conjunctions in the encoded
!     query.
!
      CALL ZZEKWEQI ( 'NUM_CONSTRAINTS',  NRELS, EQRYI )
      CALL ZZEKWEQI ( 'NUM_CONJUNCTIONS', NCONJ, EQRYI )

!
!     Add the conjunction size list to the encoded query.
!
      DO I = 1, NCONJ
         CALL APPNDI ( SIZES(I), EQRYI )
      END DO


      CALL CHKOUT ( 'ZZEKNRML' )
      RETURN
      END
