!$Procedure      ZZEKNRML ( EK, normalize WHERE clause )

      subroutine zzeknrml ( query,   ntoken,  lxbegs,  lxends,  tokens, &
                            values,  numvls,  chrbuf,  chbegs,  chends, &
                            eqryi,   eqryc,   eqryd,   error,   prserr )
      use pepper_kinds, only: ip, wp

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

      include 'ekkeyw.inc'
      include 'ekopcd.inc'
      include 'ekqlimit.inc'
      include 'ekquery.inc'
      include 'ektokn.inc'
      include 'ektype.inc'

      integer(ip), parameter :: lbcell = -5

      character(*)          query
      integer(ip)           ntoken
      integer(ip)           lxbegs ( * )
      integer(ip)           lxends ( * )
      integer(ip)           tokens ( * )
      integer(ip)           values ( * )
      real(wp)              numvls ( * )
      character(*)          chrbuf
      integer(ip)           chbegs ( * )
      integer(ip)           chends ( * )
      integer(ip)           eqryi  ( lbcell : * )
      character(*)          eqryc
      real(wp)              eqryd  ( * )
      logical               error
      character(*)          prserr

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
!        <simple expression>      =>   <LHS> <operator> <RHS>
!
!                                    | <LHS> BETWEEN     <RHS> AND <RHS>
!
!                                    | <LHS> NOT BETWEEN <RHS> AND <RHS>
!
!        <NULL value expression>  =>   <LHS> <Null operator> NULL
!
!        <LHS>                    =>   <name>
!
!        <RHS>                    =>   <name>
!                                    | <value>
!
!        <name>                   =>   <identifier> . <identifier>
!                                    | <identifier>
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
!        <NULL operator>         =>    IS
!                                    | IS NOT
!                                    | EQ
!                                    | NE
!                                    | =
!                                    | !=
!                                    | <>
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
!     SPICELIB functions
!
      integer(ip)           cardi
      integer(ip)           isrchi
      integer(ip)           lnkhl
      integer(ip)           lnknfn
      integer(ip)           lnknxt
      integer(ip)           lnkprv
      integer(ip)           lnktl

      logical               return

!
!     Local parameters
!
      integer(ip), parameter :: nil = 0

!
!     Data structure bounds:
!

!
!     MAXREL is the maximum number of relations that can be handled
!     by this routine.
!
      integer(ip), parameter :: maxrel = 10*maxtok

!
!     MAXMET is the maximum number of meta-tokens making up any
!     expression.
!
      integer(ip), parameter :: maxmet = 10*maxtok

!
!     LBPOOL is the lower bound of the second index of a linked list
!     pool array.
!
      integer(ip), parameter :: lbpool = -5

!
!     Stack parameters:
!
      integer(ip), parameter :: maxstk = 500

!
!     Operator parameters:
!
!     NLOGOP is the number of recognized logical operators.  These
!     are AND, OR, and NOT.
!
      integer(ip), parameter :: nlogop = 3

!
!     NRELOP is the number of arithmetic and character comparison
!     operators.
!
      integer(ip), parameter :: nrelop = 8

!
!     Meta-token codes, excluding codes for relational operators:
!
      integer(ip), parameter :: rgroup = -1

      integer(ip), parameter :: lgroup = rgroup - 1

      integer(ip), parameter :: btween = lgroup - 1

      integer(ip), parameter :: notbtw = btween - 1

      integer(ip), parameter :: btwexp = notbtw - 1

      integer(ip), parameter :: name = btwexp - 1

      integer(ip), parameter :: ident = name   - 1

      integer(ip), parameter :: value = ident  - 1

      integer(ip), parameter :: period = value  - 1

      integer(ip), parameter :: and = period - 1

      integer(ip), parameter :: or = and    - 1

      integer(ip), parameter :: not = or     - 1

      integer(ip), parameter :: expr = not    - 1

!
!     Number of keywords that can terminate a WHERE clause.
!
      integer(ip), parameter :: nendkw = 3

!
!     State parameters:
!
      integer(ip), parameter :: push = 0

      integer(ip), parameter :: pop = push   + 1

      integer(ip), parameter :: parse = pop    + 1

      integer(ip), parameter :: reducd = parse  + 1

      integer(ip), parameter :: term = reducd + 1

!
!     'Pop condition' codes:
!
      integer(ip), parameter :: none = 0

      integer(ip), parameter :: reduce = 1

      integer(ip), parameter :: endgrp = reduce + 1

!
!     Token descriptor size:
!
      integer(ip), parameter :: dscsiz = eqvdsz + 1

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
      integer(ip)           rels   ( 3,  maxrel          )
      integer(ip)           rlpool ( 2,  lbpool : maxrel )

      integer(ip)           cjpool ( 2,  lbpool : maxrel )
      integer(ip)           cjptrs (              maxrel )

!
!     Each normalized expression is a disjunction of conjunctions.  Each
!     such disjunction is represented by a linked list of nodes
!     associated with pointers to entries in the CJPOOL array.  DJPTRS
!     is the parallel array used to associate each node of a disjunction
!     with the head node of a conjunction list in CJPOOL.
!
      integer(ip)           djpool ( 2,  lbpool : maxrel )
      integer(ip)           djptrs (              maxrel )

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
      integer(ip)           mtpool ( 2,  lbpool : maxtok )
      integer(ip)           mtcode (              maxtok )
      integer(ip)           mtexpr (              maxtok )

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
      integer(ip)           mstart ( maxstk )
      integer(ip)           popcnd ( maxstk )
      integer(ip)           nmeta

!
!     Other local variables
!
      integer(ip)           b
      integer(ip)           cj(4)
      integer(ip)           cjnode
      integer(ip)           colptr
      integer(ip)           dj(2)
      integer(ip)           djnode
      integer(ip)           djtail
      integer(ip)           dspool ( 2, lbpool : maxrel )
      integer(ip)           dscbuf ( dscsiz,     maxrel )
      integer(ip)           e
      integer(ip)           endkw  ( nendkw )
      integer(ip)           endloc
      integer(ip)           exprhd
      integer(ip)           first
      integer(ip)           fourth
      integer(ip)           head1
      integer(ip)           head2
      integer(ip)           i
      integer(ip)           j
      integer(ip)           k
      integer(ip)           level
      integer(ip)           lxb
      integer(ip)           lxe
      integer(ip)           metahd
      integer(ip)           nconj
      integer(ip)           newcj
      integer(ip)           newdj
      integer(ip)           newrel
      integer(ip)           next
      integer(ip)           node
      integer(ip)           nrels
      integer(ip)           op
      integer(ip)           prev
      integer(ip)           rel(4)
      integer(ip)           relptr
      integer(ip)           relset ( lbcell : maxrel )
      integer(ip)           retcnd
      integer(ip)           rhsptr
      integer(ip)           second
      integer(ip)           skip
      integer(ip)           sizes  ( maxcon )
      integer(ip)           start
      integer(ip)           state
      integer(ip)           tabptr
      integer(ip)           tail
      integer(ip)           third
      integer(ip)           type
      integer(ip)           whrbeg
      integer(ip)           whrend
      integer(ip)           whrsiz
      integer(ip)           cmpcde ( nrelop     )
      integer(ip)           cmpneg ( nrelop     )
      integer(ip)           cmpops ( nrelop - 1 )
      integer(ip)           logcde ( nlogop     )
      integer(ip)           logops ( nlogop     )

      logical               donow
      logical               fnd
      logical               qual

!
!     Saved variables
!
      save

!
!     Initial values
!
      data  (  logops(i), logcde(i),  i = 1, nlogop )    / &

               kwand,   and, &
               kwor,    or, &
               kwnot,   not   /

!
!     Note:  there is no "UNLIKE" keyword, but there is an UNLIKE
!     operator, which is the complement of the LIKE operator.
!
      data  (  cmpops(i), cmpcde(i), cmpneg(i), i = 1, nrelop-1 )    / &

               kweq,      eq,       ne, &
               kwge,      ge,       lt, &
               kwgt,      gt,       le, &
               kwle,      le,       gt, &
               kwlt,      lt,       ge, &
               kwne,      ne,       eq, &
               kwlike,    like,     unlike      /

      data     cmpcde(nrelop), cmpneg(nrelop)   / &
                          unlike,   like        /

      data     endkw   /  kwfrom, kwordr, kwsel /

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     No error at this point.
!
      error  = .false.
      prserr = ' '

      if ( ntoken > maxtok ) then
         error  = .true.
         prserr = 'Too many tokens in query; max allowed is #.'
         call repmi  ( prserr, '#', maxtok, prserr )
         return
      end if

!
!     Find out the start and end indices of the tokens comprising the
!     WHERE clause.  If there are no tokens in the WHERE clause, we may
!     as well go home.
!
      call zzektloc ( tkkey,   kwwher,  ntoken,  tokens,  values, &
                      whrbeg,  fnd                                )

      whrbeg  =  whrbeg + 1

      if ( .not. fnd ) then
         error   =  .true.
         prserr  =  'Missing WHERE keyword.'
         return
      end if

!
!     The WHERE clause is terminated by the end of the query or by
!     the first keyword of the set {SELECT, FROM, ORDER} that follows
!     the WHERE keyword.
!
      whrend  =  ntoken

      do i = 1, 3

         call zzektloc ( tkkey,   endkw(i),  ntoken,  tokens,  values, &
                         endloc,  fnd                                 )

         if ( fnd ) then

            if (       ( endloc < whrend ) &
                 .and. ( endloc > whrbeg )  ) then

               whrend  =  endloc-1
            end if

         end if

      end do

      whrsiz  =  whrend  -  whrbeg  +  1

      if ( whrsiz == 0 ) then
         error   =  .true.
         prserr  =  'Empty WHERE clause.'
         return
      end if

!
!     Initialize the pools.
!
      call lnkini ( maxrel, rlpool )
      call lnkini ( maxrel, cjpool )
      call lnkini ( maxrel, djpool )
      call lnkini ( maxtok, mtpool )
      call lnkini ( maxrel, dspool )

!
!     Loop through our token list and classify the tokens.  Initialize
!     the meta-token list.
!
      nmeta  =  0
      tail   =  nil

      i      =  whrbeg

      do while ( i <= whrend )
!
!        Allocate a node and link it in at the tail of the meta-token
!        list.
!
         call lnkan  ( mtpool, node         )
         call lnkila ( tail,   node, mtpool )
         tail         = node

!
!        Each meta-token's expression pointer points to its original
!        token index, by default.
!
         mtexpr(node) = i

         if ( tokens(i) == tklpar  ) then

            mtcode(node) = lgroup

         else if ( tokens(i) == tkrpar ) then

            mtcode(node) = rgroup

         else if (       ( tokens(i) == tkint  ) &
                   .or.  ( tokens(i) == tkdp   )  ) then
!
!           Numeric values must be added to the encoded query.  We
!           allocate a descriptor from the descriptor pool for
!           each identifier.  The expression pointer for the
!           identifier points to the descriptor.  Note:  the
!           allocation should be safe, since we've checked the total
!           number of tokens in the query.
!
            mtcode(node) = value

            call lnkan ( dspool, mtexpr(node) )

            if ( tokens(i) == tkint ) then
               type = int
            else
               type = dp
            end if

            call zzekinqn ( numvls(values(i)), &
                            type, &
                            lxbegs(i), &
                            lxends(i), &
                            eqryi, &
                            eqryd, &
                            dscbuf( 1, mtexpr(node) )  )

!
!           Set the descriptor to indicate that it represents a value.
!
            dscbuf( dscsiz, mtexpr(node) )  =  value

         else if ( tokens(i) == tkqstr ) then
!
!           The treatment of strings is analogous to that of numbers.
!
            mtcode(node) = value

            call lnkan ( dspool, mtexpr(node) )

            b  =  chbegs( values(i) )
            e  =  chends( values(i) )

            call zzekinqc (  chrbuf(b:e), &
                             e - b + 1, &
                             lxbegs(i), &
                             lxends(i), &
                             eqryi, &
                             eqryc, &
                             dscbuf( 1, mtexpr(node) )  )

!
!           Set the descriptor to indicate that it represents a value.
!
            dscbuf( dscsiz, mtexpr(node) )  =  value

         else if ( tokens(i) == tkid ) then
!
!           Identifiers must be added to the encoded query.  We
!           allocate a descriptor from the descriptor pool for
!           each identifier.  The expression pointer for the
!           identifier points to the descriptor.
!
            mtcode(node) = ident

            call lnkan ( dspool, mtexpr(node) )

            b  =  chbegs( values(i) )
            e  =  chends( values(i) )

            call zzekinqc (  chrbuf(b:e), &
                             e - b + 1, &
                             lxbegs(i), &
                             lxends(i), &
                             eqryi, &
                             eqryc, &
                             dscbuf( 1, mtexpr(node) )  )

!
!           Set the descriptor to indicate that it represents an
!           identifier.
!
            dscbuf( dscsiz, mtexpr(node) )  =  ident

         else if ( tokens(i) == tkdot ) then

            mtcode(node) = period

         else if ( tokens(i) == tkkey  ) then
!
!           We have a keyword.  Identify it and locate the corresponding
!           code.
!
            j  =  isrchi ( values(i), nlogop,   logops )
            k  =  isrchi ( values(i), nrelop-1, cmpops )

            if ( j > 0 ) then
!
!              We have a logical operator, unless we have the NOT LIKE
!               or NOT BETWEEN sequence.
!
               if ( logcde(j) /= not ) then

                  mtcode(node) = logcde(j)

               else

                  if ( i <= whrend ) then

                     if (       ( tokens(i+1) == tkkey  ) &
                          .and. ( values(i+1) == kwlike )  ) then
!
!                       Replace the NOT LIKE sequence with the
!                       UNLIKE operator.  Skip over the LIKE token.
!
                        mtcode(node)  =  unlike
                        i             =  i + 1

                     else if (       ( tokens(i+1) == tkkey  ) &
                               .and. ( values(i+1) == kwbetw )  ) then
!
!                       Replace the NOT BETWEEN sequence with the
!                       NOTBTW operator.  Skip over the BETWEEN token.
!
                        mtcode(node)  =  notbtw
                        i             =  i + 1

                     else
                        mtcode(node)  =  not
                     end if

                  else
                     mtcode(node) = not
                  end if

               end if

            else if ( k > 0 ) then
               mtcode(node) = cmpcde(k)

            else if ( values(i) == kwbetw ) then
               mtcode(node) = btween

            else if ( values(i) == kwis ) then
!
!              The token IS translates to EQ; the token sequence
!              IS NOT translates to NE.
!
               if ( i < whrend ) then

                  if (       ( tokens(i+1) == tkkey ) &
                       .and. ( values(i+1) == kwnot )  ) then
!
!                    We have an IS NOT sequence.  Skip over the NOT
!                    token; indicate the sequence with a single NE
!                    meta-token.
!
                     mtcode(node) = ne
                     i            = i + 1
                  else
                     mtcode(node) = eq
                  end if

               else
                  mtcode(node) = eq
               end if

            else if ( values(i) == kwnull ) then
!
!              The expression pointer for null values is NIL.
!
               mtcode(node) = value
               mtexpr(node) = nil

            else
!
!              Sorry, that was the last chance for valid keywords.
!
               lxb     =  lxbegs(i)
               lxe     =  lxends(i)
               error   =  .true.
               prserr  =  'Unexpected keyword # found at location #.'
               call repmc  ( prserr, '#', query(lxb:lxe), prserr )
               call repmi  ( prserr, '#', lxb,            prserr )
               return

            end if

         else
!
!           Sorry, that was the last chance, period.
!
            lxb     =  lxbegs(i)
            lxe     =  lxends(i)
            error   =  .true.
            prserr  =  'Unexpected token # found at location #.'
            call repmc  ( prserr, '#', query(lxb:lxe), prserr )
            call repmi  ( prserr, '#', lxb,            prserr )
            return

         end if
!
!        At this point, we've classified the Ith token.  MTCODE(NODE)
!        is the meta-token code for this token.
!
         i     = i     + 1
         nmeta = nmeta + 1

      end do

!
!     Initialize the head of the meta-token list.
!
      metahd = lnkhl( tail, mtpool )

!
!     Filter out extraneous parentheses around column names or
!     values.
!
      node = metahd

      do while ( node > 0 )

         if (      ( mtcode(node) == name  ) &
              .or. ( mtcode(node) == value )  )  then
!
!           If the current metatoken is bracketed by parentheses,
!           remove them and update the metatoken count accordingly.
!
            prev  =  lnkprv( node, mtpool )
            next  =  lnknxt( node, mtpool )

            if (  ( prev > 0 ) .and. ( next > 0 )  ) then

               if (       ( mtcode( prev ) == lgroup ) &
                    .and. ( mtcode( next ) == rgroup )   ) then

                  call lnkfsl ( prev, prev, mtpool )
                  call lnkfsl ( next, next, mtpool )
                  metahd  =   lnkhl ( node, mtpool )
                  nmeta   =   nmeta - 2
!
!                 We don't advance the current token in this case
!                 because there may be more parentheses to remove.
!
               else
!
!                 This token is not bracketed by parentheses; look at
!                 the next metatoken.
!
                  node  =  next

               end if

            else
!
!              This token is not bracketed by tokens on both sides; look
!              at the next metatoken.  It's ok for the next token to be
!              NIL.
!
               node  =  next

            end if

         else
!
!           The current token is not a name or value; look at the next
!           token.
!
            node  =  lnknxt( node, mtpool )

         end if

      end do

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

      if ( whrsiz < 3 ) then

         error  = .true.
         prserr = 'Too few tokens in WHERE clause.'
         return

      else

         level          =  1
         mstart(level)  =  metahd
         popcnd(level)  =  none
         state          =  parse

      end if

      do while ( state /= term )

         if ( state == parse ) then
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
            first   =  mstart ( level )

            if ( first > 0 ) then
               second  =  lnknxt ( first,  mtpool )
            else
               second  =  nil
            end if

            if ( second > 0 ) then
               third   =  lnknxt ( second, mtpool )
            else
               third   =  nil
            end if

            if ( third > 0 ) then
               fourth  =  lnknxt ( third,  mtpool )
            else
               fourth  =  nil
            end if

            if ( first <= 0 ) then
!
!              This never happens to good commands.
!
               error  = .true.
               prserr = 'WHERE clause ran out of tokens '            // &
                        'unexpectedly.  This may be due to an '      // &
                        'extra left parenthesis.'
               return

            end if

!
!           We have at least one meta-token to work with.  We'll
!           take different actions depending on its type.
!

            if ( mtcode(first) == ident ) then
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
               if ( third > 0 ) then
!
!                 We can look at the following two tokens.
!
                  if (       ( mtcode(second) == period ) &
                       .and. ( mtcode(third)  == ident  )  ) then

                     qual  =  .true.
                  else
                     qual  =  .false.
                  end if

               else
!
!                 There aren't enough tokens for this name to be
!                 qualified.
!
                  qual  =  .false.

               end if

               if ( qual ) then
!
!                 We have a fully qualified column name.  Hook up the
!                 table and column name descriptors.
!
                  tabptr  =  mtexpr(first)
                  colptr  =  mtexpr(third)

                  call lnkila ( tabptr, colptr, dspool )

!
!                 Reduce the expression to a <name> metatoken.
!
                  mtcode(first)  =  name

                  call lnkfsl( second, third, mtpool )

                  nmeta          =  nmeta - 2

               else
!
!                 We have an unqualified column name.  Allocate a table
!                 descriptor.  Set the table descriptor to indicate a
!                 null character descriptor.  Link this descriptor in
!                 before the column descriptor.
!
                  call lnkan ( dspool, tabptr )

                  call cleari ( dscsiz, dscbuf(1,tabptr) )
                  dscbuf(eqctyp,tabptr)  =  chr
                  dscbuf(dscsiz,tabptr)  =  ident

                  colptr  =  mtexpr(first)

                  call lnkila ( tabptr, colptr, dspool )

!
!                 Reduce the expression to a <name> metatoken.
!                 The reduction doesn't change the number of metatokens.
!
                  mtexpr(first)  =  tabptr
                  mtcode(first)  =  name

               end if

!
!              Decide the next state.
!
               state  =  reducd

            else if ( mtcode(first) == value ) then
!
!              If the query is valid, the sequence of meta-tokens
!              should be one of
!
!                 <value>  AND  <name>
!                 <value>  AND  <value>
!
!              Both of these reduce to the symbol <BETWEEN expr>.
!
               if (  third <= 0 ) then

                  error  = .true.
                  prserr = 'Tokens were missing from comparison '   // &
                           'relation.'
                  return

               end if

!
!              Null values are not allowed in BETWEEN expressions.
!
               if (      ( mtexpr(first) == nil ) &
                    .or. ( mtexpr(third) == nil )  )  then

                  error  = .true.
                  prserr = 'NULL values are not allowed in BETWEEN ' // &
                           'or NOT BETWEEN clauses.'
                  return

               end if

               if ( mtcode(third)  == ident ) then
!
!                 We'll need to reduce the IDENT before proceeding.
!
                  start   =  third
                  retcnd  =  reduce
                  state   =  push

               else if (      (       mtcode(second) == and      ) &
                        .and. (     ( mtcode(third)  == name  ) &
                               .or. ( mtcode(third)  == value )  )   ) &
               then
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
!                 Hook up the name or value descriptors.
!
                  call lnkilb ( mtexpr(first), mtexpr(third), dspool )

                  mtcode(first)  =  btwexp

                  call lnkfsl ( second, third, mtpool )

                  nmeta          =  nmeta - 2

!
!                 Decide the next state.
!
                  state  =  reducd

               else if ( mtcode(second) > 0 ) then
!
!                 The third meta-token is in the wrong place at the
!                 wrong time.
!
                  error  =  .true.
                  prserr =  'Token sequence must be followed '      // &
                            'by a value.'

                  return

               else
!
!                 The second meta-token is supposed to be the AND token,
!                 but it's actually something else.
!
                  error  =  .true.
                  prserr =  'Token must be followed by the AND ' // &
                            'operator.'

                  return

               end if

            else if ( mtcode(first) == name ) then
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
               if (  third <= 0 ) then

                  error  = .true.
                  prserr = 'Tokens were missing from comparison '   // &
                           'relation.'
                  return

               end if

               if ( mtcode(third)  == ident ) then
!
!                 We'll need to reduce the IDENT before proceeding.
!
                  start   =  third
                  retcnd  =  reduce
                  state   =  push

               else if (      (       mtcode(second) == and      ) &
                        .and. (     ( mtcode(third)  == name  ) &
                               .or. ( mtcode(third)  == value )  )   ) &
               then
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
!                 Null values are not allowed in BETWEEN expressions.
!
                  if (      ( mtexpr(first) == nil ) &
                       .or. ( mtexpr(third) == nil )  )  then

                     error  = .true.
                     prserr = 'NULL values are not allowed in ' // &
                              'BETWEEN or NOT BETWEEN clauses.'
                     return

                  end if

!
!                 Hook up the name or value descriptors.
!
                  call lnkilb ( mtexpr(first), mtexpr(third), dspool )

                  mtcode(first)  =  btwexp

                  call lnkfsl ( second, third, mtpool )

                  nmeta          =  nmeta - 2

!
!                 Decide the next state.
!
                  state  =  reducd

               else if (      (        mtcode(second) > 0       ) &
                        .and. (      ( mtcode(third)  == name  ) &
                                .or. ( mtcode(third)  == value ) )   ) &
               then
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
                  if ( lnknfn(rlpool) < 1 ) then
                     error  = .true.
                     prserr = 'Relation table is full.'
                     return
                  end if

                  call lnkan ( rlpool, newrel )

                  rels(1,newrel) = mtexpr(first )
                  rels(2,newrel) = mtcode(second)
                  rels(3,newrel) = mtexpr(third )

                  call lnkfsl ( second, third, mtpool )
                  nmeta  =  nmeta - 2

!
!                 Now allocate an entry in the conjunction pool
!                 and make this entry point to the relation table
!                 entry.
!
                  if ( lnknfn(cjpool) < 1 ) then
                     error  = .true.
                     prserr = 'Conjunction table is full.'
                     return
                  end if

                  call lnkan ( cjpool, newcj )

                  cjptrs(newcj)  =  newrel

!
!                 Now allocate an entry in the disjunction pool
!                 and make this entry point to the conjunction pool
!                 entry.
!
                  if ( lnknfn(djpool) < 1 ) then
                     error  = .true.
                     prserr = 'Disjunction table is full.'
                     return
                  end if

                  call lnkan ( djpool, newdj )

                  djptrs(newdj)  =  newcj

!
!                 Change the type of the first meta-token to EXPR and
!                 have that meta-token point to this table entry.  Bag
!                 the other two meta-tokens.
!
                  mtcode(first)  =  expr
                  mtexpr(first)  =  newdj

!
!                 Decide the next state.
!
                  state  =  reducd

               else if (      ( mtcode(second) == btween ) &
                         .or. ( mtcode(second) == notbtw )  ) then
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
                  if (       ( mtcode(second) == btween ) &
                       .and. ( mtcode(third)  == btwexp )  ) then
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
                     do i = 1, 4

                        if (  ( i == 1 ) .or. ( i == 3 )  ) then

                           k  =  mtexpr(third)

                        else
!
!                          We need the descriptor pointer for the RHS
!                          item.  This descriptor is linked to the tail
!                          of the descriptor for the LHS item.  The
!                          number of nodes to skip over depends on
!                          whether the LHS item is a name or value.
!
                           k  =  mtexpr(third)

                           if ( dscbuf(dscsiz,k) == ident ) then
                              skip = 1
                           else
                              skip = 0
                           end if

                           do j = 1, skip + 1
                              k  =  lnknxt( k, dspool )
                           end do

                        end if

                        if ( lnknfn(rlpool) < 1 ) then
                           error  = .true.
                           prserr = 'Relation table is full.'
                           return
                        end if

                        call lnkan ( rlpool, rel(i) )

                        rels( 1, rel(i) ) = mtexpr(first)

                        if ( ( i == 1 ) .or. ( i == 4 )  ) then
                           rels( 2, rel(i) ) = ge
                        else
                           rels( 2, rel(i) ) = le
                        end if

                        rels( 3, rel(i) ) = k

                        if ( lnknfn(cjpool) < 1 ) then
                           error  = .true.
                           prserr = 'Conjunction table is full.'
                           return
                        end if

                        call lnkan ( cjpool, cj(i) )

                        cjptrs(cj(i))  =  rel(i)

                     end do

!
!                    Link the conjunction nodes to form the two
!                    conjunctions shown above.
!
                     call lnkila ( cj(1), cj(2), cjpool )
                     call lnkila ( cj(3), cj(4), cjpool )

!
!                    Allocate disjunction pool entries and make them
!                    point to the two respective conjunctions.
!
                     do i = 1, 2

                        if ( lnknfn(djpool) < 1 ) then
                           error  = .true.
                           prserr = 'Disjunction table is full.'
                           return
                        end if

                        call lnkan ( djpool, dj(i) )
                        djptrs(dj(i))  =  cj( 2*i - 1 )

                     end do

!
!                    Finally, link the disjunction pool entries, and
!                    create an <expression> meta-token.  Free the unused
!                    meta-tokens.
!
                     call lnkila ( dj(1), dj(2), djpool )

                     mtcode(first)  =  expr
                     mtexpr(first)  =  dj(1)

                     call lnkfsl ( second, third, mtpool )
                     nmeta  =  nmeta - 2

!
!                    Decide the next state.
!
                     state  =  reducd

                  else if (       ( mtcode(second) == notbtw ) &
                            .and. ( mtcode(third)  == btwexp )  ) then
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
                     do i = 1, 4

                        if (  ( i == 1 ) .or. ( i == 3 )  ) then

                           k  =  mtexpr(third)

                        else
!
!                          We need the descriptor pointer for the RHS
!                          item.  This descriptor is linked to the tail
!                          of the descriptor for the LHS item.  The
!                          number of nodes to skip over depends on
!                          whether the LHS item is a name or value.
!
                           k  =  mtexpr(third)

                           if ( dscbuf(dscsiz,k) == ident ) then
                              skip = 1
                           else
                              skip = 0
                           end if

                           do j = 1, skip + 1
                              k  =  lnknxt( k, dspool )
                           end do

                        end if

                        if ( lnknfn(rlpool) < 1 ) then
                           error  = .true.
                           prserr = 'Relation table is full.'
                           return
                        end if

                        call lnkan ( rlpool, rel(i) )

                        rels( 1, rel(i) ) = mtexpr(first)

                        if ( i <= 2 ) then
                           rels( 2, rel(i) ) = lt
                        else
                           rels( 2, rel(i) ) = gt
                        end if

                        rels( 3, rel(i) ) = k

                        if ( lnknfn(cjpool) < 1 ) then
                           error  = .true.
                           prserr = 'Conjunction table is full.'
                           return
                        end if

                        call lnkan ( cjpool, cj(i) )

                        cjptrs(cj(i))  =  rel(i)

                     end do

!
!                    Link the conjunction nodes to form the two
!                    conjunctions shown above.
!
                     call lnkila ( cj(1), cj(2), cjpool )
                     call lnkila ( cj(3), cj(4), cjpool )

!
!                    Allocate disjunction pool entries and make them
!                    point to the two respective conjunctions.
!
                     do i = 1, 2

                        if ( lnknfn(djpool) < 1 ) then
                           error  = .true.
                           prserr = 'Disjunction table is full.'
                           return
                        end if

                        call lnkan ( djpool, dj(i) )
                        djptrs(dj(i))  =  cj( 2*i - 1 )

                     end do

!
!                    Finally, link the disjunction pool entries, and
!                    create an <expression> meta-token.  Free the unused
!                    meta-tokens.
!
                     call lnkila ( dj(1), dj(2), djpool )

                     mtcode(first)  =  expr
                     mtexpr(first)  =  dj(1)

                     call lnkfsl ( second, third, mtpool )
                     nmeta  =  nmeta - 2
!
!                    Decide the next state.
!
                     state  =  reducd

                  else if (      ( mtcode(third) == name  ) &
                            .or. ( mtcode(third) == value )  )  then
!
!                    If the third meta-token is anything other than
!                    <BETWEEN expr>, we'll have to parse the portion of
!                    the query following the BETWEEN keyword before
!                    reducing the <BETWEEN> or <NOT BETWEEN> expression.
!
                     start   =  third
                     retcnd  =  reduce
                     state   =  push

                  else

                     error  =  .true.
                     prserr = 'Token following BETWEEN operator is ' // &
                              'invalid.'
                     return

                  end if

               else if ( mtcode(second) > 0 ) then
!
!                 The third meta-token is in the wrong place at the
!                 wrong time.
!
                  error  =  .true.
                  prserr =  'Token sequence must be followed '      // &
                            'by a value.'

                  return

               else
!
!                 The second meta-token is supposed to be a comparison
!                 operator, but it's actually something else.
!
                  error  =  .true.
                  prserr =  'Token must be followed by a '          // &
                            'comparison operator.'

                  return

               end if

            else if ( mtcode(first) == expr ) then
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
               if ( second <= 0 ) then
!
!                 This is the last state we pass through
!                 before exiting the loop.  However, some syntax errors
!                 can get us here as well.
!
                  if (  ( level > 1 ) .or. ( nmeta > 1 )  ) then

                     error  = .true.
                     prserr = 'More tokens expected.'
                     return

                  end if

                  state = term

               else if ( mtcode(second) == rgroup ) then
!
!                 We've reached the end of a `parenthesized'
!                 expression.
!
                  if (       ( level         > 1      ) &
                       .and. ( popcnd(level) == endgrp )  ) then
!
!                    Time to pop the state.
!
                     state = pop

                  else
!
!                    There should not be a right grouper here.
!
                     error  =  .true.
                     prserr = 'Unexpected right parenthesis '      // &
                              'found.'
                     return

                  end if

!
!              In all other cases, there must be at least three
!              meta-tokens here.  Make sure there are.
!
               else if ( third <= 0 ) then

                  error  =  .true.
                  prserr =  'More tokens expected.'
                  return

!
!              Take care of the cases that will require reducing a sub-
!              expression before reducing the current expression.
!
               else if (      ( mtcode(third) == ident  ) &
                         .or. ( mtcode(third) == name   ) &
                         .or. ( mtcode(third) == not    )  ) then

                  start   =  third
                  retcnd  =  reduce
                  state   =  push

               else if ( mtcode(third) == lgroup ) then
!
!                 We'll have to push our state before continuing.
!
                  start   =  fourth
                  retcnd  =  endgrp
                  state   =  push

!
!              Now continue with the interesting cases.
!
               else if (       ( mtcode(first)  == expr ) &
                         .and. ( mtcode(second) == or   ) &
                         .and. ( mtcode(third)  == expr )  )  then
!
!                 We have a disjunction of two normalized
!                 expressions.  We're not ready to perform a
!                 reduction yet; we need to see whether there's
!                 a higher priority operator, namely AND, on the
!                 right of the second expression.
!
                  donow = .true.

                  if ( fourth > 0 ) then

                     if ( mtcode(fourth) == and ) then
!
!                       The third token is already spoken for:
!                       the expression involving the operator
!                       to its right must be processed first.
!
                        donow = .false.

                     end if

                  end if

                  if ( donow ) then
!
!                    This is an easy case to handle:
!                    we can form the resulting normalized
!                    expression by just linking together the two
!                    lists in the disjunction table.
!
                     dj(1) = mtexpr(first)
                     dj(2) = mtexpr(third)

                     call lnkilb ( dj(1), dj(2), djpool )
!
!                    The first meta-token will point to the resulting
!                    normalized expression; we'll discard the other
!                    two meta-tokens.
!
                     call lnkfsl ( second, third, mtpool )
                     nmeta = nmeta - 2

!
!                    MTEXPR(FIRST) and MTCODE(FIRST) are already
!                    set correctly.  All we need to do is determine
!                    our next state.  The next state defaults to
!                    PARSE; the other possibility is POP.
!
                     state  =  reducd

                  else
!
!                    We'll have to reduce the expression on the right
!                    of the third meta-token before coming back to
!                    this expression.  Get ready to push our state.
!
!                    The condition that must be met in order to pop our
!                    state will be that we've performed a reduction.
!
                     retcnd = reduce
                     start  = third
                     state  = push

                  end if
!
!                 Either we've reduced an OR expression, in which case
!                 the state has been set to PARSE or POP, or we've
!                 found a sub-expression that must be reduced before
!                 we attack the current expression, in which case the
!                 state has been set to PUSH.
!

               else if (       ( mtcode(first)  == expr ) &
                         .and. ( mtcode(second) == and  ) &
                         .and. ( mtcode(third)  == expr )  )  then
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
!                    EXPR2 =        ( B11 and B12 and ... )
!                                or ( B21 and B22 and ... )
!                                              .
!                                              .
!                                              .
!                                or ( BN1 and BN2 and ... )
!
!                 Then ( EXPR1 and EXPR2 ) =
!
!                        or       {  (     ( AI1 and AI2 and ... )
!                    I = 1,...,M       and ( BJ1 and BJ2 and ... ) )  }
!                    J = 1,...,N
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
                  dj(1)    = mtexpr(first)
                  djtail = nil

                  do while ( dj(1) > 0 )

                     dj(2) = mtexpr(third)

                     do while ( dj(2) > 0 )
!
!                       Allocate a new disjunction table entry,
!                       and create a new conjunction that represents
!                       the conjunction of the conjunction lists
!                       pointed to by DJ(1) and DJ(2).
!
                        if ( lnknfn(djpool) < 1 ) then
                           error  = .true.
                           prserr = 'Disjunction table is full.'
                           return
                        end if

                        call lnkan ( djpool, newdj )

!
!                       Make copies of the conjunction lists pointed
!                       to by DJ(1) and DJ(2).
!
                        cj(1) = djptrs(dj(1))
                        tail  = nil

                        do while ( cj(1) > 0 )

                           if ( lnknfn(cjpool) < 1 ) then
                              error  = .true.
                              prserr = 'Conjunction table is full.'
                              return
                           end if

                           call lnkan  (  cjpool, newcj          )
                           call lnkila (  tail,   newcj, cjpool  )
                           tail         = newcj
                           cjptrs(tail) = cjptrs(cj(1))

                           cj(1)        = lnknxt ( cj(1), cjpool )

                        end do

                        head1 = lnkhl( tail, cjpool )

                        cj(2) = djptrs(dj(2))
                        tail  = nil

                        do while ( cj(2) > 0 )

                           if ( lnknfn(cjpool) < 1 ) then
                              error  = .true.
                              prserr = 'Conjunction table is full.'
                              return
                           end if

                           call lnkan  (  cjpool, newcj          )
                           call lnkila (  tail,   newcj, cjpool  )
                           tail         = newcj
                           cjptrs(tail) = cjptrs(cj(2))

                           cj(2)        = lnknxt ( cj(2), cjpool )

                        end do

                        head2 = lnkhl( tail, cjpool )

!
!                       Now link these copies and make NEWDJ point to
!                       the resulting list.
!
                        call lnkilb ( head1, head2, cjpool )

                        djptrs(newdj) = head1

!
!                       Link NEWDJ in at the tail of the disjunction
!                       list.
!
                        call lnkila  ( djtail, newdj, djpool )
                        djtail = newdj

                        dj(2)  = lnknxt ( dj(2),   djpool )

                     end do

                     dj(1) = lnknxt ( dj(1), djpool )

                  end do

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
                  call lnkilb ( mtexpr(first), mtexpr(third), djpool )

                  djnode = mtexpr(first)

                  do while ( djnode > 0 )
!
!                    Free the conjunction list pointed to by DJNODE.
!
                     cjnode = djptrs(djnode)

                     call lnkfsl (  cjnode, &
                                    lnktl(cjnode,cjpool), &
                                    cjpool                )

                     djnode = lnknxt( djnode, djpool )

                  end do
!
!                 Free the disjunction list that starts with
!                 MTEXPR(FIRST).
!
                  call lnkfsl (  mtexpr(first), &
                                 lnktl( mtexpr(first), djpool ), &
                                 djpool                          )

!
!                 NEWDJ is the tail node of the list of disjunctions
!                 we've just finished.  The first meta-token should
!                 point to the head of this disjunction list.
!
                  mtexpr(first)  =  lnkhl ( newdj, djpool )

!
!                 We no longer need the other two meta-tokens.
!
                  call lnkfsl ( second, third, mtpool )
                  nmeta = nmeta - 2

!
!                 Decide the next state.
!
                  state  =  reducd

               else
!
!                 There are no other valid cases in which the first
!                 meta-token is an expression.
!
                  error  =  .true.
                  prserr =  'Unexpected token found following '       // &
                            'valid expression.'
                  return

               end if

            else if ( mtcode(first) == not ) then
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
               if ( second <= 0 ) then

                  error  = .true.
                  prserr = 'Tokens were missing from logical '        // &
                           'expression.'
                  return

               else if ( mtcode(second) == lgroup ) then
!
!                 We'll have to push our state before continuing.
!
                  start   =  third
                  retcnd  =  endgrp
                  state   =  push

               else if (      ( mtcode(second) == not   ) &
                         .or. ( mtcode(second) == ident ) &
                         .or. ( mtcode(second) == name  )  )  then

                  start   =  second
                  retcnd  =  reduce
                  state   =  push

               else if ( mtcode(second) == expr ) then
!
!                 We have the negation of a normalized expression. Since
!                 the NOT operator has higher precedence than any other,
!                 we need not concern ourselves with the token on the
!                 right of the expression.
!
!                 This case requires application of DeMorgan's laws to
!                 convert the expression to a normalized form.
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
                  djnode = mtexpr(second)

                  cjnode = djptrs(djnode)
                  tail   = nil

                  do while ( cjnode > 0 )
!
!                    Create a new singleton disjunction list
!                    that points to the relation pointed to by
!                    CJNODE.
!
                     if ( lnknfn(djpool) < 1 ) then
                        error  = .true.
                        prserr = 'Disjunction table is full.'
                        return
                     end if

                     call lnkan ( djpool, newdj )

                     if ( lnknfn(cjpool) < 1  ) then
                        error  = .true.
                        prserr = 'Conjunction table is full.'
                        return
                     end if

                     call lnkan ( cjpool, newcj )

                     cjptrs(newcj) = cjptrs(cjnode)
                     djptrs(newdj) = newcj
!
!                    Now link the new singleton disjunction list in
!                    at the tail of the disjunction list that
!                    parallels the conjunction list we're currently
!                    traversing.
!
                     call lnkila ( tail, newdj, djpool )
                     tail   = newdj

                     cjnode = lnknxt ( cjnode, cjpool )

                  end do

!
!                 Keep track of the head of the new normalized
!                 expression.
!
                  exprhd = lnkhl ( tail, djpool )

!
!                 Now, for every remaining conjunction in the original
!                 expression, we'll form the normalized expression
!                 resulting from the conjunction of its negation and
!                 of our cumulative normalized expression.  As mentioned
!                 before, we won't negate the comparison operators
!                 just yet.
!
                  djnode = lnknxt ( djnode, djpool )

                  do while ( djnode > 0 )
!
!                    Loop through our existing cumulative
!                    expression and the latest conjunction, forming
!                    all pairwise conjunctions.
!
                     dj(1)    = exprhd
                     djtail = nil

                     do while ( dj(1) > 0 )

                        cj(2) = djptrs(djnode)

                        do while ( cj(2) > 0 )
!
!                          Make a copy of the conjunction list pointed
!                          to by DJPTRS(DJ(1)).
!
                           cjnode = djptrs(dj(1))
                           tail   = nil

                           do while ( cjnode > 0 )

                              if ( lnknfn(cjpool) < 1 ) then
                                 error  = .true.
                                 prserr = 'Conjunction table is full.'
                                 return
                              end if

                              call lnkan  ( cjpool, newcj         )
                              call lnkila ( tail,   newcj, cjpool )

                              cjptrs(newcj)  = cjptrs(cjnode)
                              tail           = newcj
                              cjnode         = lnknxt ( cjnode, cjpool )

                           end do

                           cj(1) = lnkhl ( tail, cjpool )

!
!                          Allocate a new conjunction table entry for
!                          the conjunction of the expressions
!                          pointed to by CJ(1) and CJ(2).  Allocate a
!                          new disjunction table entry to point to this
!                          new conjunction.
!
                           if ( lnknfn(cjpool) < 1 ) then
                              error  = .true.
                              prserr = 'Conjunction table is full.'
                              return
                           end if

                           call lnkan ( cjpool, newcj )
                           cjptrs(newcj)  =  cjptrs(cj(2))

                           if ( lnknfn(djpool) < 1  ) then
                              error  = .true.
                              prserr = 'Disjunction table is full.'
                              return
                           end if

                           call lnkan ( djpool, newdj )

!
!                          Hook everything up.
!
                           call lnkilb ( cj(1), newcj, cjpool )

                           djptrs(newdj) =  cj(1)

                           call lnkila ( djtail, newdj, djpool )
                           djtail        =  newdj

                           cj(2)         =  lnknxt ( cj(2), cjpool )

                        end do

                        dj(1)    = lnknxt ( dj(1), djpool )

                     end do

!
!                    Before going on, clean up the conjunction and
!                    disjunction pool entries used by our last
!                    version of the cumulative expression.
!
                     dj(1) = exprhd

                     do while ( dj(1) > 0 )

                        cj(1) = djptrs(dj(1))
                        cj(2) = lnktl ( cj(1), cjpool )

                        call lnkfsl ( cj(1), cj(2), cjpool )

                        dj(1) = lnknxt ( dj(1), djpool )

                     end do

                     call lnkfsl (  exprhd, &
                                    lnktl( exprhd, djpool ), &
                                    djpool                    )

!
!                    Set EXPRHD to be the head of our updated,
!                    cumulative expression.  Start to work on the
!                    next conjunction.
!
                     exprhd = lnkhl  ( djtail, djpool )
                     djnode = lnknxt ( djnode, djpool )

                  end do

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
                  call ssizei ( maxrel, relset )

                  djnode = mtexpr(second)

                  do while ( djnode > 0 )

                     cjnode = djptrs(djnode)

                     do while ( cjnode > 0 )

                        relptr = cjptrs(cjnode)
                        call insrti ( relptr, relset )

                        cjnode = lnknxt( cjnode, cjpool )

                     end do

                     djnode = lnknxt ( djnode, djpool )

                  end do

                  do i = 1, cardi(relset)

                     relptr          =  relset( i )
                     j               =  isrchi( rels(2,relptr), &
                                                nrelop, &
                                                cmpcde         )
                     rels(2,relptr)  =  cmpneg( j )

                  end do

!
!                 Set the pointer of the first meta-token to point
!                 to our normalized expression, and change the
!                 meta-token's code to <expr>.
!
                  mtexpr(first) = exprhd
                  mtcode(first) = expr

!
!                 Get rid of the second meta-token, and determine the
!                 next state.
!
                  call lnkfsl ( second, second, mtpool )
                  nmeta  =  nmeta - 1

                  state  =  reducd

               else
!
!                 The second token is invalid in this context.
!
                  error  = .true.
                  prserr = 'Token following NOT operator was invalid.'
                  return

               end if
!
!              This is the end of the NOT case.
!

            else if ( mtcode(first) == lgroup ) then
!
!              We're looking at the start of a `parenthesized'
!              sub-expression.
!
!              Push our state, and start parsing at meta-token
!              SECOND.  The condition for popping our state will be
!              that we encounter a right grouper.
!
               retcnd = endgrp
               start  = second
               state  = push

            else
!
!              Only a syntax error could get us here.
!
               error  =  .true.
               prserr = 'Unexpected token found.'
               return

            end if

!
!           This is the end of the code for the PARSE state.  We've
!           determined the next parsing state at this point.
!

         else if ( state == reducd ) then
!
!           A reduction has been done.  Decide the next state.
!
            state  =  reducd

            if ( popcnd(level) == reduce ) then
               state         = pop
            else
               mstart(level) = first
               state         = parse
            end if

         else if ( state == push ) then
!
!           Increment the stack level, and save the current
!           starting point and pop condition.
!
            level  =  level + 1

            if ( level > maxstk ) then
               error          = .true.
               prserr         = 'Stack is full'
               state          =  term
            else
               mstart(level)  =  start
               popcnd(level)  =  retcnd
               state          =  parse
            end if

         else if ( state == pop ) then
!
!           If we can, pop the state.
!
            if ( level > 1 ) then

               if ( popcnd(level) == endgrp ) then
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
                  prev   =  lnkprv ( first, mtpool )

                  if ( mstart(level-1) == prev ) then
                     mstart(level-1) = first
                  end if

                  if ( metahd == prev ) then
                     metahd = first
                  end if

                  call lnkfsl ( prev,   prev,   mtpool )
                  call lnkfsl ( second, second, mtpool )

                  nmeta  =  nmeta - 2

               end if

               level = level - 1
               state = parse

            else
               error  =  .true.
               prserr =  'Syntax error:  badly formed WHERE clause.'
               return
            end if

         end if

!
!        We've considered all states.
!

      end do

!
!     At this point, there should be a single meta-token of type EXPR.
!     This meta-token should point to a normalized expression.  We'll
!     set the encoded query to represent this expression.  For each
!     constraint, we'll add a constraint descriptor to the encoded
!     query.  We'll also update the count of constraints, the count of
!     conjunctions, and we'll add a list of conjunction sizes.
!
      djnode = mtexpr(first)
      nconj  = 0
      nrels  = 0

      do while ( djnode > 0 )

         nconj         =  nconj + 1
         sizes(nconj)  =  0
         cjnode        =  djptrs(djnode)

         do while ( cjnode > 0 )

            nrels          =  nrels        + 1
            sizes (nconj)  =  sizes(nconj) + 1
            relptr         =  cjptrs(cjnode)

            tabptr         =  rels(1,relptr)
            op             =  rels(2,relptr)
            rhsptr         =  rels(3,relptr)

!
!           Add a constraint descriptor to the encoded query.  The
!           structure of these descriptors is documented in the include
!           file for encoded query parameters.
!
!           First, save space for the constraint type.  We'll fill this
!           in after finding out what's on the right hand side.
!
            call appndi ( 0, eqryi )
            k  =  cardi ( eqryi )

!
!           Next, add name descriptors for the table and column on
!           the left-hand side.  These descriptors are linked and
!           pointed to by NAMPTR.
!
            do i = 1, eqvdsz
               call appndi (  dscbuf( i, tabptr ),  eqryi  )
            end do

            colptr  =  lnknxt( tabptr, dspool )

            do i = 1, eqvdsz
               call appndi (  dscbuf( i, colptr ),  eqryi  )
            end do

!
!           What happens next depends on whether the query has a null
!           value on the right hand side.  This is indicated by the
!           relation's value pointer being NIL.
!
            if ( rhsptr == nil ) then
!
!              For constraints involving null values, we change the
!              operator to ISNULL or NOTNUL as appropriate.
!
               if ( op == eq ) then
                  op  =  isnull

               else if ( op == ne ) then
                  op  =  notnul

               else

                  error  =  .true.
                  prserr =  'NULL values can only be used with the ' // &
                            'operators "IS NULL", "NOT NULL", '      // &
                            'or equivalents.'
                  return

               end if

!
!              Set the operator code.
!
               call appndi ( op, eqryi )

!
!              Pad the constraint descriptor up to the full length.
!
               do i = 1, 2*eqvdsz
                  call appndi ( 0, eqryi )
               end do

!
!              Set the descriptor's type by updating the reserved
!              location.
!
               eqryi(k)  =  eqval

            else
!
!              For `normal' constraints, that is, constraints that don't
!              involve null values, we set the operator code, then
!              fill in the information describing the RHS of the
!              constraint.
!
               call appndi ( op, eqryi )

               if (  dscbuf( dscsiz, rhsptr ) == value ) then
!
!                 The RHS contains a value.  Append the descriptor
!                 for the value, then pad the constraint descriptor.
!
                  do i = 1, eqvdsz
                     call appndi (  dscbuf( i, rhsptr ),  eqryi  )
                  end do

                  do i = 1, eqvdsz
                     call appndi (  0,  eqryi  )
                  end do

!
!                 Set the descriptor's type by updating the reserved
!                 location.
!
                  eqryi(k)  =  eqval

               else
!
!                 The RHS contains a column name.  Append the
!                 descriptors for the table and column.
!
                  do i = 1, eqvdsz
                     call appndi (  dscbuf( i, rhsptr ),  eqryi  )
                  end do

                  colptr  =  lnknxt( rhsptr, dspool )

                  do i = 1, eqvdsz
                     call appndi (  dscbuf( i, colptr ),  eqryi  )
                  end do

!
!                 Set the descriptor's type by updating the reserved
!                 location.
!
                  eqryi(k)  =  eqcol

               end if

            end if
!
!           We've updated the encoded query to reflect the current
!           constraint relation.
!
            cjnode = lnknxt ( cjnode, cjpool )

         end do
!
!        We've set the array element SIZES(NCONJ).
!
         djnode = lnknxt ( djnode, djpool )

      end do

!
!     Set the counts of constraints and conjunctions in the encoded
!     query.
!
      call zzekweqi ( 'NUM_CONSTRAINTS',  nrels, eqryi )
      call zzekweqi ( 'NUM_CONJUNCTIONS', nconj, eqryi )

!
!     Add the conjunction size list to the encoded query.
!
      do i = 1, nconj
         call appndi ( sizes(i), eqryi )
      end do

      return
      end
