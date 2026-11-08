!$Procedure      ZZEKPARS ( EK, parse tokenized EK query )

      subroutine zzekpars (  query,   ntoken,  lxbegs,  lxends, &
                             tokens,  values,  numvls,  chrbuf, &
                             chbegs,  chends,  eqryi,   eqryc, &
                             eqryd,   error,   prserr          )

      implicit none

! $ Abstract
!
!     Parse an EK query that has been scanned and tokenized.
!     Represent the result as an encoded EK query.
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

      include 'ektokn.inc'
      include 'ekkeyw.inc'
      include 'ekbool.inc'
      include 'ektype.inc'
      include 'ekopcd.inc'
      include 'ekquery.inc'
      include 'ekqlimit.inc'

      integer               lbcell
      parameter           ( lbcell = -5 )

      character*(*)         query
      integer               ntoken
      integer               lxbegs ( * )
      integer               lxends ( * )
      integer               tokens ( * )
      integer               values ( * )
      double precision      numvls ( * )
      character*(*)         chrbuf
      integer               chbegs ( * )
      integer               chends ( * )
      integer               eqryi  ( lbcell : * )
      character*(*)         eqryc
      double precision      eqryd  ( * )
      logical               error
      character*(*)         prserr

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
      integer               cardi

      logical               failed
      logical               return


!
!     Local parameters
!

!
!     State parameters
!
      integer               from
      parameter           ( from    =            0 )

      integer               frmtab
      parameter           ( frmtab  =   from   + 1 )

      integer               frmcnt
      parameter           ( frmcnt  =   frmtab + 1 )

      integer               frmals
      parameter           ( frmals  =   frmcnt + 1 )

      integer               where
      parameter           ( where   =   frmals + 1 )

      integer               order
      parameter           ( order   =   where  + 1 )

      integer               ordrby
      parameter           ( ordrby  =   order  + 1 )

      integer               ordtab
      parameter           ( ordtab  =   ordrby + 1 )

      integer               ordnam
      parameter           ( ordnam  =   ordtab + 1 )

      integer               ordcol
      parameter           ( ordcol  =   ordnam + 1 )

      integer               ordsns
      parameter           ( ordsns  =   ordcol + 1 )

      integer               selkey
      parameter           ( selkey  =   ordsns + 1 )

      integer               select
      parameter           ( select  =   selkey + 1 )

      integer               seltab
      parameter           ( seltab  =   select + 1 )

      integer               selnam
      parameter           ( selnam  =   seltab + 1 )

      integer               selcol
      parameter           ( selcol  =   selnam + 1 )

      integer               term
      parameter           ( term    =   selcol + 1 )

!
!     Other local parameters
!
      integer               maxrel
      parameter           ( maxrel =  200 )

      integer               typlen
      parameter           ( typlen =   32 )

      integer               keylen
      parameter           ( keylen =   32 )

!
!     Local variables
!
      character*(typlen)    errtyp
      character*(keylen)    expkey

      integer               alsdsc ( eqvdsz )
      integer               b
      integer               coldsc ( eqvdsz )
      integer               e
      integer               i
      integer               j
      integer               l
      integer               lxb
      integer               lxe
      integer               namdsc ( eqvdsz )
      integer               ncnstr
      integer               norder
      integer               nsel
      integer               ntabs
      integer               state
      integer               tabdsc ( eqvdsz )
      integer               token
      integer               toknum
      integer               valdsc ( eqvdsz )

      logical               fnd

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'ZZEKPARS' )
      end if

!
!     Initialize the encoded query each time, for safety.
!
      call zzekqini ( eqimin, maxqnm, eqryi, eqryc, eqryd )

      if ( failed() ) then
         error  =  .true.
         prserr =  'SPICE(BUG):  encoded query init failed.'
         call chkout ( 'ZZEKPARS' )
         return
      end if

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
      call zzektloc ( tkkey,   kwfrom,  ntoken,  tokens,  values, &
                      toknum,  fnd                                )

      if ( .not. fnd ) then

         error   =  .true.
         errtyp  =  'FROM_NOT_FOUND'
         state   =   term

      else

         state   =  from
         ntabs   =  0
         nsel    =  0
         ncnstr  =  0
         norder  =  0
         error   =  .false.
         prserr  =  ' '
         errtyp  =  ' '

      end if


      do while ( state .ne. term )
!
!        Advance to the next token, if there is one.
!
         toknum  =  toknum + 1

         if ( toknum .gt. ntoken ) then
!
!           We're out of tokens.  Set the token value to indicate
!           `end of query'.
!
            token  =  tkeoq

         else

            token  =  tokens(toknum)

         end if

!
!        Perform semantic actions based on the state and current token.
!
         if ( state .eq. from ) then
!
!           We expect to see an identifier representing a table name.
!           No other tokens are allowed.
!
            if ( token .eq. tkid ) then
!
!              We've found a table name (as far as we can tell at
!              this point).  Make sure we haven't exceeded the limit
!              for table names; if not, add the appropriate information
!              to the encoded query.
!
               ntabs  =   ntabs  +  1


               if ( ntabs .gt. maxtab ) then

                  error   =  .true.
                  errtyp  =  'TOO_MANY_TABLES'
                  state   =  term

               else

                  i       =   values(toknum)
                  b       =   chbegs(i)
                  e       =   chends(i)
                  l       =   e  -  b   +  1
                  lxb     =   lxbegs(toknum)
                  lxe     =   lxends(toknum)


                  call zzekinqc ( chrbuf( b : e ), &
                                  l, &
                                  lxb, &
                                  lxe, &
                                  eqryi, &
                                  eqryc, &
                                  tabdsc          )

!
!                 Append the table descriptor to the integer part of the
!                 query.
!
                  do j = 1, eqvdsz
                     call appndi ( tabdsc(j), eqryi )
                  end do

!
!                 Add a place-holder value descriptor to reserve
!                 space for an alias descriptor for this table.  If an
!                 actual alias is supplied, we'll update this
!                 descriptor.
!
                  call cleari ( eqvdsz, alsdsc )
                  alsdsc(eqdtyp)  =  chr

                  do j = 1, eqvdsz
                     call appndi ( alsdsc(j), eqryi )
                  end do

!
!                 Update the table count in the encoded query.
!
                  call zzekweqi ( 'NUM_TABLES', ntabs, eqryi )


                  state  =  frmtab

               end if


            else if ( token .eq. tkeoq ) then

               error   =  .true.
               errtyp  =  'MORE_TOKENS_EXP'
               state   =  term


            else
!
!              We've got the wrong kind of token here.
!
               error   =  .true.
               errtyp  =  'TABLE_EXP'
               state   =  term

            end if
!
!           State is a member of {FRMTAB, TERM}.
!

         else if ( state .eq. frmtab ) then
!
!           We should see a comma, an alias, one of the SELECT,
!           WHERE or ORDER keywords, or the end of the query.
!
            if ( token .eq. tkeoq ) then
!
!              We're out of tokens.  It's time to parse the
!              WHERE clause.
!
               state   =  where


            else if ( token .eq. tkcoma ) then
!
!              It's time to look for another table name.
!
               state  =  from


            else if ( token .eq. tkid ) then
!
!              We've got an alias.  Add this string to the encoded
!              query.
!
               i   =   values(toknum)
               b   =   chbegs(i)
               e   =   chends(i)
               l   =   e  -  b   +  1
               lxb =   lxbegs(toknum)
               lxe =   lxends(toknum)

               call zzekinqc ( chrbuf( b : e ), &
                               l, &
                               lxb, &
                               lxe, &
                               eqryi, &
                               eqryc, &
                               alsdsc                        )

!
!              Update the place-holder alias descriptor in the integer
!              part of the query.
!
               call movei ( alsdsc, &
                            eqvdsz, &
                            eqryi(  cardi(eqryi) - eqvdsz + 1  )   )


               state  =  frmals


            else if (  token .eq.  tkkey  )  then
!
!              The last table name in the FROM clause is followed by
!              a keyword.  SELECT, WHERE and ORDER are the only valid
!              possibilities.
!
               if (       ( values(toknum) .ne. kwwher ) &
                    .and. ( values(toknum) .ne. kwsel  ) &
                    .and. ( values(toknum) .ne. kwordr )  ) then
!
!                 We've got a keyword we don't want here.
!
                  error  =  .true.
                  errtyp =  'BAD_KEYWORD'
                  state  =  term

               else
!
!                 Parse the WHERE clause.
!
                  state  =  where

               end if


            else
!
!              We've got the wrong kind of token altogether.
!
               error  =  .true.
               errtyp =  'ALIAS_EXP'
               state  =  term

            end if

!
!           STATE is a member of {FROM, FRMALS, WHERE, TERM}.
!



         else if ( state .eq. frmals ) then
!
!           We should see a comma, the SELECT, WHERE or ORDER
!           keywords, or the end of the query.
!
            if ( token .eq. tkeoq ) then
!
!              We're out of tokens.  It's time to parse the
!              WHERE clause.
!
               state   =  where


            else if ( token .eq. tkcoma ) then
!
!              It's time to look for another table name.
!
               state  =  from


            else if (  token .eq.  tkkey  )  then
!
!              The last table name in the FROM clause is followed by
!              a keyword.  SELECT, WHERE and ORDER are the only valid
!              possibilities.
!
               if (       ( values(toknum) .ne. kwwher ) &
                    .and. ( values(toknum) .ne. kwsel  ) &
                    .and. ( values(toknum) .ne. kwordr )  ) then
!
!                 We've got a keyword we don't want here.
!
                  error  =  .true.
                  errtyp =  'BAD_KEYWORD'
                  state  =  term

               else
!
!                 Parse the WHERE clause.
!
                  state  =  where

               end if


            else
!
!              We've got the wrong kind of token altogether.
!
               error  =  .true.
               errtyp =  'COMMA_OR_KEY_EXP'
               state  =  term

            end if
!
!           STATE is a member of {FROM, WHERE, TERM}.
!


         else if ( state .eq. selkey ) then
!
!           It's time to parse the SELECT clause.  We'll need to
!           locate the SELECT keyword.
!
            call zzektloc ( tkkey,   kwsel,  ntoken,  tokens,  values, &
                            toknum,  fnd                               )

            if ( .not. fnd ) then

               error   =  .true.
               errtyp  =  'SELECT_NOT_FOUND'
               state   =   term

            else

               state   =  select

            end if


         else if ( state .eq. select ) then
!
!           We must see either the * token, the ALL keyword,
!           or an identifier here.  The identifier may be a lone
!           column name, or it may be a column name qualified by a
!           table name or alias.
!
!           For the moment, we don't support the * or ALL options.
!
            if ( token .eq. tkid ) then
!
!              We've found a name (as far as we can tell at this point).
!              Make sure we haven't exceeded the limit for SELECT
!              column names; if not, store the name string in the
!              encoded query, and save the descriptor until we've
!              figured out whether we're looking at a column name or
!              table name.
!
               nsel  =  nsel  +  1

               if ( nsel.gt. maxsel ) then

                  error   =  .true.
                  errtyp  =  'TOO_MANY_SEL_COLS'
                  state   = term

               else

                  i       =   values(toknum)
                  b       =   chbegs(i)
                  e       =   chends(i)
                  l       =   e  -  b   +  1
                  lxb     =   lxbegs(toknum)
                  lxe     =   lxends(toknum)


                  call zzekinqc ( chrbuf( b : e ), &
                                  l, &
                                  lxb, &
                                  lxe, &
                                  eqryi, &
                                  eqryc, &
                                  namdsc          )

!
!                 Add a place-holder value descriptor to reserve
!                 space for a table descriptor for this name.  If it
!                 turns out that the current name is a table name, we'll
!                 update this descriptor.
!
                  call cleari ( eqvdsz, valdsc )
                  valdsc(eqdtyp)  =  chr

                  do j = 1, eqvdsz
                     call appndi ( valdsc(j), eqryi )
                  end do

!
!                 Update the SELECT column count in the encoded query.
!
                  call zzekweqi ( 'NUM_SELECT_COLS', nsel, eqryi )


                  state  =  selnam

               end if


            else if ( token .eq. tkeoq ) then

               error  =  .true.
               errtyp =  'MORE_TOKENS_EXP'
               state  =  term


            else
!
!              We've got the wrong kind of token here.
!
               error  =  .true.
               errtyp =  'TABLE_OR_COLUMN_EXP'
               state  =  term

            end if
!
!           State is a member of {SELNAM, TERM}.
!


         else if ( state .eq. selnam ) then
!
!           We've seen a SELECT column name, or else the name
!           of a table qualifying a SELECT column name.
!
            if ( token .eq. tkeoq ) then
!
!              The name we picked up was an unqualified column
!              name.  Append the saved name descriptor to the encoded
!              query.
!
               do j = 1, eqvdsz
                  call appndi ( namdsc(j), eqryi )
               end do

               state   =   term


            else if ( token .eq. tkcoma ) then
!
!              The name we picked up was an unqualified column
!              name.  Append the saved name descriptor to the encoded
!              query.  Another name should follow.
!
               do j = 1, eqvdsz
                  call appndi ( namdsc(j), eqryi )
               end do

               state  =  select


            else if ( token .eq. tkdot ) then
!
!              The name we picked up was a table name or alias.  A
!              column name should follow.
!
               state  =  seltab


            else if ( token .eq. tkkey ) then
!
!              We have the last column name in the SELECT clause.
!
!              Append the saved name descriptor to the encoded
!              query.
!
               do j = 1, eqvdsz
                  call appndi ( namdsc(j), eqryi )
               end do

!
!              The last column name in the SELECT clause is followed by
!              a keyword.  FROM, WHERE and ORDER are the only valid
!              possibilities.
!
               if (      ( values(toknum) .eq. kwwher ) &
                    .or. ( values(toknum) .eq. kwfrom ) &
                    .or. ( values(toknum) .eq. kwordr )  ) then
!
!                 We're done with the SELECT clause.
!
                  state  =  term

               else
!
!                 We've got a keyword we don't want here.
!
                  error  =  .true.
                  errtyp =  'BAD_KEYWORD'
                  state  =  term

               end if

            else
!
!              We've got the wrong kind of token here.
!
               error  =  .true.
               errtyp =  'BAD_TOKEN'
               state  =  term

            end if
!
!           STATE is a member of {SELECT, SELTAB, TERM}.
!


         else if ( state .eq. seltab ) then
!
!           We've picked up a qualifying table name for a SELECT
!           column.  We must see a column name here.
!
            if ( token .eq. tkid ) then
!
!              Update the place-holder table name descriptor in the
!              encoded query.
!
               call movei ( namdsc, &
                            eqvdsz, &
                            eqryi(  cardi(eqryi) - eqvdsz + 1  )   )

!
!              Add the column name to the character part of the
!              encoded query.
!
               i   =   values(toknum)
               b   =   chbegs(i)
               e   =   chends(i)
               l   =   e  -  b   +  1
               lxb =   lxbegs(toknum)
               lxe =   lxends(toknum)

               call zzekinqc ( chrbuf( b : e ), &
                               l, &
                               lxb, &
                               lxe, &
                               eqryi, &
                               eqryc, &
                               coldsc           )

!
!              Add the descriptor for the column name to the encoded
!              query.
!
               do j = 1, eqvdsz
                  call appndi ( coldsc(j), eqryi )
               end do


               state  =  selcol


            else if ( token .eq. tkeoq ) then

               error  =  .true.
               errtyp =  'MORE_TOKENS_EXP'
               state  =  term

            else

               error  =  .true.
               errtyp =  'COLUMN_EXP'
               state  =  term

            end if
!
!           STATE is a member of {SELCOL, TERM}.
!


         else if ( state .eq. selcol ) then
!
!           We've picked up a qualified column name.  At this point,
!           we should see a keyword, a comma, or the end of the
!           query.
!
            if ( token .eq. tkkey ) then
!
!              The last column name in the SELECT clause is followed by
!              a keyword.  FROM, WHERE and ORDER are the only valid
!              possibilities.
!
               if (      ( values(toknum) .eq. kwwher ) &
                    .or. ( values(toknum) .eq. kwfrom ) &
                    .or. ( values(toknum) .eq. kwordr )  ) then
!
!                 We're done with the SELECT clause.
!
                  state  =  term

               else
!
!                 We've got a keyword we don't want here.
!
                  error  =  .true.
                  errtyp =  'BAD_KEYWORD'
                  state  =  term

               end if


            else if ( token .eq. tkcoma ) then
!
!              We expect another SELECT column.
!
               state  =  select


            else if ( token .eq. tkeoq ) then
!
!              We're done with the SELECT clause.
!
               state  =  term

            else

               error  =  .true.
               errtyp =  'COMMA_OR_KEY_EXP'
               state  =  term

            end if
!
!           STATE is a member of {SELECT, TERM}.
!


         else if ( state .eq. where ) then
!
!           The WHERE clause is optional.  See whether we have one.  The
!           clause is started by a WHERE keyword.
!
            call zzektloc ( tkkey,   kwwher,  ntoken,  tokens,  values, &
                            toknum,  fnd                               )


            if ( fnd ) then
!
!              We're going to hand off the list of tokens that comprise
!              the WHERE clause of the query to a routine that will
!              parse the tokens and form a list of relational
!              constraints.  Once this is done, all we have to do here
!              is check the validity of the column names and the values
!              used in the constraints.
!
               call zzeknrml ( query, &
                               ntoken, &
                               lxbegs, &
                               lxends, &
                               tokens, &
                               values, &
                               numvls, &
                               chrbuf, &
                               chbegs, &
                               chends, &
                               eqryi, &
                               eqryc, &
                               eqryd, &
                               error, &
                               prserr )


               if ( error ) then

                  errtyp  =   'WHERE_ERROR'
                  state   =   term

               else
!
!                 Parse the ORDER BY clause, if one is present.
!
                  state   =   order

               end if


            else
!
!              Parse the ORDER BY clause, if one is present.
!
               state   =   order

            end if
!
!           STATE is a member of {ORDER, TERM}.
!


         else if ( state .eq. order ) then
!
!           The ORDER BY clause is optional.  See whether we have one.
!           The clause is started by an ORDER keyword.
!
            call zzektloc ( tkkey,   kwordr,  ntoken,  tokens,  values, &
                            toknum,  fnd                               )


            if ( fnd ) then
!
!              The BY keyword should follow the ORDER keyword.
!
               if ( toknum .lt. ntoken ) then

                  toknum  =  toknum + 1

                  if (       (  tokens(toknum) .eq. tkkey ) &
                       .and. (  values(toknum) .eq. kwby  )  ) then
!
!                    We're ready to parse the ORDER BY clause.
!
                     state  =  ordrby

                  else
!
!                    No BY keyword followed the ORDER keyword.
!
                     error  =  .true.
                     errtyp =  'BY_EXPECTED'
                     state  =  term

                  end if


               else
!
!                 We're out of tokens where we shouldn't be.
!
                  error  =  .true.
                  errtyp =  'BY_EXPECTED'
                  state  =  term

               end if


            else
!
!              We're ready to go on to the SELECT clause.
!
               state  =  selkey

            end if
!
!           STATE is a member of {ORDRBY, SELKEY, TERM}.
!



         else if ( state .eq. ordrby ) then
!
!           We must see a name in the order column list here.
!           The name may be a lone column name, or it may be a column
!           name qualified by a table name or alias.
!
            if ( token .eq. tkid ) then
!
!              We've found a name (as far as we can tell at this point).
!              Make sure we haven't exceeded the limit for order-by
!              column names; if not, store the name string in the
!              encoded query, and save the descriptor until we've
!              figured out whether we're looking at a column name or
!              table name.
!
               norder  =  norder  +  1

               if ( norder .gt. maxord ) then

                  error   =  .true.
                  errtyp  =  'TOO_MANY_ORD_COLS'
                  state   = term

               else

                  i       =   values(toknum)
                  b       =   chbegs(i)
                  e       =   chends(i)
                  l       =   e  -  b   +  1
                  lxb     =   lxbegs(toknum)
                  lxe     =   lxends(toknum)


                  call zzekinqc ( chrbuf( b : e ), &
                                  l, &
                                  lxb, &
                                  lxe, &
                                  eqryi, &
                                  eqryc, &
                                  namdsc          )

!
!                 Add a place-holder value descriptor to reserve
!                 space for a table descriptor for this name.  If it
!                 turns out that the current name is a table name, we'll
!                 update this descriptor.
!
                  call cleari ( eqvdsz, valdsc )
                  valdsc(eqdtyp)  =  chr

                  do j = 1, eqvdsz
                     call appndi ( valdsc(j), eqryi )
                  end do

!
!                 Update the order-by column count in the encoded query.
!
                  call zzekweqi ( 'NUM_ORDERBY_COLS', norder, eqryi )


                  state  =  ordnam

               end if


            else if ( token .eq. tkeoq ) then

               error  =  .true.
               errtyp =  'MORE_TOKENS_EXP'
               state  =  term


            else
!
!              We've got the wrong kind of token here.
!
               error  =  .true.
               errtyp =  'TABLE_OR_COLUMN_EXP'
               state  =  term

            end if
!
!           State is a member of {ORDNAM, TERM}.
!


         else if ( state .eq. ordnam ) then
!
!           We've seen an order-by column name, or else the name
!           of a table qualifying an order-by column name.
!
            if ( token .eq. tkeoq ) then
!
!              The name we picked up was an unqualified column
!              name.  Append the saved name descriptor to the encoded
!              query.
!
               do j = 1, eqvdsz
                  call appndi ( namdsc(j), eqryi )
               end do

!
!              Since no ASCENDING or DESCENDING sense keyword was
!              supplied, append the default value ASCENDING to the
!              order-by column descriptor in the encoded query.
!
               call appndi ( eqasnd, eqryi )

!
!              We're done with the ORDER BY clause; go on to parse the
!              SELECT clause.
!
               state   =  selkey


            else if ( token .eq. tkcoma ) then
!
!              The name we picked up was an unqualified column
!              name.  Append the saved name descriptor to the encoded
!              query.  Another name should follow.
!
               do j = 1, eqvdsz
                  call appndi ( namdsc(j), eqryi )
               end do

!
!              Since no ASCENDING or DESCENDING sense keyword was
!              supplied, append the default value ASCENDING to the
!              order-by column descriptor in the encoded query.
!
               call appndi ( eqasnd, eqryi )

               state  = ordrby


            else if ( token .eq. tkdot ) then
!
!              The name we picked up was a table name or alias.  A
!              column name should follow.
!
               state = ordtab


            else if ( token .eq. tkkey ) then
!
!              We have a column name, which may be followed by a
!              keyword indicating the sense of the ordering, or may
!              be followed by a keyword starting a new clause.
!
!              Append the saved name descriptor to the encoded
!              query.
!
               do j = 1, eqvdsz
                  call appndi ( namdsc(j), eqryi )
               end do

!
!              Set the sense descriptor according to the keyword we've
!              picked up.  After this, we're ready to look for another
!              order-by column.
!
               if ( values(toknum) .eq. kwasnd ) then

                  call appndi ( eqasnd, eqryi )

                  state = ordsns


               else if ( values(toknum) .eq. kwdsnd ) then

                  call appndi ( eqdsnd, eqryi )

                  state = ordsns


               else if (      ( values(toknum) .eq. kwwher ) &
                         .or. ( values(toknum) .eq. kwfrom ) &
                         .or. ( values(toknum) .eq. kwsel  )  ) then

!
!                 Since no ASCENDING or DESCENDING sense keyword was
!                 supplied, append the default value ASCENDING to the
!                 order-by column descriptor in the encoded query.
!
                  call appndi ( eqasnd, eqryi )

!
!                 We're done with the ORDER BY clause.  Go on to
!                 parse the SELECT clause.
!
                  state  =  selkey

               else
!
!                 We've got a keyword we don't want here.
!
                  error  =  .true.
                  errtyp =  'BAD_KEYWORD'
                  state  =  term

               end if


            else
!
!              We've got the wrong kind of token here.
!
               error  =  .true.
               errtyp =  'BAD_TOKEN'
               state  =  term

            end if
!
!           STATE is a member of {ORDRBY, ORDTAB, ORDSNS, SELKEY, TERM}.
!


         else if ( state .eq. ordtab ) then
!
!           We've picked up a qualifying table name for an order-by
!           column.  We must see a column name here.
!
            if ( token .eq. tkid ) then
!
!              Update the place-holder table name descriptor in the
!              encoded query.
!
               call movei ( namdsc, &
                            eqvdsz, &
                            eqryi(  cardi(eqryi) - eqvdsz + 1  )   )

!
!              Add the column name to the character part of the
!              encoded query.
!
               i   =   values(toknum)
               b   =   chbegs(i)
               e   =   chends(i)
               l   =   e  -  b   +  1
               lxb =   lxbegs(toknum)
               lxe =   lxends(toknum)

               call zzekinqc ( chrbuf( b : e ), &
                               l, &
                               lxb, &
                               lxe, &
                               eqryi, &
                               eqryc, &
                               coldsc           )

!
!              Add the descriptor for the column name to the encoded
!              query.
!
               do j = 1, eqvdsz
                  call appndi ( coldsc(j), eqryi )
               end do


               state  =  ordcol


            else if ( token .eq. tkeoq ) then

               error  =  .true.
               errtyp =  'MORE_TOKENS_EXP'
               state  =  term

            else

               error  =  .true.
               errtyp =  'COLUMN_EXP'
               state  =  term

            end if
!
!           STATE is a member of {ORDCOL, TERM}.
!


         else if ( state .eq. ordcol ) then
!
!           We've picked up a qualified column name.  At this point,
!           we should see a sense keyword, a comma, the end of the
!           query, or one of the FROM, SELECT, or WHERE keywords.
!
            if ( token .eq. tkkey ) then

               if ( values(toknum) .eq. kwasnd ) then
!
!                 The ASCENDING keyword has been supplied.  After this,
!                 look for another column.
!
                  call appndi ( eqasnd, eqryi )

                  state  =  ordsns


               else if ( values(toknum) .eq. kwdsnd ) then
!
!                 The DESCENDING keyword has been supplied.  After this,
!                 look for another column.
!
                  call appndi ( eqdsnd, eqryi )

                  state  =  ordsns


               else if (      ( values(toknum) .eq. kwwher ) &
                         .or. ( values(toknum) .eq. kwfrom ) &
                         .or. ( values(toknum) .eq. kwsel  )  ) then
!
!                 We're done with the ORDER BY clause.  Go on to
!                 parse the SELECT clause.
!
                  state  =  selkey

               else

                  error  =  .true.
                  errtyp =  'BAD_KEYWORD'
                  state  =  term

               end if


            else if ( token .eq. tkcoma ) then
!
!              The ASCENDING keyword is implied.
!
               call appndi ( eqasnd, eqryi )

               state  =  ordrby


            else if ( token .eq. tkeoq ) then
!
!              The ASCENDING keyword is implied.
!
               call appndi ( eqasnd, eqryi )

!
!              We're done with the ORDER BY clause.  Parse the SELECT
!              clause.
!
               state  =  selkey

            else

               error  =  .true.
               errtyp =  'COMMA_OR_KEY_EXP'
               state  =  term

            end if
!
!           STATE is a member of {ORDRBY, ORDSNS, SELKEY, TERM}.
!


         else if ( state .eq. ordsns ) then
!
!           We've picked up an order sense keyword.  At this point,
!           we should see comma or the end of the query, or one of the
!           FROM, SELECT, or WHERE keywords.
!
            if ( token .eq. tkcoma ) then
!
!              We're ready to look for another column.
!
               state  =  ordrby


            else if ( token .eq. tkeoq ) then
!
!              We're done with the ORDER BY clause.  Parse the SELECT
!              clause.
!
               state  =  selkey


            else if ( token .eq. tkkey ) then

               if (      ( values(toknum) .eq. kwwher ) &
                    .or. ( values(toknum) .eq. kwfrom ) &
                    .or. ( values(toknum) .eq. kwsel  )  ) then
!
!                 We're done with the ORDER BY clause.  Go on to
!                 parse the SELECT clause.
!
                  state  =  selkey

               else

                  error  =  .true.
                  errtyp =  'BAD_KEYWORD'
                  state  =  term

               end if


            else

               error  =  .true.
               errtyp =  'COMMA_EXP'
               state  =  term

            end if
!
!           STATE is a member of {ORDRBY, SELKEY, TERM}.
!


         else
!
!           Somehow, we've reached an invalid state.
!
            error  = .true.
            prserr = 'SPICE(BUG) -- Invalid state reached in EK parser.'
            state  =  term

         end if
!
!           STATE is a member of {ORDRBY, TERM}.
!

      end do
!
!     At this point, either an error has been detected, or the query
!     has been parsed, and the query is represented in encoded form
!     in the outputs EQRYI, EQRYC, and EQRYD.
!

!
!     We centralize construction of error messages in the following
!     section.
!
      if ( error ) then

         if ( errtyp .eq. 'FROM_NOT_FOUND' ) then

            prserr = 'Every query must contain a FROM clause. ' // &
                     'The FROM keyword was not found.'


         else if ( errtyp .eq. 'SELECT_NOT_FOUND' ) then

            prserr = 'Every query must contain a SELECT clause. ' // &
                     'The SELECT keyword was not found.'


         else if ( errtyp .eq. 'BY_EXPECTED' ) then

            prserr = 'The BY keyword was not found following the ' // &
                     'ORDER keyword.'


         else if ( errtyp .eq. 'BAD_KEYWORD' ) then

            lxb    =  lxbegs(toknum)
            lxe    =  lxends(toknum)
            prserr = 'Invalid keyword at location #. '  // &
                     'Actual token was: #'
            call repmi ( prserr, '#', lxb,            prserr )
            call repmc ( prserr, '#', query(lxb:lxe), prserr )


         else if ( errtyp .eq. 'TABLE_OR_COLUMN_EXP' ) then

            lxb    =  lxbegs(toknum)
            lxe    =  lxends(toknum)
            prserr = 'Table or column name expected at location ' // &
                     '#. Actual token was: #'
            call repmi ( prserr, '#', lxb,            prserr )
            call repmc ( prserr, '#', query(lxb:lxe), prserr )


         else if ( errtyp .eq. 'TABLE_EXP' ) then

            lxb    =  lxbegs(toknum)
            lxe    =  lxends(toknum)
            prserr = 'Table name expected at location #. '  // &
                     'Actual token was: #'
            call repmi ( prserr, '#', lxb,            prserr )
            call repmc ( prserr, '#', query(lxb:lxe), prserr )


         else if ( errtyp .eq. 'COLUMN_EXP' ) then

            lxb    =  lxbegs(toknum)
            lxe    =  lxends(toknum)
            prserr = 'Column name expected at location #. '  // &
                     'Actual token was: #'
            call repmi ( prserr, '#', lxb,            prserr )
            call repmc ( prserr, '#', query(lxb:lxe), prserr )


         else if ( errtyp .eq. 'ALIAS_EXP' ) then

            lxb    =  lxbegs(toknum)
            lxe    =  lxends(toknum)
            prserr = 'Table alias, comma, or keyword expected at ' // &
                     'location #. Actual token was: #'
            call repmi ( prserr, '#', lxb,            prserr )
            call repmc ( prserr, '#', query(lxb:lxe), prserr )


         else if ( errtyp .eq. 'COMMA_OR_KEY_EXP' ) then

            lxb    =  lxbegs(toknum)
            lxe    =  lxends(toknum)
            prserr = 'Comma or keyword expected at ' // &
                     'location #. Actual token was: #'
            call repmi ( prserr, '#', lxb,            prserr )
            call repmc ( prserr, '#', query(lxb:lxe), prserr )


         else if ( errtyp .eq. 'COMMA_EXP' ) then

            lxb    =  lxbegs(toknum)
            lxe    =  lxends(toknum)
            prserr = 'Comma expected at location #. Actual token was: #'
            call repmi ( prserr, '#', lxb,            prserr )
            call repmc ( prserr, '#', query(lxb:lxe), prserr )


         else if ( errtyp .eq. 'MORE_TOKENS_EXP' ) then

            prserr  =  'More tokens were expected in query.'


         else if ( errtyp .eq. 'KEYWORD_EXP' ) then

            prserr = 'The keyword # was expected at location ' // &
                     '#. Actual token was: #'
            call repmc ( prserr, '#', expkey,         prserr )
            call repmi ( prserr, '#', lxb,            prserr )
            call repmc ( prserr, '#', query(lxb:lxe), prserr )


         else if ( errtyp .eq. 'BAD_TOKEN' ) then

            lxb    =  lxbegs(toknum)
            lxe    =  lxends(toknum)
            prserr = 'Invalid token at location #. Token was: #'
            call repmi ( prserr, '#', lxb,            prserr )
            call repmc ( prserr, '#', query(lxb:lxe), prserr )


         else if ( errtyp .eq. 'TOO_MANY_TABLES' ) then

            prserr  =  'Number of tables in "FROM" clause exceeds ' // &
                       'allowed maximum of #.'
            call repmi  ( prserr, '#', maxtab, prserr )


         else if ( errtyp .eq. 'TOO_MANY_ORD_COLS' ) then

            prserr  =  'Number of order-by columns exceeds allowed ' // &
                       'maximum of #.'
            call repmi  ( prserr, '#', maxtab, prserr )


         else if ( errtyp .eq. 'TOO_MANY_SEL_COLS' ) then

            prserr  =  'Number of SELECT columns exceeds allowed ' // &
                       'maximum of #.'
            call repmi  ( prserr, '#', maxsel, prserr )


         else if ( errtyp .ne. 'WHERE_ERROR' ) then

            prserr  =  'SPICE(BUG)--Unrecognized error type.  Type ' // &
                       'was #.'
            call repmc ( prserr, '#', errtyp, prserr )

         end if



      else
!
!        Indicate that parsing is complete.
!
         call zzekweqi ( 'PARSED', itrue, eqryi )

      end if


      call chkout ( 'ZZEKPARS' )
      return
      end
