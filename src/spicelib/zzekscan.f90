!$Procedure      ZZEKSCAN ( EK, scan query )

      subroutine zzekscan (  query,  maxntk, maxnum, ntoken, tokens, &
                             lxbegs, lxends, values, numvls, chrbuf, &
                             chbegs, chends, scnerr, errmsg         )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Scan tokens in an EK query.
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

      include 'ekkeyw.inc'
      include 'ektokn.inc'
      include 'ekqlimit.inc'

      character(*)          query
      integer(ip)           maxntk
      integer(ip)           maxnum
      integer(ip)           ntoken
      integer(ip)           tokens ( * )
      integer(ip)           lxbegs ( * )
      integer(ip)           lxends ( * )
      integer(ip)           values ( * )
      real(wp)              numvls ( * )
      character(*)          chrbuf
      integer(ip)           chbegs ( * )
      integer(ip)           chends ( * )
      logical               scnerr
      character(*)          errmsg

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
!     2)  Examples of lexically invalid queries are:
!
!            from time where time lt
!            1991 jan 1                         {time string is not
!                                                quoted}
!
!            from time * where time
!             .lt. 1991 jan 1                   {operator should be lt}
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
      integer(ip)           bsrchc
      integer(ip)           frstpc
      integer(ip)           isrchc
      integer(ip)           rtrim

      logical               beint
      logical               return

!
!     Local parameters
!
      character(*), parameter :: dquote = '"'

      character(*), parameter :: squote = ''''

      integer(ip), parameter :: lnsize = 80

      integer(ip), parameter :: nxttok = 1

      integer(ip), parameter :: newtok = nxttok + 1

      integer(ip), parameter :: term = newtok + 1

      integer(ip), parameter :: qstr = term   + 1

      integer(ip), parameter :: dot = qstr   + 1

      integer(ip), parameter :: number = dot    + 1

      integer(ip), parameter :: ident = number + 1

      integer(ip), parameter :: spcial = ident + 1

      integer(ip), parameter :: lbcell = -5

      integer(ip), parameter :: mxspec = 512

!
!     Number of tokens made up of special characters:
!
      integer(ip), parameter :: nspec = 13

!
!     Max length of any such token:
!
      integer(ip), parameter :: spclen = 2

!
!     Local variables
!
      character(1)          chr
      character(spclen)     spcstr ( nspec )
      character(lnsize)     hdchrs
      character(kwlen)      keywds ( nkeywd )
      character(lnsize)     tlchrs
      character(maxqry)     tquery

      integer(ip)           chcard
      integer(ip)           cptr
      integer(ip)           i
      integer(ip)           idspec ( lbcell : mxspec )
      integer(ip)           j
      integer(ip)           kwvals ( nkeywd )
      integer(ip)           l
      integer(ip)           last
      integer(ip)           length
      integer(ip)           nchars
      integer(ip)           nnums
      integer(ip)           nstrs
      integer(ip)           ptr
      integer(ip)           room
      integer(ip)           state
      integer(ip)           spctok ( nspec )
      integer(ip)           spcval ( nspec )

      logical               pass1

!
!     Statement Functions
!
      logical               isalph
      logical               isdigt
      logical               isdot
      logical               issign
      logical               isnum
      logical               isquot

!
!     Saved variables
!
      save

!
!     Initial values
!

!
!     These keyword declarations must be made in alphabetical order!
!
      data  (  keywds(i), kwvals(i), i = 1, 10 )      / &
                 'ALL',          kwall, &
                 'AND',          kwand, &
                 'ASC ',         kwasnd, &
                 'AVG ',         kwavg, &
                 'BETWEEN',      kwbetw, &
                 'BY',           kwby, &
                 'COUNT',        kwcnt, &
                 'DESC',         kwdsnd, &
                 'DISTINCT',     kwdstn, &
                 'EQ',           kweq                 /

      data  (  keywds(i), kwvals(i), i = 11, 20 )     / &
                 'FROM',         kwfrom, &
                 'GE',           kwge, &
                 'GROUP',        kwgrp, &
                 'GT',           kwgt, &
                 'HAVING',       kwhav, &
                 'IS',           kwis, &
                 'LE',           kwle, &
                 'LIKE',         kwlike, &
                 'LT',           kwlt, &
                 'MAX',          kwmax                /

      data  (  keywds(i), kwvals(i), i = 21, nkeywd ) / &
                 'MIN',          kwmin, &
                 'NE',           kwne, &
                 'NOT',          kwnot, &
                 'NULL',         kwnull, &
                 'OR',           kwor, &
                 'ORDER',        kwordr, &
                 'SELECT',       kwsel, &
                 'SUM',          kwsum, &
                 'WHERE',        kwwher               /

!
!     The following tokens are sequences of special characters.  Some
!     of these are synonyms for keywords; some have other meanings.  In
!     this data statement, the longer sequences must precede the shorter
!     ones, in order for the matching algorithm to work properly.
!
      data  (  spcstr(i), spctok(i), spcval(i), i = 1, nspec )  / &
                 '!=',      tkkey,    kwne, &
                 '^=',      tkkey,    kwne, &
                 '<>',      tkkey,    kwne, &
                 '<=',      tkkey,    kwle, &
                 '>=',      tkkey,    kwge, &
                 '<',       tkkey,    kwlt, &
                 '>',       tkkey,    kwgt, &
                 '=',       tkkey,    kweq, &
                 '(',       tklpar,   0, &
                 ')',       tkrpar,   0, &
                 ',',       tkcoma,   0, &
                 '.',       tkdot,    0, &
                 '*',       tkstar,   0                         /

      data        pass1              /  .true.  /

!
!     Statement Function Definitions
!
      isalph ( chr )  =       (      ( ichar(chr) >= ichar('A') ) &
                               .and. ( ichar(chr) <= ichar('Z') ) ) &
                        .or.  (      ( ichar(chr) >= ichar('a') ) &
                               .and. ( ichar(chr) <= ichar('z') ) )

      isdigt ( chr )  =       (      ( ichar(chr) >= ichar('0') ) &
                               .and. ( ichar(chr) <= ichar('9') ) )

      isdot  ( chr )  =     ( chr == '.' )

      issign ( chr )  =     ( chr == '+' )    .or. ( chr == '-' )

      isnum  ( chr )  =       isdigt( chr )     .or.   issign ( chr ) &
                                                .or.  ( chr == '.' )

      isquot ( chr )  =     ( chr == squote ) .or. ( chr == dquote )

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     The first time through, set up our identifier character set.
!
      if ( pass1 ) then
!
!        Each identifier must start with a letter (of either case).
!        The subsequent characters must be letters, numbers, dollar
!        signs or underscores.
!
         hdchrs = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' // &
                  'abcdefghijklmnopqrstuvwxyz'

         tlchrs = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' // &
                  'abcdefghijklmnopqrstuvwxyz' // &
                  '0123456789'                 // &
                  '$_'

         call ssizei ( mxspec,         idspec )
         call lxcsid ( hdchrs, tlchrs, idspec )

         pass1  =  .false.

      end if

!
!     We'll work with a local copy of the query.
!
      l       =  rtrim ( query )
      tquery  =  query(:l)

!
!     Initialize pointers and counts.
!
      cptr   =  1
      nnums  =  0
      nstrs  =  0
      chcard =  0
      ntoken =  0

!
!     Start out in the token search state.
!
      state  =  nxttok

      do while ( state /= term )

         if ( state == nxttok ) then
!
!           In our initial state, we're looking for a new token.
!           We stop when we have enough characters to determine
!           which kind of token we have, or if we run out of
!           characters.
!
!           Set our character pointer to the beginning of the next
!           token.
!
            if ( ntoken > 0 ) then
              cptr =  lxends(ntoken) + 1
            end if

            if ( cptr > l ) then

               state  =  term

            else

               do while (       ( tquery(cptr:cptr) == ' ' ) &
                          .and. ( cptr              < l   )  )

                  cptr  =  cptr + 1
               end do

               if ( tquery(cptr:cptr) == ' ' ) then
!
!                 We're out of non-blank characters to look at.
!
                  state  =  term

               else

                  chr    =  tquery(cptr:cptr)
                  state  =  newtok

               end if

            end if
!
!           STATE is in the set {NEWTOK, TERM}.
!

         else if ( state == newtok ) then
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
            if ( ntoken == maxntk ) then

               errmsg  =  'Maximum allowed number of tokens is #; at '// &
                          'least # tokens are present in QUERY.'
               call repmi ( errmsg, '#', maxntk,   errmsg )
               call repmi ( errmsg, '#', maxntk+1, errmsg )

               scnerr = .true.
               return

            end if

            if ( isquot(chr) ) then

               state = qstr

            else if ( isdot(chr) ) then

               state = dot

            else if ( isnum(chr) ) then

               state = number

            else if ( isalph(chr) ) then

               state = ident

            else

               state = spcial

            end if

!
!           At this point, the next value of STATE has been determined.
!           STATE is in the set
!
!              {QSTR, NUMBER, IDENT, SPCIAL}
!

         else if ( state == qstr ) then
!
!           Look for a quoted string starting at location CPTR.
!           Use the current character as the quote character.
!
            call lxqstr ( tquery, chr, cptr, last, nchars )

            if ( nchars == 0 ) then

               errmsg = 'Invalid quoted string at location #.'
               call repmi ( errmsg, '#', cptr, errmsg )

               scnerr = .true.
               return

            end if

!
!           We've located a quoted string lexeme.  Parse the lexeme
!           and obtain the corresponding string value.  First make
!           sure we have enough room for the parsed string.
!
            room  =  len(chrbuf)-chcard

            if ( nchars > room )  then

               errmsg  =  'Insufficient space to store quoted '    // &
                          'string at location #; # chars needed; ' // &
                          'only # are available.'

               call repmi  ( errmsg, '#', cptr,    errmsg )
               call repmi  ( errmsg, '#', nchars,  errmsg )
               call repmi  ( errmsg, '#', room,    errmsg )

               scnerr = .true.
               return

            end if

            call parsqs ( tquery(cptr:cptr+nchars-1), &
                          chr, &
                          chrbuf(chcard+1:), &
                          length, &
                          scnerr, &
                          errmsg, &
                          ptr              )

            if ( scnerr ) then

               call prefix ( '#', 2, errmsg )

               call repmc  ( errmsg, &
                             '#', &
                             'Error occurred while parsing '    // &
                             'quoted string token at location #:', &
                             errmsg                        )

               call repmi  ( errmsg, '#', cptr, errmsg )

               scnerr = .true.

               return

            end if

!
!           We've found a valid quoted string.  Set our outputs.
!
            ntoken          =  ntoken + 1
            tokens(ntoken)  =  tkqstr
            nstrs           =  nstrs  + 1
            values(ntoken)  =  nstrs
            chbegs(nstrs)   =  chcard + 1
            chends(nstrs)   =  chcard + length
            chcard          =  chends(nstrs)
            lxbegs(ntoken)  =  cptr
            lxends(ntoken)  =  last

            state           =  nxttok

!
!           STATE is now NXTTOK.
!

         else if ( state == dot ) then
!
!           The token begins with a period.  We could be looking at
!           a floating point number, or we could be looking at a
!           period in a compound identifier.
!
!           Look for a number starting at location CPTR.
!
            call lx4num ( tquery, cptr, last, nchars )

            if ( nchars > 0 ) then
               state  =  number
            else
               state  =  spcial
            end if

!
!           STATE has been set to NUMBER or SPCIAL.  CPTR and NTOKEN
!           remain unchanged.
!

         else if ( state == number ) then
!
!           Look for a number starting at location CPTR.
!
            call lx4num ( tquery, cptr, last, nchars )

            if ( nchars == 0 ) then

               errmsg = 'Invalid numeric token at location #.'
               call repmi ( errmsg, '#', cptr, errmsg )

               scnerr = .true.
               return

            end if

!
!           Parse the token, but only do so if there's enough
!           room to store the result.
!
            room  =  maxnum - nnums

            if ( room < 1 ) then

               errmsg  =  'Insufficient space to store value of ' // &
                          'number at location #; # elements are ' // &
                          'available in the NUMVLS array; # are ' // &
                          'required.'

               call repmi  ( errmsg, '#', cptr,     errmsg )
               call repmi  ( errmsg, '#', maxnum,   errmsg )
               call repmi  ( errmsg, '#', maxnum+1, errmsg )

               scnerr = .true.
               return

            end if

            call nparsd ( tquery(cptr:last), &
                          numvls(nnums+1), &
                          errmsg, &
                          ptr                 )

            if ( errmsg /= ' ' ) then
!
!              This check is done for safety; by construction, we
!              should always have a valid number if LX4NUM
!              thinks we have a valid number, so in fact ERRMSG
!              should always be blank.
!

               call prefix ( '#', 2, errmsg )

               call repmc  ( errmsg, &
                             '#', &
                             'Error found in numeric ' // &
                             'token at location #:', &
                             errmsg                        )

               call repmi  ( errmsg, '#', cptr+ptr-1, errmsg )

               scnerr = .true.
               return

            end if

!
!           We found a valid numeric token.  We distinguish
!           between integers and d.p. numbers; set the token
!           to the most restrictive category possible.
!
            ntoken  =  ntoken + 1

            if (   beint( tquery(cptr:last) )    ) then
               tokens(ntoken) =  tkint
            else
               tokens(ntoken) =  tkdp
            end if

!
!           Set the rest of our outputs.
!
            nnums           =  nnums  + 1
            values(ntoken)  =  nnums
            lxbegs(ntoken)  =  cptr
            lxends(ntoken)  =  last

            state           =  nxttok

!
!           STATE is now NXTTOK.
!

         else if ( state == ident ) then
!
!           Look for an identifier starting at location CPTR.
!
            call lxidnt ( idspec, tquery, cptr, last, nchars )

            if ( nchars == 0 ) then
!
!              This check is done for safety; by construction, we
!              should always have a valid identifier of at least one
!              character if we get to the IDENT state, so in fact
!              NCHARS should never equal zero.
!
               errmsg = 'Invalid identifier at location #.'
               call repmi ( errmsg, '#', cptr, errmsg )

               scnerr = .true.
               return

            end if

!
!           We've located an identifier lexeme.  Make sure we have
!           enough room for the string.
!
            room  =  len(chrbuf)-chcard

            if ( nchars > room )  then

               errmsg  =  'Insufficient space to store identifier '// &
                          'string at location #; # chars needed; ' // &
                          'only # are available.'

               call repmi  ( errmsg, '#', cptr,   errmsg )
               call repmi  ( errmsg, '#', nchars, errmsg )
               call repmi  ( errmsg, '#', room,   errmsg )

               scnerr = .true.
               return

            end if

!
!           We've found a valid identifier or keyword.  Set our
!           outputs.  Convert the string to upper case.
!
            ntoken =  ntoken + 1

            call ucase (  tquery( cptr:last ), &
                          chrbuf( chcard+1:chcard+nchars )  )

            i      =  bsrchc ( chrbuf( chcard+1 : chcard+nchars ), &
                               nkeywd, &
                               keywds                            )

            if ( i > 0 ) then
!
!              It's a keyword.
!
               tokens(ntoken)  =  tkkey
               values(ntoken)  =  kwvals(i)
               lxbegs(ntoken)  =  cptr
               lxends(ntoken)  =  last

               state           =  nxttok

            else
!
!              It's an identifier.
!
               nstrs           =  nstrs  + 1
               chbegs(nstrs)   =  chcard + 1
               chends(nstrs)   =  chcard + nchars
               chcard          =  chends(nstrs)
               tokens(ntoken)  =  tkid
               values(ntoken)  =  nstrs
               lxbegs(ntoken)  =  cptr
               lxends(ntoken)  =  last

               state           =  nxttok

!
!              We finished scanning an identifier.
!
!              STATE is set to NXTTOK.
!
            end if
!
!           We scanned a keyword or an identifier.
!
!           STATE is set to NXTTOK.
!

         else if ( state == spcial ) then
!
!           Look for a valid token starting with a special character at
!           location CPTR. We attempt to match the longest possible
!           special token.
!
            i  =  min ( spclen,   l - cptr + 1 )
            j  =  0

            do while (  ( i >= 1 ) .and. ( j == 0 ) )

               last =  cptr + i - 1
               j    =  isrchc ( tquery(cptr:last), nspec, spcstr )

               if ( j == 0 ) then
                  i  =  i - 1
               end if

            end do

            if ( j > 0 ) then
!
!              We've identified a valid token.
!
               ntoken =  ntoken + 1

               tokens(ntoken)  =  spctok(j)
               values(ntoken)  =  spcval(j)
               lxbegs(ntoken)  =  cptr
               lxends(ntoken)  =  cptr - 1 + rtrim( spcstr(j) )

               state           =  nxttok

            else

               errmsg = 'Invalid character found at location #. '
               call repmi  ( errmsg, '#', cptr, errmsg )
!
!              If the offending character is printable, include it
!              in the error message.  Otherwise, include the integer
!              code for the character.
!
               if ( frstpc(chr) > 0 ) then

                  call suffix ( '<character> = ''#''',   2, errmsg )
                  call repmc  ( errmsg, '#', chr,           errmsg )

               else

                  call suffix ( 'ICHAR(<character>) = #', 2, errmsg )
                  call repmi  ( errmsg, '#', ichar(chr),     errmsg )

               end if

               scnerr = .true.
               return

            end if
!
!           STATE is now NXTTOK.
!

         end if

      end do

!
!     If we got this far, we've found the tokens in the query.
!
      scnerr  =  .false.
      errmsg  =  ' '

      return
      end
