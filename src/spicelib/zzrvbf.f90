!$Procedure ZZRVBF ( Private --- Pool, read the next buffer variable )

      subroutine zzrvbf ( buffer, bsize,  linnum, namlst, &
                          nmpool, names,  datlst, &
                          dppool, dpvals, &
                          chpool, chvals, &
                          varnam, eof )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read the next variable from a text buffer into the kernel pool.
!
! $ Required_Reading
!
!     PRIVATE KERNEL
!
! $ Keywords
!
!     POOL
!
! $ Declarations

      implicit none
      integer(ip)           lbpool
      parameter           ( lbpool = -5 )

      character(*)          buffer     (  * )
      integer(ip)           bsize
      integer(ip)           linnum

      integer(ip)           namlst     (           * )
      integer(ip)           nmpool     ( 2, lbpool:* )
      character(*)          names      (           * )
      integer(ip)           datlst     (           * )
      integer(ip)           dppool     ( 2, lbpool:* )
      real(wp)              dpvals     (           * )
      integer(ip)           chpool     ( 2, lbpool:* )
      character(*)          chvals     (           * )

      character(*)          varnam
      logical               eof

      integer(ip)           linlen
      parameter           ( linlen = 132 )

      integer(ip)           maxlen
      parameter           ( maxlen = 32 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BUFFER     I   Array of text to be read and parsed.
!     BSIZE      I   Number of lines in text buffer.
!     LINNUM    I/O  line number to begin reading buffer
!     NAMLST    I/O  array of collision resolution list heads
!     NMPOOL    I/O  linked list pool of collision resolution lists
!     NAMES     I/O  array of names of kernel pool variables
!     DATLST    I/O  array of heads of lists of variable values
!     DPPOOL    I/O  linked list pool of pointer lists to d.p. values
!     DPVALS    I/O  array of d.p. kernel pool values
!     CHPOOL    I/O  linked list pool of pointer lists to string values
!     CHVALS    I/O  array of string kernel pool values
!     VARNAM     O   name of variable parsed
!     EOF        O   if TRUE end of input file has been reached.
!
! $ Detailed_Input
!
!     BUFFER    is a string array that contains the text that should
!               be parsed and placed into the kernel pool data
!               structure.
!
!     BSIZE     is the number of lines of text in BUFFER.
!
!     LINNUM    the line number (in BUFFER) at which to begin parsing
!               text.
!
!     NAMLST    this collection of arrays together with the hash
!     NMPOOL    function ZZHASH provide the mechanism for storing
!     NAMES     and retrieving kernel pool variables.
!     DATLST
!     DPPOOL    Given a potential variable name NAME the function
!     DPVALS    ZZHASH(NAME) gives the location in the array in
!     CHPOOL    NAMLST where one should begin looking for the
!     CHVALS    kernel pool variable NAME.
!
!               If NAMLST( ZZHASH(NAME) ) is zero, there is no kernel
!               pool variable corresponding to NAME.  If it is non-zero
!               then NAMLST is the head node of a linked list of names
!               that evaluate to the same integer under the function
!               ZZHASH.  Letting NODE = NAMLST( ZZHASH(NAME) ) check
!               NAMES(NODE) for equality with NAME.  If there is
!               no match find the next node ( NMPOOL(NEXT,NODE) ) until
!               a match occurs or all nodes of the list have been
!               examined.  To insert a new NAME allocate a node NEW from
!               the free list of NMPOOL and append it to the tail of the
!               list pointed to by NAMLST ( ZZHASH(NAME) ).
!
!               Once a node for NAME is located (call it NAMEAT)
!               the values for NAME can be found by examining
!               DATLST(NAMEAT).  If zero, no values have yet been
!               given to NAME.  If less than zero, -DATLST(NAMEAT)
!               is the head node of a list in CHPOOL that gives the
!               indexes of the values of NAME in CHVALS.  If greater
!               than zero, DATLST(NAMEAT) is the head node of a list
!               in DPPOOL that gives the indexes of the values of NAME
!               in DPVALS.
!
!
!
!
! $ Detailed_Output
!
!     LINNUM     is the line number at which the "next" read should
!                begin.
!
!     NAMLST     is the same structure as input but updated to
!     NMPOOL     include the next variable read from the text buffer.
!     NAMES
!     DATLST
!     DPPOOL
!     DPVALS
!     CHPOOL
!     CHVALS
!
!     VARNAM     is the name of the variable. VARNAM is blank if
!                no variable is read.
!
!     EOF        is true when the end of the internal buffer has been
!                reached, and is false otherwise.
!
! $ Parameters
!
!      LINLEN      is the maximum length of a line in the buffer.
!
!      MAXLEN      is the maximum length of the variable names that
!                  can be stored in the kernel pool (defined in pool.f).
!
! $ Exceptions
!
!
!     1) The error 'SPICE(BADTIMESPEC)' is signaled if a value
!        beginning with '@' cannot be parsed as a time.
!
!     2) The error 'SPICE(BADVARASSIGN)' is signaled if variable
!        assignment does not have the form NAME = [(] value [ value ) ].
!
!     3) The error 'SPICE(KERNELPOOLFULL)' is signaled if there is
!        no room left in the kernel pool to store another variable
!        or value.
!
!     4) The error 'SPICE(NONPRINTINGCHAR)' is signaled if the name
!        in a variable assignment contains a non-printing character.
!
!     5) The error 'SPICE(NUMBEREXPECTED)' is signaled if a value
!        that is unquoted cannot be parsed as time or number.
!
!     6) The error 'SPICE(TYPEMISMATCH)' is signaled if a variable
!        has a first value of one type (numeric or character) and
!        a subsequent component has the other type.
!
!     7) The error 'SPICE(BADVARNAME)' signals if a kernel pool
!        variable name length exceeds MAXLEN.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See POOL (entry point LMPOOL).
!
! $ Restrictions
!
!     The input buffer should be no more than 132 characters in width.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 03-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    SPICELIB Version 1.1.0, 09-FEB-2010 (EDW)
!
!        Added an error check on the length of the kernel pool variable
!        names read from BUFFER.
!
!-    SPICELIB Version 1.0.0, 29-MAR-1999 (WLT)
!
!-&

!
!
!     SPICELIB functions
!
      integer(ip)           lastpc
      integer(ip)           lnknfn
      integer(ip)           rtrim
      integer(ip)           zzhash

      logical               return
      logical               failed

!
!     Local parameters.
!
!     Below are a collection of enumerated lists that are used
!     to discern what part of the processing we are in and what
!     kind of entity we are dealing with.  First the overall
!     processing flow of a variable assignment.
!

      integer(ip)           begin
      parameter           ( begin  = 1 )

      integer(ip)           done
      parameter           ( done   = begin  + 1 )

      integer(ip)           invar
      parameter           ( invar  = done   + 1 )

!
!     Next we have the various types of tokens that can be found
!     in the parsing of an input line
!
!     Q   --- quoted (or protected tokens)
!     NQ  --- unquoted tokens
!     BV  --- beginning of a vector
!     EV  --- ending of a vector
!     EQ  --- equal sign
!     EQP --- equal sign plus
!
      integer(ip)           q
      parameter           ( q      = 1          )

      integer(ip)           nq
      parameter           ( nq     = q      + 1 )

      integer(ip)           bv
      parameter           ( bv     = nq     + 1 )

      integer(ip)           ev
      parameter           ( ev     = bv     + 1 )

      integer(ip)           eq
      parameter           ( eq     = ev     + 1 )

      integer(ip)           eqp
      parameter           ( eqp    = eq     + 1 )

!
!     A variable can have one of three types as we process
!     it.  It can have an unknown type UNKNWN, STRTYP or NUMTYP.
!
!

      integer(ip)           strtyp
      parameter           ( strtyp = 1 )

      integer(ip)           numtyp
      parameter           ( numtyp = strtyp + 1 )

      integer(ip)           unknwn
      parameter           ( unknwn = numtyp + 1 )

!
!     The next two parameters indicate which component of a linked
!     list node point to the previous node and the next node.
!
      integer(ip)           prev
      parameter           ( prev = 2 )

      integer(ip)           next
      parameter           ( next = 1 )


      integer(ip)           filsiz
      parameter           ( filsiz = 255 )
!
!     The next collection of variables are set up in first pass
!     through this routine.  They would be parameters if FORTRAN
!     allowed us to do this in a standard way.
!
      integer(ip)           iblank
      integer(ip)           icomma
      integer(ip)           iequal
      integer(ip)           ilparn
      integer(ip)           iplus
      integer(ip)           iquote
      integer(ip)           irparn
      integer(ip)           itab
      integer(ip)           itmark


      character(linlen)     line
      character(linlen)     name

      character(256)        error

      real(wp)              dvalue

      integer(ip)           at
      integer(ip)           b
      integer(ip)           badat
      integer(ip)           begs(linlen)
      integer(ip)           chnode
      integer(ip)           code
      integer(ip)           count
      integer(ip)           datahd
      integer(ip)           dirctv
      integer(ip)           dpnode
      integer(ip)           e
      integer(ip)           ends(linlen)
      integer(ip)           free
      integer(ip)           head
      integer(ip)           i
      integer(ip)           j
      integer(ip)           lookat
      integer(ip)           nameat
      integer(ip)           ncomp
      integer(ip)           node
      integer(ip)           nxttok
      integer(ip)           r1
      integer(ip)           r2
      integer(ip)           status
      integer(ip)           tail
      integer(ip)           type(linlen)
      integer(ip)           varlen
      integer(ip)           vartyp

!
!     The logicals below are used to take apart the tokens in an
!     input line.
!
      logical               even
      logical               intokn
      logical               inquot
      logical               insepf

      logical               full
      logical               found
      logical               first

!
!     The following logicals are in-line functions that are used
!     when processing the input strings.
!
      logical               issep
      logical               isquot
      logical               isequ
      logical               isrpar
      logical               islpar
      logical               isplus
      logical               isbad
      logical               istime
!
!     Save everything.
!
      save


      data                  first  / .true. /

!
!     Below are a collection of In-line function definitions that are
!     intended to make the code a bit easier to write and read.
!

      issep ( code ) =      code == iblank &
                       .or. code == icomma &
                       .or. code == ilparn &
                       .or. code == irparn &
                       .or. code == iequal &
                       .or. code == itab

      isquot( code ) =      code == iquote
      isequ ( code ) =      code == iequal
      isrpar( code ) =      code == irparn
      islpar( code ) =      code == ilparn
      isplus( code ) =      code == iplus
      istime( code ) =      code == itmark


      isbad ( dirctv ) =       dirctv /= eq &
                         .and. dirctv /= eqp

!
!     Standard SPICE error handling.
!
      if ( return() ) return
!
!     Initializations.
!
      if ( first ) then

         first  = .false.
         icomma =  ichar( ','   )
         iblank =  ichar( ' '   )
         iquote =  ichar( ''''  )
         ilparn =  ichar( '('   )
         irparn =  ichar( ')'   )
         iequal =  ichar( '='   )
         iplus  =  ichar( '+'   )
         itmark =  ichar( '@'   )
         itab   =  9

      end if

!
!     No variable yet and no parsing errors so far.
!
      name  = ' '
      error = ' '
      ncomp = 0


!
!     Get the next data line. Unless something is terribly wrong,
!     this will begin a new variable definition. We have to read
!     the whole variable, unless we get an error, in which case
!     we can quit.
!
      status = begin

      do while ( ( status /= done ) .and. ( .not. failed() ) )

         line = ' '
!
!        We need to skip blank lines...
!
         do while ( line == ' ' )
            eof    = linnum > bsize

            if ( eof ) return

            line   = buffer(linnum )
            linnum = linnum + 1
         end do

!
!        Find the "tokens" in the input line. As you scan from left
!        to right along the line, exactly one of the following
!        conditions is true.
!
!        1) You are in a separator field
!        4) You are in a quoted substring
!        5) You are in a non-quoted substring that isn't a separator
!           field.
!
!        Stuff between separator fields are regarded as tokens.  Note
!        this includes quoted strings.
!
!        In addition we keep track of 3 separators: '=', '(', ')'
!        Finally, whenever we encounters the separator '=', we back
!        up and see if it is preceded by a '+', if so we attach
!        it to the '=' and treat the pair of characters as a single
!        separator.
!

         even   = .true.
         intokn = .false.
         inquot = .false.
         insepf = .true.

         count  =  0
         i      =  0

         do while ( i < len(line) )
!
!           The current character is either a separator, quote or
!           some other character.
!
            i    = i + 1
            code = ichar(line(i:i))

            if ( issep(code) ) then
!
!              There are 3 possible states we could be in
!                 Separation Field
!                 A quoted substring with the last quote an odd one.
!                 A quoted substring with the last quote an even one.
!                 A non-quoted token.
!              In the first two cases nothing changes, but in the
!              next two cases we transition to a separation field.
!
               if ( intokn .or. ( inquot .and. even ) ) then
                  inquot = .false.
                  intokn = .false.
                  insepf = .true.
               end if

               if ( insepf ) then
!
!                 We need to see if this is one of the special
!                 separators
!
                  if      ( isequ(code)  ) then

                     count       = count + 1
                     begs(count) = i
                     type(count) = eq
                     ends(count) = i

                     if ( i > 1 ) then
!
!                       Look back at the previous character.
!                       See if it is a plus character.
!
                        code = ichar( line(i-1:i-1) )

                        if ( isplus(code) ) then
!
!                          This is the directive '+=' we need
!                          to set the beginning of this token
!                          to the one before this and adjust
!                          the end of the last token.
!
                           type(count) = eqp
                           begs(count) = i-1

                           if (      begs(count-1) &
                                == ends(count-1) ) then

                              count       = count - 1
                              begs(count) = i - 1
                              ends(count) = i
                              type(count) = eqp

                           else

                              ends(count-1) = ends(count-1) - 1

                           end if

                        end if

                     end if

                  else if ( isrpar(code) ) then

                     count       = count + 1
                     begs(count) = i
                     ends(count) = i
                     type(count) = ev

                  else if ( islpar(code) ) then

                     count       = count + 1
                     begs(count) = i
                     ends(count) = i
                     type(count) = bv

                  end if

               end if

            else if ( isquot(code) ) then
!
!              There are 3 cases of interest.
!                 We are in a quoted substring already
!                 We are in a separator field
!                 We are in a non-quoted token.
!              In the first case nothing changes.  In the second
!              two cases we change to being in a quoted substring.
!
               even = .not. even

               if ( .not. inquot ) then

                  insepf      = .false.
                  intokn      = .false.
                  inquot      = .true.
                  count       =  count + 1
                  begs(count) =  i
                  type(count) =  q

               end if

               ends(count) = i

            else
!
!              This is some character other than a quote, or
!              separator character.
!
!              We are in one of four situations.
!
!                 1) We are in a quoted substring with an odd number of
!                    quotes.
!                 2) We are in a quoted substring with an even number of
!                    quotes.
!                 2) We are in a separator field
!                 3) We are in a non-quoted token.
!
!              In cases 1 and 3 nothing changes. So we won't check
!              those cases.
!

               if ( insepf .or. ( inquot .and. even ) ) then
                  inquot      = .false.
                  insepf      = .false.
                  intokn      = .true.
                  count       =  count + 1
                  begs(count) =  i
                  type(count) =  nq
               end if

               ends(count) = i

            end if

         end do


!
!        The first word on the first line should be the name of a
!        variable. The second word should be a directive: = or +=.
!
         if ( status == begin ) then
!
!           There must be at least 3 contributing tokens on this line.
!
            if ( count < 3 ) then

               call setmsg ( 'A kernel variable was not properly ' &
               //            'formed on line # text buffer.' &
               //            'Such an assignment should have the ' &
               //            'form: ''<variable name> [+]= ' &
               //            '<values>''. This line was ''#''. ' )

               r2 = rtrim(line)

               call errint ( '#', linnum     )
               call errch  ( '#', line(1:r2) )
               call sigerr ( 'SPICE(BADVARASSIGN)' )
               return

            end if
!
!           See if the variable name is legitimate:
!
            badat = lastpc(line(begs(1):ends(1)))

            if ( badat <= ends(1) - begs(1) ) then
!
!              There is a non-printing character in the variable
!              name.  This isn't allowed.
!
               at = begs(1) + badat


               call setmsg ( 'There is a non-printing character ' &
               //            'embedded in line # of the text ' &
               //            'buffer.  Non-printing ' &
               //            'characters are not allowed in ' &
               //            'kernel variable assignments.  The ' &
               //            'non-printing character has ASCII ' &
               //            'code #. ' )


               call errint ( '#', linnum             )
               call errint ( '#', ichar(line(at:at)) )
               call sigerr ( 'SPICE(NONPRINTINGCHAR)')
               return
            end if


!
!           Check the variable name length; signal an error
!           if longer than MAXLEN.
!
            varlen = len( line( begs(1): ends(1) ) )

            if ( varlen > maxlen ) then

               call setmsg ( 'A kernel pool variable name in the ' &
               //            'input buffer exceeds the maximum ' &
               //            'allowed length #1. The actual length ' &
               //            'of the variable name is #2, the ' &
               //            'offending variable name to #3 characters:' &
               //            ' ''#4''.')

               call errint ( '#1', maxlen                   )
               call errint ( '#2', varlen                   )
               call errint ( '#3', linlen                   )
               call errch  ( '#4', line( begs(1): ends(1) ) )
               call sigerr ( 'SPICE(BADVARNAME)'            )

            end if

!
!           The variable name is ok. How about the directive.
!
            varnam = line(begs(1):ends(1))
            dirctv = type(2)


!
!           If this is replacement (=) and not an addition (+=),
!           delete the values currently associated with the variable.
!           They will be replaced later.
!
            if ( isbad(dirctv) ) then

               call setmsg ( 'A kernel variable was not properly ' &
               //            'formed on line # of the text buffer. ' &
               //            'Such an assignment should have the ' &
               //            'form: ''<variable name> [+]= ' &
               //            '<values>''.  More specifically, ' &
               //            'the assignment operator did not have ' &
               //            'one of the expected forms: ''='' or ' &
               //            '''+=''. The line was ''#''. ' )

               r2 = rtrim(line)

               call errint ( '#', linnum     )
               call errch  ( '#', line(1:r2) )
               call sigerr ( 'SPICE(BADVARASSIGN)' )
               return

            end if
!
!           Locate this variable name in the name pool or insert it
!           if it isn't there.  The location will be NAMEAT and
!           we will use the variable FOUND to indicate whether or
!           not it was already present.
!
            lookat =  zzhash ( varnam )
            node   =  namlst ( lookat )
            full   =  lnknfn ( nmpool ) <= 0
            found  = .false.
!
!           See if this name (or one colliding with it in the
!           hash scheme) has already been stored in the name list.
!
            if ( node > 0 ) then

               head =  node
               tail = -nmpool(prev,head)

               do while ( node > 0 .and. .not. found )

                  found  =  names (node) == varnam
                  nameat =  node
                  node   =  nmpool(next,node)

               end do

               if (       .not. found &
                    .and. .not. full   ) then
!
!                 We didn't find this name on the conflict resolution
!                 list. Allocate a new slot for it.
!
                  call lnkan  ( nmpool, node         )
                  call lnkila ( tail,   node, nmpool )

                  names(node) =  varnam
                  nameat      =  node

               end if

            else if ( .not. full ) then
!
!              Nothing like this variable name (in the hashing sense)
!              has been loaded so far.  We need to allocate
!              a name slot for this variable.
!
               call lnkan ( nmpool, node )

               namlst(lookat) =  node
               names (node  ) =  varnam
               nameat         =  node

            end if
!
!           If the name pool was full and we didn't find this name
!           we've got an error. Diagnose it and return.
!
            if ( full .and. .not. found ) then

               call setmsg ( 'The kernel pool does not have room ' &
               //            'for any more variables.  It filled ' &
               //            'up at line # of the text buffer. ' )

               call errint ( '#', linnum             )
               call sigerr ( 'SPICE(KERNELPOOLFULL)' )
               return

            end if


!
!           Now depending upon the kind of directive, we will need
!           to remove data and allocate a new list or simply append
!           data to the existing list.
!
            if ( dirctv == eq ) then
!
!              We are going to dump whatever is associated with
!              this name and then we will need to allocate a new
!              linked list for the data.
!
               vartyp =  unknwn

               if ( found ) then
!
!                 We need to free the data associated with this
!                 variable.
!
                  datahd         = datlst( nameat )
                  datlst(nameat) = 0

                  if ( datahd < 0 ) then
!
!                    This variable was character type we need to
!                    free a linked list from the character data
!                    pool.
!
                     head = - datahd
                     tail = - chpool(prev,head)

                     call lnkfsl ( head, tail, chpool )

                  else
!
!                    This variable was numeric type. We need to
!                    free a linked list from the numeric pool.
!
                     head =   datahd
                     tail = - dppool(prev,head)

                     call lnkfsl ( head, tail, dppool )

                  end if

               end if


            else if ( dirctv == eqp ) then
!
!              We need to append to the current variable.
!
               if ( found ) then

                  if ( datlst(nameat) > 0 ) then
                     vartyp = numtyp
                  else if ( datlst(nameat) < 0 ) then
                     vartyp = strtyp
                  else
                     vartyp = unknwn
                  end if

               else
                  vartyp = unknwn
               end if

            end if

!
!           If this is a vector, the next thing on the line will be a
!           left parenthesis. Otherwise, assume that this is a scalar.
!           If it's a vector, get the first value. If it's a scalar,
!           plant a bogus right parenthesis, to make the following loop
!           terminate after one iteration.
!

            if ( type(3) == bv ) then
               nxttok        = 4
            else
               nxttok      = 3
               count       = count + 1
               type(count) = ev
            end if

!
!        For subsequent lines, treat everything as a new value.
!
         else

            nxttok = 1

         end if

!
!        We have a value anyway. Store it in the table.
!
!        Keep going until the other shoe (the right parenthesis)
!        drops, or until the end of the line is reached.
!
!        Dates begin with @; anything else is presumed to be a number.
!
         do while ( type(nxttok) /= ev .and. nxttok <= count )
!
!           Get the begin and end of this token.
!
            b = begs(nxttok)
            e = ends(nxttok)

            if ( vartyp == unknwn ) then
!
!              We need to determine which category of variable we
!              have by looking at this token and deducing the
!              type.
!
               if      ( type(nxttok) == q  ) then

                  vartyp = strtyp

               else if ( type(nxttok) == nq ) then

                  vartyp = numtyp

               else
!
!                 This is an error. We should have had one of the
!                 two previous types.
!
!                 First perform the clean up function.
!
                  call zzcln ( lookat, nameat, &
                               namlst, datlst, nmpool, chpool, dppool )


                  call setmsg ( 'The first item following the ' &
                  //            'assignment operator should be ' &
                  //            'the value of a variable or a ' &
                  //            'left parenthesis ''('' followed ' &
                  //            'by a value for a variable. This ' &
                  //            'is not true on line # of the ' &
                  //            'text buffer. ' )


                  call errint ( '#', linnum     )
                  call sigerr ( 'SPICE(BADVARASSIGN)' )
                  return

               end if

            end if


            if ( vartyp == strtyp ) then
!
!              First make sure that this token represents a string.
!
               if ( type(nxttok) /= q ) then
!
!                 First perform the clean up function.
!
                  call zzcln ( lookat, nameat, &
                               namlst, datlst, nmpool, chpool, dppool )

                  r1 = rtrim  ( varnam        )

                  call setmsg ( 'The kernel variable # has ' &
                  //            'been set up as a string ' &
                  //            'variable.  However, the value ' &
                  //            'that you are attempting to ' &
                  //            'assign to this variable on line ' &
                  //            '# of the text buffer is ' &
                  //            'not a string value. ' )

                  call errch  ( '#', varnam(1:r1) )
                  call errint ( '#', linnum       )
                  call sigerr ( 'SPICE(TYPEMISMATCH)' )
                  return

               end if
!
!              Still going? Make sure there is something between
!              the quotes.
!
               if ( b+1 >= e ) then
!
!                 First perform the clean up function.
!
                  call zzcln ( lookat, nameat, &
                               namlst, datlst, nmpool, chpool, dppool )


                  call setmsg ( 'There is a quoted string with ' &
                  //            'no characters on line # of the ' &
                  //            'text buffer. ' )

                  call errint ( '#', linnum       )
                  call sigerr ( 'SPICE(TYPEMISMATCH)' )
                  return

               end if

!
!              We are ready to go.  Allocate a node for this data
!              item. First make sure there is room to do so.
!
               free = lnknfn ( chpool )

               if ( free <= 0 ) then


                  call setmsg ( 'There is no room available for ' &
                  //            'adding another character value ' &
                  //            'to the kernel pool.  The ' &
                  //            'character values buffer became ' &
                  //            'full at line # of the text ' &
                  //            'buffer. ' )


                  call errint ( '#', linnum       )
                  call sigerr ( 'SPICE(KERNELPOOLFULL)' )
                  return


               end if
!
!              Allocate a node for storing this string value:
!
               call lnkan ( chpool, chnode )

               if ( datlst(nameat) == 0 ) then
!
!                 There was no data for this name yet.  We make
!                 CHNODE be the head of the data list for this name.
!
                  datlst(nameat) = -chnode

               else
!
!                 Put this node after the tail of the current list.
!
                  head = -datlst(nameat)
                  tail = -chpool(prev, head )

                  call lnkila ( tail, chnode, chpool )

               end if
!
!              Finally insert this data item in the data buffer
!              at CHNODE.  Note any quotes will be doubled so we
!              have to undo this affect when we store the data.
!
               chvals(chnode) = ' '
               ncomp          = ncomp + 1

               i = 1
               j = b+1

               do while ( j < e )
                  code = ichar(line(j:j))

                  if ( isquot(code) ) then
                     j = j+1
                  end if

                  chvals(chnode)(i:i) = line(j:j)
                  i = i + 1
                  j = j + 1
               end do
!
!              That's all for this value. It's now time to loop
!              back through and get the next value.
!
            else

               if ( type(nxttok) /= nq ) then
!
!                 First perform the clean up function.
!
                  call zzcln ( lookat, nameat, &
                               namlst, datlst, nmpool, chpool, dppool )

                  r1 = rtrim  ( varnam        )

                  call setmsg ( 'The kernel variable # has ' &
                  //            'been set up as a numeric or time ' &
                  //            'variable.  However, the value ' &
                  //            'that you are attempting to ' &
                  //            'assign to this variable on line ' &
                  //            '# of the kernel buffer is ' &
                  //            'not a numeric or time value. ' )

                  call errch  ( '#', varnam(1:r1) )
                  call errint ( '#', linnum       )
                  call sigerr ( 'SPICE(TYPEMISMATCH)' )
                  return

               end if
!
!              Look at the first character to see if we have a time
!              or a number.
!
               code = ichar( line(b:b) )

               if ( istime(code) ) then
!
!                 We need to have more than a single character.
!
                  if ( e == b ) then
!
!                    First perform the clean up function.
!
                     call zzcln (lookat, nameat, &
                                 namlst, datlst, nmpool, chpool, dppool)
                     r1 = rtrim (varnam        )

                     call setmsg ( 'At character # of  line # in ' &
                     //            'the text buffer the ' &
                     //            'character ''@'' appears.  This ' &
                     //            'character is reserved for ' &
                     //            'identifying time values in ' &
                     //            'assignments to kernel pool ' &
                     //            'variables.  However it is not ' &
                     //            'being used in this fashion for ' &
                     //            'the variable ''#''. ' )

                     call errint ( '#', b            )
                     call errint ( '#', linnum       )
                     call errch  ( '#', varnam(1:r1) )
                     call sigerr ( 'SPICE(BADTIMESPEC)' )
                     return

                  end if


                  call tparse ( line(b+1:e), dvalue, error )

                  if ( error /= ' ' ) then
!
!                    First perform the clean up function.
!
                     call zzcln (lookat, nameat, &
                                 namlst, datlst, nmpool, chpool, dppool)

                     call setmsg ( 'Encountered ''#'' while ' &
                     //            'attempting to parse a time ' &
                     //            'on line # of the text buffer. ' )

                     call errch  ( '#', line(b+1:e) )
                     call errint ( '#', linnum      )
                     call sigerr ( 'SPICE(BADTIMESPEC)' )
                     return
                  end if

               else

                  call nparsd ( line(b:e), dvalue, error, i )

                  if ( error /= ' ' ) then

                     call zzcln (lookat, nameat, &
                                 namlst, datlst, nmpool, chpool, dppool)

                     call setmsg ( 'Encountered ''#'' while ' &
                     //            'attempting to parse a number on ' &
                     //            'line # of the text buffer' )
                     call errch  ( '#',  line(b:e) )
                     call errint ( '#',  linnum    )
                     call sigerr ( 'SPICE(NUMBEREXPECTED)' )
                     return
                  end if

               end if
!
!              OK. We have a parsed value.  See if there is room in
!              the numeric portion of the pool to store this value.
!
               free = lnknfn ( dppool )

               if ( free <= 0 ) then


                  call setmsg ( 'There is no room available for ' &
                  //            'adding another numeric value ' &
                  //            'to the kernel pool.  The ' &
                  //            'numeric values buffer became ' &
                  //            'full at line # of the text ' &
                  //            'buffer.' )


                  call errint ( '#', linnum       )
                  call sigerr ( 'SPICE(KERNELPOOLFULL)' )
                  return


               end if

!
!              Allocate a node for storing this numeric value:
!
               call lnkan ( dppool, dpnode )

               if ( datlst(nameat) == 0 ) then
!
!                 There was no data for this name yet.  We make
!                 DPNODE be the head of the data list for this name.
!
                  datlst(nameat) =  dpnode

               else
!
!                 Put this node after the tail of the current list.
!
                  head =  datlst(nameat)
                  tail = -dppool(prev, head )

                  call lnkila ( tail, dpnode, dppool )

               end if
!
!              Finally insert this data item into the numeric buffer.
!
               dpvals(dpnode) = dvalue
               ncomp          = ncomp + 1

            end if

!
!           Now process the next token in the list of tokens.
!
            nxttok = nxttok + 1

         end do
!
!        We could have ended the above loop in one of two ways.
!
!        1) NXTTOK now exceeds count.  This means we did not reach
!           an end of vector marker.
!        2) We hit an end of vector marker.
!
         if ( nxttok > count ) then
            status = invar
         else
            status = done
         end if

      end do


!
!     It is possible that we reached this point without actually
!     assigning a value to the kernel pool variable.  This can
!     happen if there is a vector input of the form NAME = ( )
!
      if ( ncomp < 1 ) then

         call zzcln ( lookat, nameat, &
                      namlst, datlst, nmpool, chpool, dppool )

         call setmsg ( 'The first item following the ' &
         //            'assignment operator should be ' &
         //            'the value of a variable or a ' &
         //            'left parenthesis ''('' followed ' &
         //            'by a value for a variable. This ' &
         //            'is not true on line # of the ' &
         //            'text buffer. ' )


         call errint ( '#', linnum-1   )
         call sigerr ( 'SPICE(BADVARASSIGN)' )
         return

      end if


!
!     Return the name of the variable.
!
      name = varnam


      return
      end
