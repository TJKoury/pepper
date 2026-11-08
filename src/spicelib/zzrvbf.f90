!$Procedure ZZRVBF ( Private --- Pool, read the next buffer variable )

      SUBROUTINE ZZRVBF ( BUFFER, BSIZE,  LINNUM, NAMLST, &
                          NMPOOL, NAMES,  DATLST, &
                          DPPOOL, DPVALS, &
                          CHPOOL, CHVALS, &
                          VARNAM, EOF )

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

      IMPLICIT NONE
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      CHARACTER*(*)         BUFFER     (  * )
      INTEGER               BSIZE
      INTEGER               LINNUM

      INTEGER               NAMLST     (           * )
      INTEGER               NMPOOL     ( 2, LBPOOL:* )
      CHARACTER*(*)         NAMES      (           * )
      INTEGER               DATLST     (           * )
      INTEGER               DPPOOL     ( 2, LBPOOL:* )
      DOUBLE PRECISION      DPVALS     (           * )
      INTEGER               CHPOOL     ( 2, LBPOOL:* )
      CHARACTER*(*)         CHVALS     (           * )

      CHARACTER*(*)         VARNAM
      LOGICAL               EOF

      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 132 )

      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 32 )

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
      INTEGER               LASTPC
      INTEGER               LNKNFN
      INTEGER               RTRIM
      INTEGER               ZZHASH

      LOGICAL               RETURN
      LOGICAL               FAILED

!
!     Local parameters.
!
!     Below are a collection of enumerated lists that are used
!     to discern what part of the processing we are in and what
!     kind of entity we are dealing with.  First the overall
!     processing flow of a variable assignment.
!

      INTEGER               BEGIN
      PARAMETER           ( BEGIN  = 1 )

      INTEGER               DONE
      PARAMETER           ( DONE   = BEGIN  + 1 )

      INTEGER               INVAR
      PARAMETER           ( INVAR  = DONE   + 1 )

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
      INTEGER               Q
      PARAMETER           ( Q      = 1          )

      INTEGER               NQ
      PARAMETER           ( NQ     = Q      + 1 )

      INTEGER               BV
      PARAMETER           ( BV     = NQ     + 1 )

      INTEGER               EV
      PARAMETER           ( EV     = BV     + 1 )

      INTEGER               EQ
      PARAMETER           ( EQ     = EV     + 1 )

      INTEGER               EQP
      PARAMETER           ( EQP    = EQ     + 1 )

!
!     A variable can have one of three types as we process
!     it.  It can have an unknown type UNKNWN, STRTYP or NUMTYP.
!
!

      INTEGER               STRTYP
      PARAMETER           ( STRTYP = 1 )

      INTEGER               NUMTYP
      PARAMETER           ( NUMTYP = STRTYP + 1 )

      INTEGER               UNKNWN
      PARAMETER           ( UNKNWN = NUMTYP + 1 )

!
!     The next two parameters indicate which component of a linked
!     list node point to the previous node and the next node.
!
      INTEGER               PREV
      PARAMETER           ( PREV = 2 )

      INTEGER               NEXT
      PARAMETER           ( NEXT = 1 )


      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )
!
!     The next collection of variables are set up in first pass
!     through this routine.  They would be parameters if FORTRAN
!     allowed us to do this in a standard way.
!
      INTEGER               IBLANK
      INTEGER               ICOMMA
      INTEGER               IEQUAL
      INTEGER               ILPARN
      INTEGER               IPLUS
      INTEGER               IQUOTE
      INTEGER               IRPARN
      INTEGER               ITAB
      INTEGER               ITMARK


      CHARACTER*(LINLEN)    LINE
      CHARACTER*(LINLEN)    NAME

      CHARACTER*(256)       ERROR

      DOUBLE PRECISION      DVALUE

      INTEGER               AT
      INTEGER               B
      INTEGER               BADAT
      INTEGER               BEGS(LINLEN)
      INTEGER               CHNODE
      INTEGER               CODE
      INTEGER               COUNT
      INTEGER               DATAHD
      INTEGER               DIRCTV
      INTEGER               DPNODE
      INTEGER               E
      INTEGER               ENDS(LINLEN)
      INTEGER               FREE
      INTEGER               HEAD
      INTEGER               I
      INTEGER               J
      INTEGER               LOOKAT
      INTEGER               NAMEAT
      INTEGER               NCOMP
      INTEGER               NODE
      INTEGER               NXTTOK
      INTEGER               R1
      INTEGER               R2
      INTEGER               STATUS
      INTEGER               TAIL
      INTEGER               TYPE(LINLEN)
      INTEGER               VARLEN
      INTEGER               VARTYP

!
!     The logicals below are used to take apart the tokens in an
!     input line.
!
      LOGICAL               EVEN
      LOGICAL               INTOKN
      LOGICAL               INQUOT
      LOGICAL               INSEPF

      LOGICAL               FULL
      LOGICAL               FOUND
      LOGICAL               FIRST

!
!     The following logicals are in-line functions that are used
!     when processing the input strings.
!
      LOGICAL               ISSEP
      LOGICAL               ISQUOT
      LOGICAL               ISEQU
      LOGICAL               ISRPAR
      LOGICAL               ISLPAR
      LOGICAL               ISPLUS
      LOGICAL               ISBAD
      LOGICAL               ISTIME
!
!     Save everything.
!
      SAVE


      DATA                  FIRST  / .TRUE. /

!
!     Below are a collection of In-line function definitions that are
!     intended to make the code a bit easier to write and read.
!

      ISSEP ( CODE ) =      CODE .EQ. IBLANK &
                       .OR. CODE .EQ. ICOMMA &
                       .OR. CODE .EQ. ILPARN &
                       .OR. CODE .EQ. IRPARN &
                       .OR. CODE .EQ. IEQUAL &
                       .OR. CODE .EQ. ITAB

      ISQUOT( CODE ) =      CODE .EQ. IQUOTE
      ISEQU ( CODE ) =      CODE .EQ. IEQUAL
      ISRPAR( CODE ) =      CODE .EQ. IRPARN
      ISLPAR( CODE ) =      CODE .EQ. ILPARN
      ISPLUS( CODE ) =      CODE .EQ. IPLUS
      ISTIME( CODE ) =      CODE .EQ. ITMARK


      ISBAD ( DIRCTV ) =       DIRCTV .NE. EQ &
                         .AND. DIRCTV .NE. EQP

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZRVBF' )
      END IF
!
!     Initializations.
!
      IF ( FIRST ) THEN

         FIRST  = .FALSE.
         ICOMMA =  ICHAR( ','   )
         IBLANK =  ICHAR( ' '   )
         IQUOTE =  ICHAR( ''''  )
         ILPARN =  ICHAR( '('   )
         IRPARN =  ICHAR( ')'   )
         IEQUAL =  ICHAR( '='   )
         IPLUS  =  ICHAR( '+'   )
         ITMARK =  ICHAR( '@'   )
         ITAB   =  9

      END IF

!
!     No variable yet and no parsing errors so far.
!
      NAME  = ' '
      ERROR = ' '
      NCOMP = 0


!
!     Get the next data line. Unless something is terribly wrong,
!     this will begin a new variable definition. We have to read
!     the whole variable, unless we get an error, in which case
!     we can quit.
!
      STATUS = BEGIN

      DO WHILE ( ( STATUS .NE. DONE ) .AND. ( .NOT. FAILED() ) )

         LINE = ' '
!
!        We need to skip blank lines...
!
         DO WHILE ( LINE .EQ. ' ' )
            EOF    = LINNUM .GT. BSIZE

            IF ( EOF ) THEN
               CALL CHKOUT ( 'ZZRVBF' )
               RETURN
            END IF

            LINE   = BUFFER(LINNUM )
            LINNUM = LINNUM + 1
         END DO

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

         EVEN   = .TRUE.
         INTOKN = .FALSE.
         INQUOT = .FALSE.
         INSEPF = .TRUE.

         COUNT  =  0
         I      =  0

         DO WHILE ( I .LT. LEN(LINE) )
!
!           The current character is either a separator, quote or
!           some other character.
!
            I    = I + 1
            CODE = ICHAR(LINE(I:I))

            IF ( ISSEP(CODE) ) THEN
!
!              There are 3 possible states we could be in
!                 Separation Field
!                 A quoted substring with the last quote an odd one.
!                 A quoted substring with the last quote an even one.
!                 A non-quoted token.
!              In the first two cases nothing changes, but in the
!              next two cases we transition to a separation field.
!
               IF ( INTOKN .OR. ( INQUOT .AND. EVEN ) ) THEN
                  INQUOT = .FALSE.
                  INTOKN = .FALSE.
                  INSEPF = .TRUE.
               END IF

               IF ( INSEPF ) THEN
!
!                 We need to see if this is one of the special
!                 separators
!
                  IF      ( ISEQU(CODE)  ) THEN

                     COUNT       = COUNT + 1
                     BEGS(COUNT) = I
                     TYPE(COUNT) = EQ
                     ENDS(COUNT) = I

                     IF ( I .GT. 1 ) THEN
!
!                       Look back at the previous character.
!                       See if it is a plus character.
!
                        CODE = ICHAR( LINE(I-1:I-1) )

                        IF ( ISPLUS(CODE) ) THEN
!
!                          This is the directive '+=' we need
!                          to set the beginning of this token
!                          to the one before this and adjust
!                          the end of the last token.
!
                           TYPE(COUNT) = EQP
                           BEGS(COUNT) = I-1

                           IF (      BEGS(COUNT-1) &
                                .EQ. ENDS(COUNT-1) ) THEN

                              COUNT       = COUNT - 1
                              BEGS(COUNT) = I - 1
                              ENDS(COUNT) = I
                              TYPE(COUNT) = EQP

                           ELSE

                              ENDS(COUNT-1) = ENDS(COUNT-1) - 1

                           END IF

                        END IF

                     END IF

                  ELSE IF ( ISRPAR(CODE) ) THEN

                     COUNT       = COUNT + 1
                     BEGS(COUNT) = I
                     ENDS(COUNT) = I
                     TYPE(COUNT) = EV

                  ELSE IF ( ISLPAR(CODE) ) THEN

                     COUNT       = COUNT + 1
                     BEGS(COUNT) = I
                     ENDS(COUNT) = I
                     TYPE(COUNT) = BV

                  END IF

               END IF

            ELSE IF ( ISQUOT(CODE) ) THEN
!
!              There are 3 cases of interest.
!                 We are in a quoted substring already
!                 We are in a separator field
!                 We are in a non-quoted token.
!              In the first case nothing changes.  In the second
!              two cases we change to being in a quoted substring.
!
               EVEN = .NOT. EVEN

               IF ( .NOT. INQUOT ) THEN

                  INSEPF      = .FALSE.
                  INTOKN      = .FALSE.
                  INQUOT      = .TRUE.
                  COUNT       =  COUNT + 1
                  BEGS(COUNT) =  I
                  TYPE(COUNT) =  Q

               END IF

               ENDS(COUNT) = I

            ELSE
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

               IF ( INSEPF .OR. ( INQUOT .AND. EVEN ) ) THEN
                  INQUOT      = .FALSE.
                  INSEPF      = .FALSE.
                  INTOKN      = .TRUE.
                  COUNT       =  COUNT + 1
                  BEGS(COUNT) =  I
                  TYPE(COUNT) =  NQ
               END IF

               ENDS(COUNT) = I

            END IF

         END DO


!
!        The first word on the first line should be the name of a
!        variable. The second word should be a directive: = or +=.
!
         IF ( STATUS .EQ. BEGIN ) THEN
!
!           There must be at least 3 contributing tokens on this line.
!
            IF ( COUNT .LT. 3 ) THEN

               CALL SETMSG ( 'A kernel variable was not properly ' &
               //            'formed on line # text buffer.' &
               //            'Such an assignment should have the ' &
               //            'form: ''<variable name> [+]= ' &
               //            '<values>''. This line was ''#''. ' )

               R2 = RTRIM(LINE)

               CALL ERRINT ( '#', LINNUM     )
               CALL ERRCH  ( '#', LINE(1:R2) )
               CALL SIGERR ( 'SPICE(BADVARASSIGN)' )
               CALL CHKOUT ( 'ZZRVBF' )
               RETURN

            END IF
!
!           See if the variable name is legitimate:
!
            BADAT = LASTPC(LINE(BEGS(1):ENDS(1)))

            IF ( BADAT .LE. ENDS(1) - BEGS(1) ) THEN
!
!              There is a non-printing character in the variable
!              name.  This isn't allowed.
!
               AT = BEGS(1) + BADAT


               CALL SETMSG ( 'There is a non-printing character ' &
               //            'embedded in line # of the text ' &
               //            'buffer.  Non-printing ' &
               //            'characters are not allowed in ' &
               //            'kernel variable assignments.  The ' &
               //            'non-printing character has ASCII ' &
               //            'code #. ' )


               CALL ERRINT ( '#', LINNUM             )
               CALL ERRINT ( '#', ICHAR(LINE(AT:AT)) )
               CALL SIGERR ( 'SPICE(NONPRINTINGCHAR)')
               CALL CHKOUT ( 'ZZRVBF' )
               RETURN
            END IF


!
!           Check the variable name length; signal an error
!           if longer than MAXLEN.
!
            VARLEN = LEN( LINE( BEGS(1): ENDS(1) ) )

            IF ( VARLEN .GT. MAXLEN ) THEN

               CALL SETMSG ( 'A kernel pool variable name in the ' &
               //            'input buffer exceeds the maximum ' &
               //            'allowed length #1. The actual length ' &
               //            'of the variable name is #2, the ' &
               //            'offending variable name to #3 characters:' &
               //            ' ''#4''.')

               CALL ERRINT ( '#1', MAXLEN                   )
               CALL ERRINT ( '#2', VARLEN                   )
               CALL ERRINT ( '#3', LINLEN                   )
               CALL ERRCH  ( '#4', LINE( BEGS(1): ENDS(1) ) )
               CALL SIGERR ( 'SPICE(BADVARNAME)'            )

            END IF

!
!           The variable name is ok. How about the directive.
!
            VARNAM = LINE(BEGS(1):ENDS(1))
            DIRCTV = TYPE(2)


!
!           If this is replacement (=) and not an addition (+=),
!           delete the values currently associated with the variable.
!           They will be replaced later.
!
            IF ( ISBAD(DIRCTV) ) THEN

               CALL SETMSG ( 'A kernel variable was not properly ' &
               //            'formed on line # of the text buffer. ' &
               //            'Such an assignment should have the ' &
               //            'form: ''<variable name> [+]= ' &
               //            '<values>''.  More specifically, ' &
               //            'the assignment operator did not have ' &
               //            'one of the expected forms: ''='' or ' &
               //            '''+=''. The line was ''#''. ' )

               R2 = RTRIM(LINE)

               CALL ERRINT ( '#', LINNUM     )
               CALL ERRCH  ( '#', LINE(1:R2) )
               CALL SIGERR ( 'SPICE(BADVARASSIGN)' )
               CALL CHKOUT ( 'ZZRVBF' )
               RETURN

            END IF
!
!           Locate this variable name in the name pool or insert it
!           if it isn't there.  The location will be NAMEAT and
!           we will use the variable FOUND to indicate whether or
!           not it was already present.
!
            LOOKAT =  ZZHASH ( VARNAM )
            NODE   =  NAMLST ( LOOKAT )
            FULL   =  LNKNFN ( NMPOOL ) .LE. 0
            FOUND  = .FALSE.
!
!           See if this name (or one colliding with it in the
!           hash scheme) has already been stored in the name list.
!
            IF ( NODE .GT. 0 ) THEN

               HEAD =  NODE
               TAIL = -NMPOOL(PREV,HEAD)

               DO WHILE ( NODE .GT. 0 .AND. .NOT. FOUND )

                  FOUND  =  NAMES (NODE) .EQ. VARNAM
                  NAMEAT =  NODE
                  NODE   =  NMPOOL(NEXT,NODE)

               END DO

               IF (       .NOT. FOUND &
                    .AND. .NOT. FULL   ) THEN
!
!                 We didn't find this name on the conflict resolution
!                 list. Allocate a new slot for it.
!
                  CALL LNKAN  ( NMPOOL, NODE         )
                  CALL LNKILA ( TAIL,   NODE, NMPOOL )

                  NAMES(NODE) =  VARNAM
                  NAMEAT      =  NODE

               END IF

            ELSE IF ( .NOT. FULL ) THEN
!
!              Nothing like this variable name (in the hashing sense)
!              has been loaded so far.  We need to allocate
!              a name slot for this variable.
!
               CALL LNKAN ( NMPOOL, NODE )

               NAMLST(LOOKAT) =  NODE
               NAMES (NODE  ) =  VARNAM
               NAMEAT         =  NODE

            END IF
!
!           If the name pool was full and we didn't find this name
!           we've got an error. Diagnose it and return.
!
            IF ( FULL .AND. .NOT. FOUND ) THEN

               CALL SETMSG ( 'The kernel pool does not have room ' &
               //            'for any more variables.  It filled ' &
               //            'up at line # of the text buffer. ' )

               CALL ERRINT ( '#', LINNUM             )
               CALL SIGERR ( 'SPICE(KERNELPOOLFULL)' )
               CALL CHKOUT ( 'ZZRVBF' )
               RETURN

            END IF


!
!           Now depending upon the kind of directive, we will need
!           to remove data and allocate a new list or simply append
!           data to the existing list.
!
            IF ( DIRCTV .EQ. EQ ) THEN
!
!              We are going to dump whatever is associated with
!              this name and then we will need to allocate a new
!              linked list for the data.
!
               VARTYP =  UNKNWN

               IF ( FOUND ) THEN
!
!                 We need to free the data associated with this
!                 variable.
!
                  DATAHD         = DATLST( NAMEAT )
                  DATLST(NAMEAT) = 0

                  IF ( DATAHD .LT. 0 ) THEN
!
!                    This variable was character type we need to
!                    free a linked list from the character data
!                    pool.
!
                     HEAD = - DATAHD
                     TAIL = - CHPOOL(PREV,HEAD)

                     CALL LNKFSL ( HEAD, TAIL, CHPOOL )

                  ELSE
!
!                    This variable was numeric type. We need to
!                    free a linked list from the numeric pool.
!
                     HEAD =   DATAHD
                     TAIL = - DPPOOL(PREV,HEAD)

                     CALL LNKFSL ( HEAD, TAIL, DPPOOL )

                  END IF

               END IF


            ELSE IF ( DIRCTV .EQ. EQP ) THEN
!
!              We need to append to the current variable.
!
               IF ( FOUND ) THEN

                  IF ( DATLST(NAMEAT) .GT. 0 ) THEN
                     VARTYP = NUMTYP
                  ELSE IF ( DATLST(NAMEAT) .LT. 0 ) THEN
                     VARTYP = STRTYP
                  ELSE
                     VARTYP = UNKNWN
                  END IF

               ELSE
                  VARTYP = UNKNWN
               END IF

            END IF

!
!           If this is a vector, the next thing on the line will be a
!           left parenthesis. Otherwise, assume that this is a scalar.
!           If it's a vector, get the first value. If it's a scalar,
!           plant a bogus right parenthesis, to make the following loop
!           terminate after one iteration.
!

            IF ( TYPE(3) .EQ. BV ) THEN
               NXTTOK        = 4
            ELSE
               NXTTOK      = 3
               COUNT       = COUNT + 1
               TYPE(COUNT) = EV
            END IF

!
!        For subsequent lines, treat everything as a new value.
!
         ELSE

            NXTTOK = 1

         END IF

!
!        We have a value anyway. Store it in the table.
!
!        Keep going until the other shoe (the right parenthesis)
!        drops, or until the end of the line is reached.
!
!        Dates begin with @; anything else is presumed to be a number.
!
         DO WHILE ( TYPE(NXTTOK) .NE. EV .AND. NXTTOK .LE. COUNT )
!
!           Get the begin and end of this token.
!
            B = BEGS(NXTTOK)
            E = ENDS(NXTTOK)

            IF ( VARTYP .EQ. UNKNWN ) THEN
!
!              We need to determine which category of variable we
!              have by looking at this token and deducing the
!              type.
!
               IF      ( TYPE(NXTTOK) .EQ. Q  ) THEN

                  VARTYP = STRTYP

               ELSE IF ( TYPE(NXTTOK) .EQ. NQ ) THEN

                  VARTYP = NUMTYP

               ELSE
!
!                 This is an error. We should have had one of the
!                 two previous types.
!
!                 First perform the clean up function.
!
                  CALL ZZCLN ( LOOKAT, NAMEAT, &
                               NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )


                  CALL SETMSG ( 'The first item following the ' &
                  //            'assignment operator should be ' &
                  //            'the value of a variable or a ' &
                  //            'left parenthesis ''('' followed ' &
                  //            'by a value for a variable. This ' &
                  //            'is not true on line # of the ' &
                  //            'text buffer. ' )


                  CALL ERRINT ( '#', LINNUM     )
                  CALL SIGERR ( 'SPICE(BADVARASSIGN)' )
                  CALL CHKOUT ( 'ZZRVBF' )
                  RETURN

               END IF

            END IF


            IF ( VARTYP .EQ. STRTYP ) THEN
!
!              First make sure that this token represents a string.
!
               IF ( TYPE(NXTTOK) .NE. Q ) THEN
!
!                 First perform the clean up function.
!
                  CALL ZZCLN ( LOOKAT, NAMEAT, &
                               NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )

                  R1 = RTRIM  ( VARNAM        )

                  CALL SETMSG ( 'The kernel variable # has ' &
                  //            'been set up as a string ' &
                  //            'variable.  However, the value ' &
                  //            'that you are attempting to ' &
                  //            'assign to this variable on line ' &
                  //            '# of the text buffer is ' &
                  //            'not a string value. ' )

                  CALL ERRCH  ( '#', VARNAM(1:R1) )
                  CALL ERRINT ( '#', LINNUM       )
                  CALL SIGERR ( 'SPICE(TYPEMISMATCH)' )
                  CALL CHKOUT ( 'ZZRVBF' )
                  RETURN

               END IF
!
!              Still going? Make sure there is something between
!              the quotes.
!
               IF ( B+1 .GE. E ) THEN
!
!                 First perform the clean up function.
!
                  CALL ZZCLN ( LOOKAT, NAMEAT, &
                               NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )


                  CALL SETMSG ( 'There is a quoted string with ' &
                  //            'no characters on line # of the ' &
                  //            'text buffer. ' )

                  CALL ERRINT ( '#', LINNUM       )
                  CALL SIGERR ( 'SPICE(TYPEMISMATCH)' )
                  CALL CHKOUT ( 'ZZRVBF' )
                  RETURN

               END IF

!
!              We are ready to go.  Allocate a node for this data
!              item. First make sure there is room to do so.
!
               FREE = LNKNFN ( CHPOOL )

               IF ( FREE .LE. 0 ) THEN


                  CALL SETMSG ( 'There is no room available for ' &
                  //            'adding another character value ' &
                  //            'to the kernel pool.  The ' &
                  //            'character values buffer became ' &
                  //            'full at line # of the text ' &
                  //            'buffer. ' )


                  CALL ERRINT ( '#', LINNUM       )
                  CALL SIGERR ( 'SPICE(KERNELPOOLFULL)' )
                  CALL CHKOUT ( 'ZZRVBF' )
                  RETURN


               END IF
!
!              Allocate a node for storing this string value:
!
               CALL LNKAN ( CHPOOL, CHNODE )

               IF ( DATLST(NAMEAT) .EQ. 0 ) THEN
!
!                 There was no data for this name yet.  We make
!                 CHNODE be the head of the data list for this name.
!
                  DATLST(NAMEAT) = -CHNODE

               ELSE
!
!                 Put this node after the tail of the current list.
!
                  HEAD = -DATLST(NAMEAT)
                  TAIL = -CHPOOL(PREV, HEAD )

                  CALL LNKILA ( TAIL, CHNODE, CHPOOL )

               END IF
!
!              Finally insert this data item in the data buffer
!              at CHNODE.  Note any quotes will be doubled so we
!              have to undo this affect when we store the data.
!
               CHVALS(CHNODE) = ' '
               NCOMP          = NCOMP + 1

               I = 1
               J = B+1

               DO WHILE ( J .LT. E )
                  CODE = ICHAR(LINE(J:J))

                  IF ( ISQUOT(CODE) ) THEN
                     J = J+1
                  END IF

                  CHVALS(CHNODE)(I:I) = LINE(J:J)
                  I = I + 1
                  J = J + 1
               END DO
!
!              That's all for this value. It's now time to loop
!              back through and get the next value.
!
            ELSE

               IF ( TYPE(NXTTOK) .NE. NQ ) THEN
!
!                 First perform the clean up function.
!
                  CALL ZZCLN ( LOOKAT, NAMEAT, &
                               NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )

                  R1 = RTRIM  ( VARNAM        )

                  CALL SETMSG ( 'The kernel variable # has ' &
                  //            'been set up as a numeric or time ' &
                  //            'variable.  However, the value ' &
                  //            'that you are attempting to ' &
                  //            'assign to this variable on line ' &
                  //            '# of the kernel buffer is ' &
                  //            'not a numeric or time value. ' )

                  CALL ERRCH  ( '#', VARNAM(1:R1) )
                  CALL ERRINT ( '#', LINNUM       )
                  CALL SIGERR ( 'SPICE(TYPEMISMATCH)' )
                  CALL CHKOUT ( 'ZZRVBF' )
                  RETURN

               END IF
!
!              Look at the first character to see if we have a time
!              or a number.
!
               CODE = ICHAR( LINE(B:B) )

               IF ( ISTIME(CODE) ) THEN
!
!                 We need to have more than a single character.
!
                  IF ( E .EQ. B ) THEN
!
!                    First perform the clean up function.
!
                     CALL ZZCLN (LOOKAT, NAMEAT, &
                                 NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL)
                     R1 = RTRIM (VARNAM        )

                     CALL SETMSG ( 'At character # of  line # in ' &
                     //            'the text buffer the ' &
                     //            'character ''@'' appears.  This ' &
                     //            'character is reserved for ' &
                     //            'identifying time values in ' &
                     //            'assignments to kernel pool ' &
                     //            'variables.  However it is not ' &
                     //            'being used in this fashion for ' &
                     //            'the variable ''#''. ' )

                     CALL ERRINT ( '#', B            )
                     CALL ERRINT ( '#', LINNUM       )
                     CALL ERRCH  ( '#', VARNAM(1:R1) )
                     CALL SIGERR ( 'SPICE(BADTIMESPEC)' )
                     CALL CHKOUT ( 'ZZRVBF' )
                     RETURN

                  END IF


                  CALL TPARSE ( LINE(B+1:E), DVALUE, ERROR )

                  IF ( ERROR .NE. ' ' ) THEN
!
!                    First perform the clean up function.
!
                     CALL ZZCLN (LOOKAT, NAMEAT, &
                                 NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL)

                     CALL SETMSG ( 'Encountered ''#'' while ' &
                     //            'attempting to parse a time ' &
                     //            'on line # of the text buffer. ' )

                     CALL ERRCH  ( '#', LINE(B+1:E) )
                     CALL ERRINT ( '#', LINNUM      )
                     CALL SIGERR ( 'SPICE(BADTIMESPEC)' )
                     CALL CHKOUT ( 'ZZRVBF' )
                     RETURN
                  END IF

               ELSE

                  CALL NPARSD ( LINE(B:E), DVALUE, ERROR, I )

                  IF ( ERROR .NE. ' ' ) THEN

                     CALL ZZCLN (LOOKAT, NAMEAT, &
                                 NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL)

                     CALL SETMSG ( 'Encountered ''#'' while ' &
                     //            'attempting to parse a number on ' &
                     //            'line # of the text buffer' )
                     CALL ERRCH  ( '#',  LINE(B:E) )
                     CALL ERRINT ( '#',  LINNUM    )
                     CALL SIGERR ( 'SPICE(NUMBEREXPECTED)' )
                     CALL CHKOUT ( 'ZZRVBF' )
                     RETURN
                  END IF

               END IF
!
!              OK. We have a parsed value.  See if there is room in
!              the numeric portion of the pool to store this value.
!
               FREE = LNKNFN ( DPPOOL )

               IF ( FREE .LE. 0 ) THEN


                  CALL SETMSG ( 'There is no room available for ' &
                  //            'adding another numeric value ' &
                  //            'to the kernel pool.  The ' &
                  //            'numeric values buffer became ' &
                  //            'full at line # of the text ' &
                  //            'buffer.' )


                  CALL ERRINT ( '#', LINNUM       )
                  CALL SIGERR ( 'SPICE(KERNELPOOLFULL)' )
                  CALL CHKOUT ( 'ZZRVBF' )
                  RETURN


               END IF

!
!              Allocate a node for storing this numeric value:
!
               CALL LNKAN ( DPPOOL, DPNODE )

               IF ( DATLST(NAMEAT) .EQ. 0 ) THEN
!
!                 There was no data for this name yet.  We make
!                 DPNODE be the head of the data list for this name.
!
                  DATLST(NAMEAT) =  DPNODE

               ELSE
!
!                 Put this node after the tail of the current list.
!
                  HEAD =  DATLST(NAMEAT)
                  TAIL = -DPPOOL(PREV, HEAD )

                  CALL LNKILA ( TAIL, DPNODE, DPPOOL )

               END IF
!
!              Finally insert this data item into the numeric buffer.
!
               DPVALS(DPNODE) = DVALUE
               NCOMP          = NCOMP + 1

            END IF

!
!           Now process the next token in the list of tokens.
!
            NXTTOK = NXTTOK + 1

         END DO
!
!        We could have ended the above loop in one of two ways.
!
!        1) NXTTOK now exceeds count.  This means we did not reach
!           an end of vector marker.
!        2) We hit an end of vector marker.
!
         IF ( NXTTOK .GT. COUNT ) THEN
            STATUS = INVAR
         ELSE
            STATUS = DONE
         END IF

      END DO


!
!     It is possible that we reached this point without actually
!     assigning a value to the kernel pool variable.  This can
!     happen if there is a vector input of the form NAME = ( )
!
      IF ( NCOMP .LT. 1 ) THEN

         CALL ZZCLN ( LOOKAT, NAMEAT, &
                      NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )

         CALL SETMSG ( 'The first item following the ' &
         //            'assignment operator should be ' &
         //            'the value of a variable or a ' &
         //            'left parenthesis ''('' followed ' &
         //            'by a value for a variable. This ' &
         //            'is not true on line # of the ' &
         //            'text buffer. ' )


         CALL ERRINT ( '#', LINNUM-1   )
         CALL SIGERR ( 'SPICE(BADVARASSIGN)' )
         CALL CHKOUT ( 'ZZRVBF' )
         RETURN

      END IF


!
!     Return the name of the variable.
!
      NAME = VARNAM


      CALL CHKOUT ( 'ZZRVBF' )
      RETURN
      END
