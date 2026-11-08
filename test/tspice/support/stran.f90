!$Procedure     STRAN

      SUBROUTINE STRAN ( INPUT, OUTPUT, TRAN )

! $ Abstract
!
!     Translate the symbols in an input string.
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
! $ Keywords
!
!     PARSE
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         INPUT
      CHARACTER*(*)         OUTPUT
      LOGICAL               TRAN

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INPUT      I   Input string containing symbols to be translated.
!     OUTPUT     O   Output string, with all symbols translated.
!
! $ Detailed_Input
!
!     INPUT      is the input string to be translated. INPUT may contain
!                any number of known symbols.
!
!
! $ Detailed_Output
!
!     OUTPUT     is the translation of the input string. The first
!                of the symbols in INPUT will have been translated.
!                When INPUT is either a DEFINE or an UNDEFINE command,
!                OUTPUT is blank.
!
!                OUTPUT may overwrite INPUT.
!
! $ Input_Files
!
!     None.
!
! $ Output_Files
!
!     None.
!
! $ Input_Output_Common
!
!     None.
!
! $ Exceptions
!
!     The following exceptions are detected by this routine:
!
!     1)  Attempt to define or undefine a symbol that does
!         not begin with a letter.
!
!     2)  Attempt to define or undefine a symbol that ends with
!         a question mark '?' .
!
!     3)  Failure to specify a symbol to define or undefine.
!
!     4)  Attempting to define a reserved word.  The reserved
!         words are:
!
!            'START'
!            'STOP'
!            'EXIT'
!            'INQUIRE'
!            'SHOW'
!            'DEFINE'
!            'SHOW'
!            'UNDEFINE'
!            'HELP'
!
!      In all of the above cases OUTPUT is set to blank and TRAN to
!      FALSE.  No new symbol is placed in the table of symbol
!      definitions.
!
!      In all of these cases the error BAD_SYMBOL_SPC is signalled.
!
!      5) Recursive symbol definitions are detected and disallowed.
!         A long error message diagnosing the problem is set and
!         the error RECURSIVE_SYMBOL is signalled.
!
!      5) Overflow of the input command caused by symbol resolution.
!
!         In this case the OUTPUT is left at the state it had reached
!         prior to the overflow condition and TRAN is returned as
!         FALSE. The error SYMBOL_OVERFLOW is signalled.
!
! $ Detailed_Description
!
!     A new symbol may be defined with the DEFINE command. The
!     syntax is:
!
!            DEFINE  <symbol>  <definition>
!
!     where <symbol> is a valid symbol name and <definition> is any
!     valid definition. The DEFINE command, the symbol name, and the
!     definition are delimited by blanks.
!
!     When a symbol is defined, the symbol and definition are inserted
!     into the symbol table.
!
!     An existing symbol may be removed from the table with the
!     UNDEFINE command. The syntax is:
!
!            UNDEFINE <symbol>
!
!     where <symbol> is the name of an existing symbol. The UNDEFINE
!     command and the symbol name are delimited by blanks.
!
!     If the input string does not contain a definition statement,
!     STRANS searches the input string for potential symbol names.
!     When a valid symbol is encountered, it is removed from the
!     string and replaced by the corresponding definition. This
!     continues until no untranslated symbols remain.
!
! $ Examples
!
!     Suppose that we are given the following definitions:
!
!            DEFINE  BODIES      PLANET AND SATS
!            DEFINE  EUROPA      502
!            DEFINE  GANYMEDE    503
!            DEFINE  IO          501
!            DEFINE  JUPITER     599
!            DEFINE  PLANET      JUPITER
!            DEFINE  CALLISTO    504
!            DEFINE  SATS        IO EUROPA GANYMEDE CALLISTO
!
!      Then the string 'BODIES AND SOULS' would translate,
!      at various stages, to:
!
!           'PLANET AND SATS AND SOULS'
!
!           'JUPITER AND SATS AND SOULS'
!
!           '599 AND SATS AND SOULS'
!
!           '599 AND IO EUROPA GANYMEDE CALLISTO AND SOULS'
!
!           '599 AND 501 EUROPA GANYMEDE CALLISTO AND SOULS'
!
!           '599 AND 501 502 GANYMEDE CALLISTO AND SOULS'
!
!           '599 AND 501 502 503 CALLISTO AND SOULS'
!
!           '599 AND 501 502 503 504 AND SOULS'
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     I. M. Underwood (JPL)
!
! $ Version_and_Date
!
!     Version 1.2.0 29-Aug-1996 (WLT)
!
!        Fixed the error message for the case in which someone
!        tries to create a symbol that is more than 32 characters
!        in length.
!
!     Version 1.1, 14-SEP-1995
!
!        Reference to unused variable WORD deleted.
!
!     Version 1,    8-SEP-1986
!
!-&
!     SPICELIB Functions
!
      INTEGER               CARDC
      INTEGER               ISRCHC
      INTEGER               LASTNB
      INTEGER               NCPOS
      INTEGER               RTRIM
      LOGICAL               FAILED
      LOGICAL               MATCHM
      LOGICAL               RETURN
!
!     Other supporting functions
!
      LOGICAL               BATCH

!
!     The following parameters are used to define our table
!     of symbol translations.
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF = 0  )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 1024 )

!
!     Longest allowed symbol name is given by WDSIZE
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE  = 32 )
!
!     Maximum number of allowed symbols is MAXN
!
      INTEGER               MAXN
      PARAMETER           ( MAXN   = 200 )
!
!     The longest we expect any symbol to be is MAXL characters
!
      INTEGER               MAXL
      PARAMETER           ( MAXL = 256 )
!
!     The average number of characters per symbol is AVGL
!
      INTEGER               AVGL
      PARAMETER           ( AVGL = 64 )


      INTEGER               PTRSIZ
      PARAMETER           ( PTRSIZ = MAXN*4 + 4 )

      INTEGER               MAXCHR
      PARAMETER           ( MAXCHR = MAXN *AVGL )

      INTEGER               NLINES
      PARAMETER           ( NLINES = MAXCHR/MAXL + 1 )
!
!     Finally, here are the arrays used to hold the symbol translations.
!

      CHARACTER*(WDSIZE)    NAMES  ( LBCELL: MAXN   )
      INTEGER               PTRS   ( LBCELL: PTRSIZ )
      CHARACTER*(MAXL)      BUFFER ( LBCBUF: NLINES )

!
!     Here's the storage we need for the reserved words.
!
      INTEGER               NRES
      PARAMETER           ( NRES = 12 )

      CHARACTER*(1)         DELIM


      CHARACTER*(WDSIZE)    RESVRD ( NRES )

      INTEGER               BIGWRD
      PARAMETER           ( BIGWRD = 33 )

      CHARACTER*(BIGWRD)    SYMBL
      CHARACTER*(BIGWRD)    SYMBOL

      CHARACTER*(WDSIZE)    KEY

      CHARACTER*(80)        PATTRN
      CHARACTER*(80)        MYPRMT


      CHARACTER*(WDSIZE)    ALPHAB
      CHARACTER*(LNSIZE)    DEF

      CHARACTER*(1)         EQUOTE


      INTEGER               I
      INTEGER               J
      INTEGER               L
      INTEGER               LDEF
      INTEGER               LENO
      INTEGER               LOC
      INTEGER               LOUT
      INTEGER               LSTTRY
      INTEGER               LSYM
      INTEGER               N
      INTEGER               NNAME
      INTEGER               NXTCHR
      INTEGER               PLACE
      INTEGER               PSIZE
      INTEGER               SLOT
      INTEGER               VDIM

      LOGICAL               CHECK ( MAXN )
      LOGICAL               CHECKD( MAXN )
      LOGICAL               FIRST
      LOGICAL               GOTONE
      LOGICAL               NEW

      SAVE

      DATA                  FIRST / .TRUE. /
!
!     Set up all of the data structures and special strings in
!     the first pass through the routine.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'STRAN' )

      IF ( FIRST ) THEN

         FIRST = .FALSE.

         VDIM  = NLINES
         PSIZE = PTRSIZ
         NNAME = MAXN

         CALL SBINIT_1 ( NNAME, PSIZE, VDIM, NAMES, &
                                             PTRS, &
                                             BUFFER )

         RESVRD(  1 ) = 'START'
         RESVRD(  2 ) = 'STOP'
         RESVRD(  3 ) = 'EXIT'
         RESVRD(  4 ) = 'INQUIRE'
         RESVRD(  5 ) = 'SHOW'
         RESVRD(  6 ) = 'DEFINE'
         RESVRD(  7 ) = 'SHOW'
         RESVRD(  8 ) = 'UNDEFINE'
         RESVRD(  9 ) = 'HELP'
         RESVRD( 10 ) = 'RECALL'
         RESVRD( 11 ) = 'DO'
         RESVRD( 12 ) = 'EDIT'

         ALPHAB       = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'


      END IF
!
!     Find out what the special marker character is for suppressing
!     symbol evaluation.
!
      CALL GETEQ ( EQUOTE )

!
!     Is this a definition statement? The presence of DEFINE, INQUIRE or
!     UNDEFINE at the beginning of the string will confirm this.
!
      CALL NTHWD ( INPUT, 1, KEY, LOC )
      CALL UCASE ( KEY,      KEY      )

!
!     The keyword must be followed by a valid symbol name.
!
      IF (      KEY .EQ. 'DEFINE' &
           .OR. KEY .EQ. 'INQUIRE' &
           .OR. KEY .EQ. 'UNDEFINE' ) THEN

         CALL NTHWD ( INPUT, 2, SYMBL,  LOC )
         CALL UCASE ( SYMBL,    SYMBOL      )
         L =  RTRIM ( SYMBOL )

         IF ( SYMBOL .EQ. ' ' ) THEN

            OUTPUT = ' '
            TRAN   = .FALSE.
            CALL SETMSG ( 'The "#" command must be followed by the ' &
            //            'name of the symbol that you want to ' &
            //            '#. ' )
            CALL ERRCH  ( '#', KEY )
            CALL LCASE  ( KEY, KEY )
            CALL ERRCH  ( '#', KEY )
            CALL SIGERR ( 'BAD_SYMBOL_SPEC' )
            CALL CHKOUT ( 'STRAN' )
            RETURN

         ELSE IF ( INDEX ( ALPHAB, SYMBOL(1:1) ) .EQ. 0 ) THEN


            OUTPUT = ' '
            TRAN   = .FALSE.
            CALL LCASE ( KEY, KEY )

            CALL SETMSG ( 'You cannot # "#".  Symbols must begin ' &
            //            'with a letter (A-Z) ' )
            CALL ERRCH  ( '#', KEY    )
            CALL ERRCH  ( '#', SYMBOL )
            CALL SIGERR ( 'BAD_SYMBOL_SPEC' )
            CALL CHKOUT ( 'STRAN' )
            RETURN

         ELSE IF ( L .GT. 32 ) THEN

            OUTPUT = ' '
            TRAN   = .FALSE.
            CALL LCASE ( KEY, KEY )
            CALL SETMSG ( 'You cannot # "#...".  Symbols may not ' &
            //            'be longer than 32 characters in length.' )
            CALL ERRCH  ( '#', KEY    )
            CALL ERRCH  ( '#', SYMBOL )
            CALL SIGERR ( 'BAD_SYMBOL_SPEC' )
            CALL CHKOUT ( 'STRAN' )
            RETURN

         ELSE IF ( SYMBOL(L:L) .EQ. '?' ) THEN

            OUTPUT = ' '
            TRAN   = .FALSE.
            CALL LCASE ( KEY, KEY )

            CALL SETMSG ( 'You cannot # "#".  Symbols may not ' &
            //            'end with a question mark ''?''. ' )
            CALL ERRCH  ( '#', KEY    )
            CALL ERRCH  ( '#', SYMBOL )
            CALL SIGERR ( 'BAD_SYMBOL_SPEC' )
            CALL CHKOUT ( 'STRAN' )
            RETURN

         ELSE IF (     ( KEY .EQ. 'DEFINE' .OR. KEY .EQ. 'INQUIRE' ) &
                   .AND. ISRCHC ( SYMBOL, NRES, RESVRD ) .GT. 0 ) THEN


            OUTPUT = ' '
            TRAN   = .FALSE.

            CALL SETMSG ( 'The word ''#'' is a reserved word. ' &
            //            'You may not redefine it. ' )
            CALL ERRCH  ( '#', SYMBOL )
            CALL SIGERR ( 'BAD_SYMBOL_SPEC' )
            CALL CHKOUT ( 'STRAN' )
            RETURN

         END IF

      END IF


      IF ( KEY .EQ. 'INQUIRE' ) THEN
!
!        First of all we, can only INQUIRE for symbol definitions
!        if the program is not running in "batch" mode.
!
         IF ( BATCH() ) THEN

            CALL SETMSG ( 'You''ve attempted to INQUIRE for the ' &
            //            'value of a symbol while the program ' &
            //            'is running in "batch" mode. You can ' &
            //            'INQUIRE for a symbol value only if ' &
            //            'you are running in INTERACTIVE mode. ' )
            CALL SIGERR ( 'WRONG_MODE' )
            CALL CHKOUT ( 'STRAN' )
            RETURN

         END IF
!
!        See if there is anything following the symbol that is
!        to be defined.  This will be used as our prompt value.
!
         NXTCHR = MAX( LOC+L, NCPOS ( INPUT, ' ', LOC + L ) )

         IF ( INPUT(NXTCHR:) .NE. ' ' ) THEN

            MYPRMT = INPUT(NXTCHR:)

         ELSE

            MYPRMT = 'Enter definition for'
            CALL SUFFIX ( SYMBOL, 1, MYPRMT )
            CALL SUFFIX ( '>',    1, MYPRMT )

         END IF

         CALL GETDEL ( DELIM  )
         CALL RDSTMN ( MYPRMT, DELIM, DEF )

         CALL SBSET_1 ( SYMBOL, DEF, NAMES, &
                                     PTRS, &
                                     BUFFER )

      END IF
!
!     If this is a definition, and the symbol already exists in the
!     symbol table, simply replace the existing definition with the
!     string following the symbol name. If this is a new symbol,
!     find the first symbol in the list that should follow the new
!     one. Move the rest of the symbols back, and insert the new one
!     at this point.
!
      IF ( KEY .EQ. 'DEFINE' ) THEN

         NXTCHR = MAX( LOC+L, NCPOS ( INPUT, ' ', LOC + L ) )

         CALL SBSET_1 ( SYMBOL, INPUT(NXTCHR:), NAMES, &
                                                PTRS, &
                                                BUFFER )

      END IF

      IF ( KEY .EQ. 'DEFINE'  .OR. KEY .EQ. 'INQUIRE' ) THEN


         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'STRAN' )
            RETURN
         END IF
!
!        Now check for a recursive definition.  To do this we have
!        two parallel arrays to the NAMES array of the string
!        buffer.  The first array CHECK is used to indicate that
!        in the course of the definition resolution of the
!        new symbol, another symbol shows up.  The second array
!        called CHECKD indicats whether or not we have examined this
!        existing symbol to see if contains the newly created
!        symbol as part of its definition.
!
!        So far we have nothing to check and haven't checked anything.
!
         N = CARDC ( NAMES )

         DO J = 1, N
            CHECK (J) = .FALSE.
            CHECKD(J) = .FALSE.
         END DO
!
!        Find the location of our new symbol in the NAMES cell.
!

         PLACE = ISRCHC ( SYMBOL, N, NAMES(1) )

         NEW = .TRUE.

         DO WHILE ( NEW )
!
!           Look up the definition currently associated with
!           the symbol we are checking.
!
            CALL SBGET_1 ( SYMBOL, NAMES, &
                                   PTRS, &
                                   BUFFER, DEF, I )

            J = 1
            CALL NTHUQW ( DEF, J, EQUOTE, SYMBOL, LOC )

            DO WHILE ( LOC .GT. 0 )

               CALL   UCASE  ( SYMBOL, SYMBOL )
               SLOT = ISRCHC ( SYMBOL, N, NAMES(1) )
!
!              If the word is located in the same place as the
!              symbol we've just defined, we've introduced
!              a recursive symbol definition.  Remove this
!              symbol and diagnose the error.
!
               IF ( SLOT .EQ. PLACE ) THEN

                  OUTPUT = ' '
                  TRAN   = .FALSE.
                  SYMBOL =  NAMES(PLACE)
                  CALL SBREM_1 ( SYMBOL, NAMES, PTRS, BUFFER )


                  CALL SETMSG ( 'The definition of ''#'' is ' &
                  //            'recursive.  Recursively defined ' &
                  //            'symbol definitions are not ' &
                  //            'allowed. ' )

                  CALL ERRCH  ( '#', SYMBOL )
                  CALL SIGERR ( 'RECURSIVE_SYMBOL'  )
                  CALL CHKOUT ( 'STRAN' )
                  RETURN

               ELSE IF ( SLOT .GT. 0 ) THEN
!
!                 Otherwise if this word is in the names list
!                 we may need to check this symbol to see if
!                 it lists the just defined symbol in its definition.
!
                  IF ( CHECKD(SLOT) ) THEN
                     CHECK(SLOT) = .FALSE.
                  ELSE
                     CHECK(SLOT) = .TRUE.
                  END IF

               END IF
!
!              Locate the next unquoted word in the definition.
!
               J = J + 1
               CALL NTHUQW ( DEF, J, EQUOTE, SYMBOL, LOC )

            END DO
!
!           See if there are any new items to check.  If there
!           are create a new value for symbol, and mark the
!           new item as being checked.
!
            NEW = .FALSE.

            DO J = 1, N

               IF ( CHECK(J) .AND. .NOT. NEW ) THEN
                  SYMBOL    =  NAMES(J)
                  CHECK (J) = .FALSE.
                  CHECKD(J) = .TRUE.
                  NEW       = .TRUE.
               END IF

            END DO


         END DO
!
!        If we get to this point, we have a new non-recursively
!        defined symbol.
!
         OUTPUT = ' '
         TRAN   = .FALSE.
         CALL CHKOUT ( 'STRAN' )
         RETURN

      END IF



!
!     If this is a deletion, and the symbol already exists in the
!     symbol table, simply move the symbols that follow toward the
!     front of the table.
!
      IF ( KEY .EQ. 'UNDEFINE' ) THEN

         CALL SBREM_1 ( SYMBOL, NAMES, &
                                PTRS, &
                                BUFFER )
         OUTPUT = ' '
         TRAN   = .FALSE.
         CALL CHKOUT ( 'STRAN' )
         RETURN

      END IF



!
!     This is not a definition statement. Look for potential symbols.
!     Try to resolve the first symbol in the string by substituting the
!     corresponding definition for the existing symbol.
!
      OUTPUT = INPUT
      TRAN   = .FALSE.
      J      = 1

      CALL NTHUQW ( OUTPUT, J, EQUOTE, SYMBOL, LOC )

      DO WHILE ( ( .NOT. TRAN ) .AND. ( SYMBOL .NE. ' ' ) )

         CALL UCASE   ( SYMBOL, SYMBOL )
         CALL SBGET_1 ( SYMBOL, NAMES, &
                                   PTRS, &
                                   BUFFER, DEF, I )

         IF ( I .GT. 0 ) THEN

            LSYM   = LASTNB ( SYMBOL )
            LDEF   = LASTNB ( DEF    ) + 1
            LOUT   = LASTNB ( OUTPUT )
            LENO   = LEN    ( OUTPUT )


            IF ( LOUT - LSYM + LDEF .GT. LENO ) THEN

               TRAN = .FALSE.

               CALL SETMSG ( 'As a result of attempting to ' &
               //            'resolve the symbols in the input ' &
               //            'command, the command has ' &
               //            'overflowed the allocated memory. ' &
               //            'This is may be due to ' &
               //            'unintentionally using symbols that ' &
               //            'you had not intended to use.  You ' &
               //            'may protect portions of your ' &
               //            'string from symbol evaluation by ' &
               //            'enclosing that portion of your ' &
               //            'string between the character # as ' &
               //            'in ''DO #THIS PART WITHOUT ' &
               //            'SYMBOLS#'' . ' )

               CALL ERRCH  ( '#', EQUOTE )
               CALL ERRCH  ( '#', EQUOTE )
               CALL ERRCH  ( '#', EQUOTE )
               CALL SIGERR ( 'SYMBOL_OVERFLOW' )
               CALL CHKOUT ( 'STRAN' )
               RETURN

            END IF

            CALL REPSUB ( OUTPUT, LOC, LOC+LSYM-1, DEF(1:LDEF), &
                          OUTPUT )

            TRAN = .TRUE.

         ELSE

            J = J + 1

         END IF

         CALL NTHUQW ( OUTPUT, J, EQUOTE, SYMBOL, LOC )

      END DO

      CALL CHKOUT ( 'STRAN' )
      RETURN

!
!     The following entry point allows us to set up a search
!     of defined symbols that match a wild-card pattern.  It must
!     be called prior to getting any symbol definitions.
!
      ENTRY SYMPAT ( INPUT )

         LSTTRY = 0
         PATTRN = INPUT
         RETURN

!
!     The following entry point fetches the next symbol and its
!     definition for the next SYMBOL whose name
!     matches a previously supplied template via the entry point
!     above --- SYMPAT.
!
!     If there is no matching symbol, we get back blanks.  Note
!     that no translation of the definition is performed.
!
      ENTRY SYMGET ( INPUT, OUTPUT )

         INPUT  = ' '
         OUTPUT = ' '
         N      = CARDC(NAMES)
         DO WHILE ( LSTTRY .LT. N )

            LSTTRY = LSTTRY + 1
            GOTONE = MATCHM ( NAMES(LSTTRY), PATTRN, '*','%','~','|' )

            IF ( GOTONE ) THEN

               SYMBOL = NAMES(LSTTRY)
               INPUT  = NAMES(LSTTRY)

               CALL SBGET_1 ( SYMBOL, NAMES, &
                                      PTRS, &
                                      BUFFER, OUTPUT, I )
               RETURN

            END IF

         END DO
         RETURN


      END
