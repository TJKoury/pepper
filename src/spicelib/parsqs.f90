!$Procedure PARSQS ( Parse quoted string token )

      subroutine parsqs ( string, qchar, value, length, error, errmsg, &
                          ptr                                          )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Parse a quoted string token.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     PARSING
!     SCANNING
!     STRING
!     UTILITY
!
! $ Declarations

      implicit none

      character*(*)         string
      character*(1)         qchar
      character*(*)         value
      integer(ip)           length
      logical               error
      character*(*)         errmsg
      integer(ip)           ptr

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   Quoted string to be parsed.
!     QCHAR      I   Quote delimiter character.
!     VALUE      O   Parsed string.
!     LENGTH     O   Number of significant characters in VALUE.
!     ERROR      O   Logical error flag.
!     ERRMSG     O   Message indicating whether errors have occurred.
!     PTR        O   Position in string where an error occurred.
!
! $ Detailed_Input
!
!     STRING   is a character string containing a `quoted string
!              token'. Quoted string tokens are sequences of
!              characters that represent literal strings.
!              Syntactically, a string token is a sequence of
!              characters that begins and ends with a designated
!              `quote character'. Within the token, any
!              occurrence of the quote character is indicated by
!              an adjacent pair of quote characters: for example,
!              if the quote character is
!
!                 "
!
!              then the token representing one instance of this
!              character is
!
!                 """"
!
!              Here the first quote indicates the beginning of the
!              token, the next two quotes together indicate a
!              single quote character that constitutes the
!              `contents' of the token, and the final quote
!              indicates the end of the token.
!
!              Leading and trailing blanks in STRING are ignored.
!              The input string may not contain any trailing,
!              non-blank characters after the final quote
!              character.
!
!              All blanks occurring between the bracketing
!              quote characters in STRING are significant.
!
!
!     QCHAR    is the quote character. This is always a single
!              character. The characters
!
!                 "  and '
!
!              are common choices, but any non-blank character is
!              accepted. Case *is* significant in QCHAR.
!
! $ Detailed_Output
!
!     VALUE    is the string resulting from parsing STRING.
!              VALUE is obtained from STRING by removing the
!              bracketing quote characters and replacing each pair
!              of quote characters in the interior of STRING with
!              a singleton quote character. The value resulting
!              from parsing STRING will occupy the leftmost
!              characters of VALUE, but will not be
!              `left-justified', since leading blanks within
!              the quoted string token in STRING are significant.
!
!     LENGTH   is the number of significant characters in VALUE.
!              This is the number of characters in the string
!              resulting from parsing the input string. Because
!              parsed strings containing embedded quote
!              characters are shorter than the unparsed tokens
!              that represent them, LENGTH may be less than the
!              number of characters between the bracketing quote
!              characters of the input string.
!
!     ERROR    is a logical flag indicating whether a parse error
!              occurred; if so, ERROR is returned with the value
!              .TRUE.
!
!     ERRMSG   is a message indicating that STRING could not be
!              parsed due to an error in its structure. If the
!              input string token was successfully parsed, ERRMSG
!              will be returned as a blank string.
!
!     PTR      indicates the character position at which an
!              error in STRING was detected. If STRING is
!              correctly formed, PTR is returned as 0.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the input argument QCHAR is blank, a parse error will be
!         indicated by ERROR; PTR will be set to 1. The contents of
!         VALUE and LENGTH are undefined in this case.
!
!     2)  If STRING is not a well-formed quoted string, a parse error
!         will be indicated by ERROR and PTR. The contents of VALUE
!          and LENGTH are undefined in this case.
!
!     3)  If the length of the output string VALUE is too short to
!         accommodate the parsed string token produced by this routine,
!         a parse error message to this effect is generated.  VALUE
!         will contain the as much as possible of the result, truncated
!         on the right.
!
!     4)  If STRING consists of a null string token, that is, two
!         adjacent quote characters with nothing but blanks on either
!         side, a parse error will be indicated. The contents of VALUE
!          and LENGTH are undefined in this case.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Quote characters may be ANY non-blank character. For example, the
!     ampersand
!
!        &
!
!     is a perfectly valid quote character. If we were using the
!     ampersand as the quote character, then the term `doubled quote'
!     in the following discussion would refer to the sequence
!
!        &&
!
!     not the character
!
!        "
!
!     The string tokens that are expected inputs to this routine are
!     Fortran-style quoted strings: they start and end with quote
!     characters. In the interior of any such token, any quote
!     characters are represented by doubled quote characters. These
!     rules imply that the number of quote characters in a valid quoted
!     string token is always even. The end of a quoted string token is
!     located at the first even-numbered quote character, counting from
!     the initial quote character, that is  not the first member of a
!     pair of quotes indicating an embedded quote character.
!
!     This routine is meant to be used together with the SPICELIB
!     routine LXQSTR (Lex quoted string):  LXQSTR is used to identify
!     quoted string tokens, and this routine converts the tokens to
!     string values.
!
! $ Examples
!
!     1)  The table below illustrates the action of this routine.
!
!
!     STRING               QCHAR   VALUE           LENGTH       ERROR
!     =================================================================
!     "SPICE"              "       SPICE           5            .FALSE.
!     "SPICE"              '       <undefined>     <undefined>  .TRUE.
!     """SPICE"" system"   "       "SPICE" system  14           .FALSE.
!     " "                  "       <single blank>  1            .FALSE.
!     ''                   '       <undefined>     <undefined>  .TRUE.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 03-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 08-MAY-1996 (WLT)
!
!        Corrected the problem with an uninitialized variable
!        INLEN that was detected on the HP and reported by Steve
!        Schlaifer of MASL.
!
!-    SPICELIB Version 1.0.0, 21-NOV-1994 (NJB)
!
!-&


! $ Index_Entries
!
!     parse quoted string token
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           frstnb
      integer(ip)           lastnb

!
!     Local variables
!
      character*(1)         chr

      integer(ip)           first
      integer(ip)           ipos
      integer(ip)           inlen
      integer(ip)           last
      integer(ip)           outlen
      integer(ip)           opos


!
!     Error free, no check-in required.  No parse error to start with.
!     No characters in the parsed string to start with.
!
      error  = .false.
      errmsg = ' '
      ptr    = 0
      length = 0

!
!     Reject invalid quote characters.
!
      if ( qchar .eq. ' ' ) then

         error  = .true.
         errmsg = 'The quote character must be non-blank, but isn''t'
         ptr    = 1

      end if

!
!     Grab the lengths of the string arguments.
!
      inlen  =  len(string)
      outlen =  len(value )

!
!     The token to be parsed extends from the first non-blank
!     character to the last non-blank character of STRING.
!
      first  =  frstnb(string)
      last   =  lastnb(string)

      if ( first .eq. 0 ) then

         error  = .true.
         errmsg = 'Blank input string'
         ptr    =  inlen
         return

      end if


!
!     The input token must be bracketed by quote characters.
!
      if ( string(first:first) .ne. qchar ) then

         error  = .true.
         errmsg = 'String token does not start with quote character'
         ptr    =  first
         return

      else if ( string(last:last) .ne. qchar ) then

         error  = .true.
         errmsg = 'String token does not end with quote character'
         ptr    =  last
         return

      end if


!
!     Null strings are not accepted.
!
      if ( first .eq. last-1 ) then

         error  = .true.
         errmsg = 'Null (zero length) string token'
         ptr    = last
         return

      end if


!
!     Transfer the interior characters of the input string to the output
!     string, replacing each doubled quote character with a single quote
!     character.  The interior of the string must not contain any
!     un-doubled quotes; we have a parse error if we find any such
!     stragglers.
!
      opos  =  1
      ipos  =  first + 1

      do while (  ( ipos .le. last-1 ) .and. ( opos .le. outlen )  )
!
!        At this point, IPOS points to the current input character to
!        examine; OPOS points to the currently available position to
!        write to in the output string.
!
         chr  =  string(ipos:ipos)

         if ( chr .ne. qchar ) then
!
!           This is the normal, non-quote case.  Transfer the
!           character to the output string and advance both the input
!           and output character positions.
!
            value(opos:opos)  =  chr
            ipos              =  ipos   + 1
            opos              =  opos   + 1
            length            =  length + 1

         else
!
!           We've encountered a quote character.  By construction, the
!           parity of this quote character must be odd.  The quote must
!           be followed immediately by a second, interior quote.
!
            if ( ipos .eq. last-1 ) then
!
!              We're already looking at the last interior input
!              character.
!
               error  = .true.
               errmsg = 'Quote character is unmatched or else string '// &
                        'ends without final quote; take your pick'
               ptr    = ipos
               return

            else if ( string(ipos+1:ipos+1) .ne. qchar ) then

               error  =  .true.
               errmsg =  'Interior quote character is not doubled'
               ptr    =  ipos
               return

            else
!
!              This is the normal case; the quote character is doubled.
!              Transfer a single quote character to the output string,
!              and skip over the second quote in the input string.
!
               value(opos:opos)  =  chr
               opos              =  opos   + 1
               length            =  length + 1
               ipos              =  ipos   + 2

            end if

         end if

      end do


      if ( ipos .lt. last-1 ) then
!
!        We must have stopped transferring characters to VALUE
!        because we ran out of room.
!
         error  =  .true.
         errmsg =  'Output string too short, truncated on right'
         ptr    =  ipos
         return

      end if


      if ( opos .lt. outlen ) then
!
!        Blank-pad the trailing portion of the output string.
!
         value(opos:) = ' '

      end if


      return
      end
