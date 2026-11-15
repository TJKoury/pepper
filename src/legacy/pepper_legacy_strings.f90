!> The legacy interface's routines of the subsystem strings (65 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_strings

   implicit none

   private
   public :: astrip, bedec, beint, benum, beuns, cmprss, cpos, cposr, eqstr, esrchc, fndnwd, &
      frstnb, frstnp, frstpc, kxtrct, lastnb, lastpc, lbuild, lcase, ljucrs, ljust, lparse, &
      lparsm, ltrim, lxqstr, matchw, nblen, nbwid, ncpos, ncposr, nextwd, nthwd, parsqs, pcwid, &
      pos, posr, prefix, quote, replch, replwd, repmc, rjust, rtrim, samch, samsub, scanrj, &
      shiftl, shiftr, sigdgt, suffix, ucase, wdcnt, wdindx, zzinssub, zzrepsub, intstr, dpstr, &
      eqchr, nechr, samchi, samsbi, matchi, nparsd, nparsi, lx4uns

contains

      !> Remove from a character string all characters which fall
      !> between specified starting and ending characters, inclusive.
      !>
      !> Legacy interface: calls astrip of module pepper_strings.
      subroutine astrip ( instr, asciib, asciie, outstr )
      use pepper_strings, only: pepper_astrip => astrip
      use pepper_kinds, only: ip
      implicit none
      character(*) :: instr  !! Input string.
      character(1) :: asciib  !! First ASCII character in range to be stripped.
      character(1) :: asciie  !! Last ASCII character in range to be stripped.
      character(*) :: outstr  !! Output (stripped) string.
      ! PEPPER: instr, asciib, asciie are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_astrip ( (instr), (asciib), (asciie), outstr )
      end subroutine astrip

      !> Determine whether a string represents a decimal number.
      !>
      !> Legacy interface: calls bedec of module pepper_strings.
      logical function bedec  ( string )
      use pepper_strings, only: pepper_bedec => bedec
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! Character string.
      bedec = pepper_bedec ( string )
      end function bedec

      !> Determine whether a string represents an integer.
      !>
      !> Legacy interface: calls beint of module pepper_strings.
      logical function beint  ( string )
      use pepper_strings, only: pepper_beint => beint
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! Character string.
      beint = pepper_beint ( string )
      end function beint

      !> Determine whether a string represents a number.
      !>
      !> Legacy interface: calls benum of module pepper_strings.
      logical function benum  ( string )
      use pepper_strings, only: pepper_benum => benum
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! Character string.
      benum = pepper_benum ( string )
      end function benum

      !> Determine whether a string represents an unsigned integer.
      !>
      !> Legacy interface: calls beuns of module pepper_strings.
      logical function beuns  ( string )
      use pepper_strings, only: pepper_beuns => beuns
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! Character string.
      beuns = pepper_beuns ( string )
      end function beuns

      !> Compress a character string by removing occurrences of
      !> more than N consecutive occurrences of a specified
      !> character.
      !>
      !> Legacy interface: calls cmprss of module pepper_strings.
      subroutine cmprss ( delim, n, input, output )
      use pepper_strings, only: pepper_cmprss => cmprss
      use pepper_kinds, only: ip
      implicit none
      character(1) :: delim  !! Delimiter to be compressed.
      integer(ip) :: n  !! Maximum consecutive occurrences of DELIM.
      character(*) :: input  !! Input string.
      character(*) :: output  !! Compressed string.
      ! PEPPER: delim, input are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_cmprss ( (delim), n, (input), output )
      end subroutine cmprss

      !> Find the first occurrence in a string of a character belonging
      !> to a collection of characters, starting at a specified location,
      !> searching forward.
      !>
      !> Legacy interface: calls cpos of module pepper_strings.
      integer(ip) function cpos ( str, chars, start )
      use pepper_strings, only: pepper_cpos => cpos
      use pepper_kinds, only: ip
      implicit none
      character(*) :: str  !! Any character string.
      character(*) :: chars  !! A collection of characters.
      integer(ip) :: start  !! Position to begin looking for one of CHARS
      cpos = pepper_cpos ( str, chars, start )
      end function cpos

      !> Find the first occurrence in a string of a character belonging
      !> to a collection of characters, starting at a specified location,
      !> searching in reverse.
      !>
      !> Legacy interface: calls cposr of module pepper_strings.
      integer(ip) function cposr ( str, chars, start )
      use pepper_strings, only: pepper_cposr => cposr
      use pepper_kinds, only: ip
      implicit none
      character(*) :: str  !! Any character string.
      character(*) :: chars  !! A collection of characters.
      integer(ip) :: start  !! Position to begin looking for one of CHARS
      cposr = pepper_cposr ( str, chars, start )
      end function cposr

      !> Determine whether two strings are equivalent.
      !>
      !> Legacy interface: calls eqstr of module pepper_strings.
      logical function eqstr ( a, b )
      use pepper_strings, only: pepper_eqstr => eqstr
      use pepper_kinds, only: ip
      implicit none
      character(*) :: a  !! Arbitrary character strings.
      character(*) :: b  !! Arbitrary character strings.
      eqstr = pepper_eqstr ( a, b )
      end function eqstr

      !> Search for a given value within a character string array.
      !> Return the index of the first equivalent array entry, or zero
      !> if no equivalent element is found.
      !>
      !> Legacy interface: calls esrchc of module pepper_strings.
      integer(ip) function esrchc ( value, ndim, array )
      use pepper_strings, only: pepper_esrchc => esrchc
      use pepper_kinds, only: ip
      implicit none
      character(*) :: value  !! Key value to be found in ARRAY.
      integer(ip) :: ndim  !! Dimension of ARRAY.
      character(*) :: array ( * )  !! Character string array to search.
      esrchc = pepper_esrchc ( value, ndim, array )
      end function esrchc

      !> Find the beginning and end of the first word starting at
      !> or after a specified character.
      !>
      !> Legacy interface: calls fndnwd of module pepper_strings.
      subroutine fndnwd ( string, start, b, e )
      use pepper_strings, only: pepper_fndnwd => fndnwd
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! A string to examine for words.
      integer(ip) :: start  !! Position in the string to start looking for words.
      integer(ip) :: b  !! String position of first character of the word.
      integer(ip) :: e  !! String position of last character of the word.
      call pepper_fndnwd ( string, start, b, e )
      end subroutine fndnwd

      !> Return the index of the first non-blank character in
      !> a character string.
      !>
      !> Legacy interface: calls frstnb of module pepper_strings.
      integer(ip) function frstnb ( string )
      use pepper_strings, only: pepper_frstnb => frstnb
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! Input character string.
      integer(ip) :: ispace
      parameter           ( ispace = 32 )
      frstnb = pepper_frstnb ( string )
      end function frstnb

      !> Return the index of the first non-printable character in a
      !> character string. ASCII characters 32-126 are considered
      !> printable by this routine. (Blanks are considered printable.)
      !>
      !> Legacy interface: calls frstnp of module pepper_strings.
      integer(ip) function frstnp ( string )
      use pepper_strings, only: pepper_frstnp => frstnp
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! Input character string.
      frstnp = pepper_frstnp ( string )
      end function frstnp

      !> Return the index of the first printable character in a character
      !> string. ASCII characters 33-126 are printable. (Blanks are not
      !> considered printable.)
      !>
      !> Legacy interface: calls frstpc of module pepper_strings.
      integer(ip) function frstpc ( string )
      use pepper_strings, only: pepper_frstpc => frstpc
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! Input character string.
      frstpc = pepper_frstpc ( string )
      end function frstpc

      !> Locate a keyword in a string and extract the substring from
      !> the beginning of the first word following the keyword to the
      !> beginning of the first subsequent recognized terminator of a list.
      !>
      !> Legacy interface: calls kxtrct of module pepper_strings.
      subroutine kxtrct ( keywd,  terms, nterms, &
                          wordsq, found, substr  )
      use pepper_strings, only: pepper_kxtrct => kxtrct
      use pepper_kinds, only: ip
      implicit none
      character(*) :: keywd  !! Word that marks the beginning of text of interest.
      character(*) :: terms(*)  !! Set of words, any of which marks the end of text.
      integer(ip) :: nterms  !! Number of TERMS.
      character(*) :: wordsq  !! String containing a sequence of words.
      logical :: found  !! .TRUE. if the keyword is found in the string.
      character(*) :: substr  !! String from end of KEYWD to beginning of first TERMS item found.
      ! PEPPER: keywd is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_kxtrct ( (keywd), terms, nterms, wordsq, found, substr )
      end subroutine kxtrct

      !> Return the index of the last non-blank character in
      !> a character string.
      !>
      !> Legacy interface: calls lastnb of module pepper_strings.
      integer(ip) function  lastnb ( string )
      use pepper_strings, only: pepper_lastnb => lastnb
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! Input character string.
      integer(ip) :: ispace
      parameter           ( ispace = 32 )
      lastnb = pepper_lastnb ( string )
      end function lastnb

      !> Return the index of the last printable character in a character
      !> string. ASCII characters 33-126 are printable. (Blanks are not
      !> considered printable.)
      !>
      !> Legacy interface: calls lastpc of module pepper_strings.
      integer(ip) function  lastpc ( string )
      use pepper_strings, only: pepper_lastpc => lastpc
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! Input character string.
      lastpc = pepper_lastpc ( string )
      end function lastpc

      !> Build a list of items delimited by a character.
      !>
      !> Legacy interface: calls lbuild of module pepper_strings.
      subroutine lbuild ( items, n, delim, list )
      use pepper_strings, only: pepper_lbuild => lbuild
      use pepper_kinds, only: ip
      implicit none
      character(*) :: items ( * )  !! Items in the list.
      integer(ip) :: n  !! Number of items in the list.
      character(*) :: delim  !! String used to delimit items.
      character(*) :: list  !! List of items delimited by DELIM.
      ! PEPPER: delim is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_lbuild ( items, n, (delim), list )
      end subroutine lbuild

      !> Convert the characters in a string to lowercase.
      !>
      !> Legacy interface: calls lcase of module pepper_strings.
      subroutine lcase ( in, out )
      use pepper_strings, only: pepper_lcase => lcase
      use pepper_kinds, only: ip
      implicit none
      character(*) :: in  !! Input string.
      character(*) :: out  !! Output string, all lowercase.
      integer(ip), parameter :: uppera = ichar ('A')
      integer(ip), parameter :: upperz = ichar ('Z')
      integer(ip), parameter :: shift = ichar ('a') - uppera
      ! PEPPER: in is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_lcase ( (in), out )
      end subroutine lcase

      !> Left-justify, uppercase, and space-compress a character string.
      !>
      !> Legacy interface: calls ljucrs of module pepper_strings.
      subroutine ljucrs ( n, input, output )
      use pepper_strings, only: pepper_ljucrs => ljucrs
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: n  !! Maximum consecutive occurrences of space.
      character(*) :: input  !! Input string.
      character(*) :: output  !! Output string.
      integer(ip), parameter :: lowa = ichar ('a')
      integer(ip), parameter :: lowz = ichar ('z')
      integer(ip), parameter :: shift = ichar ('A') - lowa
      ! PEPPER: input is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_ljucrs ( n, (input), output )
      end subroutine ljucrs

      !> Left-justify a character string.
      !>
      !> Legacy interface: calls ljust of module pepper_strings.
      subroutine ljust ( input, output )
      use pepper_strings, only: pepper_ljust => ljust
      use pepper_kinds, only: ip
      implicit none
      character(*) :: input  !! Input character string.
      character(*) :: output  !! Output character string, left justified.
      ! PEPPER: input is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_ljust ( (input), output )
      end subroutine ljust

      !> Parse a list of items delimited by a single character.
      !>
      !> Legacy interface: calls lparse of module pepper_strings.
      subroutine lparse ( list, delim, nmax, n, items )
      use pepper_strings, only: pepper_lparse => lparse
      use pepper_kinds, only: ip
      implicit none
      character(*) :: list  !! List of items delimited by DELIM.
      character(1) :: delim  !! Single character used to delimit items.
      integer(ip) :: nmax  !! Maximum number of items to return.
      integer(ip) :: n  !! Number of items in the list.
      character(*) :: items ( * )  !! Items in the list, left justified.
      character(1) :: blank
      parameter           ( blank = ' ' )
      integer(ip) :: ispace
      parameter           ( ispace = 32 )
      ! PEPPER: list, delim are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_lparse ( (list), (delim), nmax, n, items )
      end subroutine lparse

      !> Parse a list of items separated by multiple delimiters.
      !>
      !> Legacy interface: calls lparsm of module pepper_strings.
      subroutine lparsm ( list, delims, nmax, n, items )
      use pepper_strings, only: pepper_lparsm => lparsm
      use pepper_kinds, only: ip
      implicit none
      character(*) :: list  !! List of items delimited by DELIMS.
      character(*) :: delims  !! Single characters which delimit items.
      integer(ip) :: nmax  !! Maximum number of items to return.
      integer(ip) :: n  !! Number of items in the list.
      character(*) :: items ( * )  !! Items in the list, left justified.
      character(1) :: blank
      parameter           ( blank = ' ' )
      integer(ip) :: ispace
      parameter           ( ispace = 32 )
      ! PEPPER: list, delims are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_lparsm ( (list), (delims), nmax, n, items )
      end subroutine lparsm

      !> Return the maximum of 1 and the location of the first non-blank
      !> character in the string.
      !>
      !> Legacy interface: calls ltrim of module pepper_strings.
      integer(ip) function ltrim ( string )
      use pepper_strings, only: pepper_ltrim => ltrim
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! String to be trimmed.
      ltrim = pepper_ltrim ( string )
      end function ltrim

      !> Scan (lex) a quoted string.
      !>
      !> Legacy interface: calls lxqstr of module pepper_strings.
      subroutine lxqstr ( string, qchar, first, last, nchar )
      use pepper_strings, only: pepper_lxqstr => lxqstr
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! String to be scanned.
      character(1) :: qchar  !! Quote delimiter character.
      integer(ip) :: first  !! Character position at which to start scanning.
      integer(ip) :: last  !! Character position of end of token.
      integer(ip) :: nchar  !! Number of characters in token.
      call pepper_lxqstr ( string, qchar, first, last, nchar )
      end subroutine lxqstr

      !> Determine whether a string is matched by a template containing
      !> wild cards.
      !>
      !> Legacy interface: calls matchw of module pepper_strings.
      logical function matchw ( string, templ, wstr, wchr )
      use pepper_strings, only: pepper_matchw => matchw
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! String to be tested.
      character(*) :: templ  !! Template (with wild cards) to test against STRING.
      character(1) :: wstr  !! Wild string token.
      character(1) :: wchr  !! Wild character token.
      matchw = pepper_matchw ( string, templ, wstr, wchr )
      end function matchw

      !> Return the non-blank length of a character string. (That is,
      !> the index of the last non-blank character when the string is
      !> left-justified.)
      !>
      !> Legacy interface: calls nblen of module pepper_strings.
      integer(ip) function  nblen ( string )
      use pepper_strings, only: pepper_nblen => nblen
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! Input character string.
      nblen = pepper_nblen ( string )
      end function nblen

      !> Determine the non-blank width of a character array---that is,
      !> the largest value of LASTNB for any element in the array.
      !>
      !> Legacy interface: calls nbwid of module pepper_strings.
      integer(ip) function nbwid ( array, nelt )
      use pepper_strings, only: pepper_nbwid => nbwid
      use pepper_kinds, only: ip
      implicit none
      character(*) :: array   ( * )  !! Input array.
      integer(ip) :: nelt  !! Number of elements in the array.
      nbwid = pepper_nbwid ( array, nelt )
      end function nbwid

      !> Find the first occurrence in a string of a character NOT belonging
      !> to a collection of characters, starting at a specified location,
      !> searching forwards.
      !>
      !> Legacy interface: calls ncpos of module pepper_strings.
      integer(ip) function ncpos ( str, chars, start )
      use pepper_strings, only: pepper_ncpos => ncpos
      use pepper_kinds, only: ip
      implicit none
      character(*) :: str  !! Any character string.
      character(*) :: chars  !! A collection of characters.
      integer(ip) :: start  !! Position to begin looking for one not in CHARS
      ncpos = pepper_ncpos ( str, chars, start )
      end function ncpos

      !> Find the first occurrence in a string of a character NOT
      !> belonging to a collection of characters, starting at a
      !> specified location, searching in reverse.
      !>
      !> Legacy interface: calls ncposr of module pepper_strings.
      integer(ip) function ncposr ( str, chars, start )
      use pepper_strings, only: pepper_ncposr => ncposr
      use pepper_kinds, only: ip
      implicit none
      character(*) :: str  !! Any character string.
      character(*) :: chars  !! A collection of characters.
      integer(ip) :: start  !! Position to begin looking for one not in CHARS
      ncposr = pepper_ncposr ( str, chars, start )
      end function ncposr

      !> Return the next word in a given character string, and
      !> left justify the rest of the string.
      !>
      !> Legacy interface: calls nextwd of module pepper_strings.
      subroutine nextwd ( string, next, rest )
      use pepper_strings, only: pepper_nextwd => nextwd
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! Input character string.
      character(*) :: next  !! The next word in the string.
      character(*) :: rest  !! The remaining part of STRING, left-justified.
      integer(ip) :: ispace
      parameter           ( ispace = 32 )
      ! PEPPER: string is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_nextwd ( (string), next, rest )
      end subroutine nextwd

      !> Return the Nth word in a character string, and its location
      !> in the string.
      !>
      !> Legacy interface: calls nthwd of module pepper_strings.
      subroutine nthwd ( string, nth, word, loc )
      use pepper_strings, only: pepper_nthwd => nthwd
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! Input character string.
      integer(ip) :: nth  !! Index of the word to be returned.
      character(*) :: word  !! The NTH word in STRING.
      integer(ip) :: loc  !! Location of WORD in STRING.
      ! PEPPER: string is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_nthwd ( (string), nth, word, loc )
      end subroutine nthwd

      !> Parse a quoted string token.
      !>
      !> Legacy interface: calls parsqs of module pepper_strings.
      subroutine parsqs ( string, qchar, value, length, error, errmsg, &
                          ptr                                          )
      use pepper_strings, only: pepper_parsqs => parsqs
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! Quoted string to be parsed.
      character(1) :: qchar  !! Quote delimiter character.
      character(*) :: value  !! Parsed string.
      integer(ip) :: length  !! Number of significant characters in VALUE.
      logical :: error  !! Logical error flag.
      character(*) :: errmsg  !! Message indicating whether errors have occurred.
      integer(ip) :: ptr  !! Position in string where an error occurred.
      ! PEPPER: string, qchar are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_parsqs ( (string), (qchar), value, length, error, errmsg, ptr )
      end subroutine parsqs

      !> Determine the printable width of a character array.
      !>
      !> Legacy interface: calls pcwid of module pepper_strings.
      integer(ip) function pcwid ( array, nelt )
      use pepper_strings, only: pepper_pcwid => pcwid
      use pepper_kinds, only: ip
      implicit none
      character(*) :: array   ( * )  !! Input array.
      integer(ip) :: nelt  !! Number of elements in the array.
      pcwid = pepper_pcwid ( array, nelt )
      end function pcwid

      !> Find the first occurrence in a string of a substring, starting at
      !> a specified location, searching forward.
      !>
      !> Legacy interface: calls pos of module pepper_strings.
      integer(ip) function pos ( str, substr, start )
      use pepper_strings, only: pepper_pos => pos
      use pepper_kinds, only: ip
      implicit none
      character(*) :: str  !! A character string
      character(*) :: substr  !! Substring to locate in the character string.
      integer(ip) :: start  !! Where to start looking for SUBSTR in STR.
      pos = pepper_pos ( str, substr, start )
      end function pos

      !> Find the first occurrence in a string of a substring, starting at
      !> a specified location, searching in reverse.
      !>
      !> Legacy interface: calls posr of module pepper_strings.
      integer(ip) function posr ( str, substr, start )
      use pepper_strings, only: pepper_posr => posr
      use pepper_kinds, only: ip
      implicit none
      character(*) :: str  !! A character string
      character(*) :: substr  !! Substring to locate in the character string.
      integer(ip) :: start  !! Where to start looking for SUBSTR in STR.
      posr = pepper_posr ( str, substr, start )
      end function posr

      !> Add a prefix to a character string.
      !>
      !> Legacy interface: calls prefix of module pepper_strings.
      subroutine prefix ( pref, spaces, string )
      use pepper_strings, only: pepper_prefix => prefix
      use pepper_kinds, only: ip
      implicit none
      character(*) :: pref  !! Prefix.
      integer(ip) :: spaces  !! Number of spaces separating prefix and suffix.
      character(*) :: string  !! Suffix on input, string on output.
      ! PEPPER: pref is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_prefix ( (pref), spaces, string )
      end subroutine prefix

      !> Enclose (quote) the non-blank part of a character string
      !> between delimiting symbols.
      !>
      !> Legacy interface: calls quote of module pepper_strings.
      subroutine quote ( in, left, right, out )
      use pepper_strings, only: pepper_quote => quote
      use pepper_kinds, only: ip
      implicit none
      character(*) :: in  !! Input string.
      character(1) :: left  !! Left delimiter.
      character(1) :: right  !! Right delimiter.
      character(*) :: out  !! Output (quoted) string.
      ! PEPPER: in, left, right are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_quote ( (in), (left), (right), out )
      end subroutine quote

      !> Replace all occurrences of a single character with a second
      !> character.
      !>
      !> Legacy interface: calls replch of module pepper_strings.
      subroutine replch ( instr, old, new, outstr )
      use pepper_strings, only: pepper_replch => replch
      use pepper_kinds, only: ip
      implicit none
      character(*) :: instr  !! Input string.
      character(1) :: old  !! Character to be replaced.
      character(1) :: new  !! Replacement character.
      character(*) :: outstr  !! Output string.
      ! PEPPER: instr, old, new are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_replch ( (instr), (old), (new), outstr )
      end subroutine replch

      !> Replace the Nth word in a string with a new word.
      !>
      !> Legacy interface: calls replwd of module pepper_strings.
      subroutine replwd ( instr, nth, new, outstr )
      use pepper_strings, only: pepper_replwd => replwd
      use pepper_kinds, only: ip
      implicit none
      character(*) :: instr  !! Input string.
      integer(ip) :: nth  !! Number of the word to be replaced.
      character(*) :: new  !! Replacement word.
      character(*) :: outstr  !! Output string.
      ! PEPPER: instr, new are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_replwd ( (instr), nth, (new), outstr )
      end subroutine replwd

      !> Replace a marker with a character string.
      !>
      !> Legacy interface: calls repmc of module pepper_strings.
      subroutine repmc ( in, marker, value, out )
      use pepper_strings, only: pepper_repmc => repmc
      use pepper_kinds, only: ip
      implicit none
      character(*) :: in  !! Input string.
      character(*) :: marker  !! Marker to be replaced.
      character(*) :: value  !! Replacement string.
      character(*) :: out  !! Output string.
      ! PEPPER: in, marker, value are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_repmc ( (in), (marker), (value), out )
      end subroutine repmc

      !> Right justify a character string.
      !>
      !> Legacy interface: calls rjust of module pepper_strings.
      subroutine rjust ( input, output )
      use pepper_strings, only: pepper_rjust => rjust
      use pepper_kinds, only: ip
      implicit none
      character(*) :: input  !! Input character string.
      character(*) :: output  !! Output character string, right justified.
      ! PEPPER: input is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_rjust ( (input), output )
      end subroutine rjust

      !> Return the maximum of 1 and the location of the last non-blank
      !> character in the string.
      !>
      !> Legacy interface: calls rtrim of module pepper_strings.
      integer(ip) function rtrim ( string )
      use pepper_strings, only: pepper_rtrim => rtrim
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! String to be trimmed.
      rtrim = pepper_rtrim ( string )
      end function rtrim

      !> Determine if two characters from different strings are the
      !> same.
      !>
      !> Legacy interface: calls samch of module pepper_strings.
      logical function samch ( str1, l1, str2, l2 )
      use pepper_strings, only: pepper_samch => samch
      use pepper_kinds, only: ip
      implicit none
      character(*) :: str1  !! A character string
      integer(ip) :: l1  !! The location (index) of a character in STR1
      character(*) :: str2  !! A character string
      integer(ip) :: l2  !! The location (index) of a character in STR2
      samch = pepper_samch ( str1, l1, str2, l2 )
      end function samch

      !> Determine whether or not two substrings are the same
      !>
      !> Legacy interface: calls samsub of module pepper_strings.
      logical function samsub ( str1, b1, e1,  str2, b2, e2 )
      use pepper_strings, only: pepper_samsub => samsub
      use pepper_kinds, only: ip
      implicit none
      character(*) :: str1  !! A string
      integer(ip) :: b1  !! Beginning of a substring in STR1
      integer(ip) :: e1  !! End of s substring in STR1
      character(*) :: str2  !! A second string
      integer(ip) :: b2  !! The beginning of a substring in STR2
      integer(ip) :: e2  !! The end  of s substring in STR2
      samsub = pepper_samsub ( str1, b1, e1, str2, b2, e2 )
      end function samsub

      !> Reject those tokens descriptors whose identities are among those
      !> of a specific collection.
      !>
      !> Legacy interface: calls scanrj of module pepper_strings.
      subroutine scanrj ( ids, n, ntokns, ident, beg, end )
      use pepper_strings, only: pepper_scanrj => scanrj
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: ids  ( * )  !! value of id's of tokens that should be dumped.
      integer(ip) :: n  !! number of id's.
      integer(ip) :: ntokns  !! number of tokens input. The number kept.
      integer(ip) :: ident  ( * )  !! identity of each of the tokens.
      integer(ip) :: beg    ( * )  !! indices of beginning of tokens.
      integer(ip) :: end    ( * )  !! indices of endings of tokens.
      call pepper_scanrj ( ids, n, ntokns, ident, beg, end )
      end subroutine scanrj

      !> Shift the contents of a character string to the left.
      !> Characters moved past the beginning of the input string are
      !> lost. Vacant spaces are filled with a specified character.
      !>
      !> Legacy interface: calls shiftl of module pepper_strings.
      subroutine shiftl ( in, nshift, fillc, out )
      use pepper_strings, only: pepper_shiftl => shiftl
      use pepper_kinds, only: ip
      implicit none
      character(*) :: in  !! Input string.
      integer(ip) :: nshift  !! Number of times to shift.
      character(1) :: fillc  !! Character to fill spaces left vacant.
      character(*) :: out  !! Shifted string.
      ! PEPPER: in, fillc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_shiftl ( (in), nshift, (fillc), out )
      end subroutine shiftl

      !> Shift the contents of a character string to the right.
      !> Characters moved past the end of the input string are
      !> lost. Vacant spaces are filled with a specified character.
      !>
      !> Legacy interface: calls shiftr of module pepper_strings.
      subroutine shiftr ( in, nshift, fillc, out )
      use pepper_strings, only: pepper_shiftr => shiftr
      use pepper_kinds, only: ip
      implicit none
      character(*) :: in  !! Input string.
      integer(ip) :: nshift  !! Number of times to shift.
      character(1) :: fillc  !! Character to fill spaces left vacant.
      character(*) :: out  !! Shifted string.
      ! PEPPER: in, fillc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_shiftr ( (in), nshift, (fillc), out )
      end subroutine shiftr

      !> Retain only the significant digits in a numeric string.
      !>
      !> Legacy interface: calls sigdgt of module pepper_strings.
      subroutine sigdgt ( in, out )
      use pepper_strings, only: pepper_sigdgt => sigdgt
      use pepper_kinds, only: ip
      implicit none
      character(*) :: in  !! Input numeric string.
      character(*) :: out  !! Numeric string, with insignificant digits removed.
      ! PEPPER: in is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_sigdgt ( (in), out )
      end subroutine sigdgt

      !> Add a suffix to a character string.
      !>
      !> Legacy interface: calls suffix of module pepper_strings.
      subroutine suffix ( suff, spaces, string )
      use pepper_strings, only: pepper_suffix => suffix
      use pepper_kinds, only: ip
      implicit none
      character(*) :: suff  !! Suffix.
      integer(ip) :: spaces  !! Number of spaces separating prefix and suffix.
      character(*) :: string  !! Prefix on input, string on output.
      ! PEPPER: suff is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_suffix ( (suff), spaces, string )
      end subroutine suffix

      !> Convert the characters in a string to uppercase.
      !>
      !> Legacy interface: calls ucase of module pepper_strings.
      subroutine ucase ( in, out )
      use pepper_strings, only: pepper_ucase => ucase
      use pepper_kinds, only: ip
      implicit none
      character(*) :: in  !! Input string.
      character(*) :: out  !! Output string, all uppercase.
      integer(ip), parameter :: lowa = ichar ('a')
      integer(ip), parameter :: lowz = ichar ('z')
      integer(ip), parameter :: shift = ichar ('A') - lowa
      ! PEPPER: in is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_ucase ( (in), out )
      end subroutine ucase

      !> Return the number of words in a string.
      !>
      !> Legacy interface: calls wdcnt of module pepper_strings.
      integer(ip) function wdcnt ( string )
      use pepper_strings, only: pepper_wdcnt => wdcnt
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! Input character string.
      wdcnt = pepper_wdcnt ( string )
      end function wdcnt

      !> Find the index of a word within a string. If the word does not
      !> exist as a word within the string, the value zero is returned.
      !>
      !> Legacy interface: calls wdindx of module pepper_strings.
      integer(ip) function wdindx ( string, word )
      use pepper_strings, only: pepper_wdindx => wdindx
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! String of characters, potentially containing words
      character(*) :: word  !! A string of consecutive printable letters.
      wdindx = pepper_wdindx ( string, word )
      end function wdindx

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zzinssub of module pepper_strings.
      subroutine zzinssub ( in, sub, loc, out )
      use pepper_strings, only: pepper_zzinssub => zzinssub
      use pepper_kinds, only: ip
      character(*) :: in  !! Input string.
      character(*) :: sub  !! Substring to be inserted.
      integer(ip) :: loc  !! Position at which substring is to be inserted.
      character(*) :: out  !! Output string.
      ! PEPPER: in, sub are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzinssub ( (in), (sub), loc, out )
      end subroutine zzinssub

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zzrepsub of module pepper_strings.
      subroutine zzrepsub ( in, left, right, string, out )
      use pepper_strings, only: pepper_zzrepsub => zzrepsub
      use pepper_kinds, only: ip
      implicit none
      character(*) :: in  !! Input string.
      integer(ip) :: left  !! Ends of substring to be replaced.
      integer(ip) :: right  !! Ends of substring to be replaced.
      character(*) :: string  !! Replacement string.
      character(*) :: out  !! Resulting string.
      ! PEPPER: in, string are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzrepsub ( (in), left, right, (string), out )
      end subroutine zzrepsub

      !> Convert an integer to an equivalent character string.
      !>
      !> Legacy interface: calls intstr of module pepper_strings.
      subroutine intstr ( number, string )
      use pepper_strings, only: pepper_intstr => intstr
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: number  !! Integer to be converted.
      character(*) :: string  !! Equivalent character string, left justified.
      integer(ip), parameter :: base = 10
      integer(ip), parameter :: strlen = 80
      character(1), parameter :: digits(0:9) = &
                 ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
      call pepper_intstr ( number, string )
      end subroutine intstr

      !> Take a double precision number and convert it to
      !> an equivalent character string representation (base 10).
      !>
      !> Legacy interface: calls dpstr of module pepper_strings.
      subroutine dpstr ( x, sigdig, string )
      use pepper_strings, only: pepper_dpstr => dpstr
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: x  !! A double precision number
      integer(ip) :: sigdig  !! The number of significant digits placed in output
      character(*) :: string  !! A character string representation of X
      integer(ip), parameter :: mostdg = 14
      integer(ip), parameter :: maxpow = 17
      real(wp), parameter :: power (0:maxpow) = &
         [ 1.0_wp, 1.0e1_wp, 1.0e2_wp, 1.0e3_wp, 1.0e4_wp, 1.0e5_wp, &
           1.0e6_wp, 1.0e7_wp, 1.0e8_wp, 1.0e9_wp, 1.0e10_wp, 1.0e11_wp, &
           1.0e12_wp, 1.0e13_wp, 1.0e14_wp, 1.0e15_wp, 1.0e16_wp, &
           1.0e17_wp ]
      real(wp), parameter :: ipower(0:maxpow) = &
         [ 1.0_wp, 1.0e-1_wp, 1.0e-2_wp, 1.0e-3_wp, 1.0e-4_wp, &
           1.0e-5_wp, 1.0e-6_wp, 1.0e-7_wp, 1.0e-8_wp, 1.0e-9_wp, &
           1.0e-10_wp, 1.0e-11_wp, 1.0e-12_wp, 1.0e-13_wp, 1.0e-14_wp, &
           1.0e-15_wp, 1.0e-16_wp, 1.0e-17_wp ]
      character(1), parameter :: digits(0:9) = &
         [ character(1) :: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]
      real(wp), parameter :: values(0:9) = &
         [ 0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, &
           7.0_wp, 8.0_wp, 9.0_wp ]
      integer(ip), parameter :: maxexp = 40
      character(2), parameter :: vaxexp(0:40) = &
         [ character(2) :: '00', '01', '02', '03', '04', '05', '06', '07', '08', '09', &
           '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', &
           '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', &
           '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', &
           '40' ]
      call pepper_dpstr ( x, sigdig, string )
      end subroutine dpstr

      !> Return .TRUE. if two given characters are equivalent when the
      !> case of the characters is ignored.
      !>
      !> Legacy interface: calls eqchr of module pepper_strings.
      logical function eqchr ( a, b )
      use pepper_strings, only: pepper_eqchr => eqchr
      use pepper_kinds, only: ip
      implicit none
      character(1) :: a  !! one of the characters to check
      character(1) :: b  !! the other character to check
      integer(ip) :: lstchr
      parameter           ( lstchr = 255 )
      eqchr = pepper_eqchr ( a, b )
      end function eqchr

      !> Return .TRUE. if two given characters are not equivalent if the
      !> case of the characters is ignored.
      !>
      !> Legacy interface: calls nechr of module pepper_strings.
      logical function nechr ( a, b )
      use pepper_strings, only: pepper_nechr => nechr
      use pepper_kinds, only: ip
      implicit none
      character(1) :: a  !! one of the characters to check
      character(1) :: b  !! the other character to check
      integer(ip) :: lstchr
      parameter           ( lstchr = 255 )
      nechr = pepper_nechr ( a, b )
      end function nechr

      !> Determine if two characters from different strings are the
      !> same when the case of the characters is ignored.
      !>
      !> Legacy interface: calls samchi of module pepper_strings.
      logical function samchi ( str1, l1, str2, l2 )
      use pepper_strings, only: pepper_samchi => samchi
      use pepper_kinds, only: ip
      implicit none
      character(*) :: str1  !! A character string
      integer(ip) :: l1  !! The location (index) of a character in STR1
      character(*) :: str2  !! A character string
      integer(ip) :: l2  !! The location (index) of a character in STR2
      samchi = pepper_samchi ( str1, l1, str2, l2 )
      end function samchi

      !> Determine whether or not two substrings are the same up to
      !> case.
      !>
      !> Legacy interface: calls samsbi of module pepper_strings.
      logical function samsbi ( str1, b1, e1,  str2, b2, e2 )
      use pepper_strings, only: pepper_samsbi => samsbi
      use pepper_kinds, only: ip
      implicit none
      character(*) :: str1  !! A string
      integer(ip) :: b1  !! Beginning of a substring in STR1
      integer(ip) :: e1  !! End of s substring in STR1
      character(*) :: str2  !! A second string
      integer(ip) :: b2  !! The beginning of a substring in STR2
      integer(ip) :: e2  !! The end  of s substring in STR2
      samsbi = pepper_samsbi ( str1, b1, e1, str2, b2, e2 )
      end function samsbi

      !> Determine whether a string is matched by a template containing
      !> wild cards. This routine is case-insensitive.
      !>
      !> Legacy interface: calls matchi of module pepper_strings.
      logical function  matchi ( string, templ, wstr, wchr )
      use pepper_strings, only: pepper_matchi => matchi
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! String to be tested.
      character(*) :: templ  !! Template (with wild cards) to test against STRING.
      character(1) :: wstr  !! Wild string token.
      character(1) :: wchr  !! Wild character token.
      matchi = pepper_matchi ( string, templ, wstr, wchr )
      end function matchi

      !> Parse a character string that represents a number and return
      !> a double precision value.
      !>
      !> Legacy interface: calls nparsd of module pepper_strings.
      subroutine nparsd ( string, x, error, ptr )
      use pepper_strings, only: pepper_nparsd => nparsd
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: string  !! Character string representing a numeric value.
      real(wp) :: x  !! Double precision value parsed from STRING.
      character(*) :: error  !! Message indicating whether errors have occurred.
      integer(ip) :: ptr  !! Position in string where an error occurred.
      ! PEPPER: string is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_nparsd ( (string), x, error, ptr )
      end subroutine nparsd

      !> Parse a character string that represents a number and return
      !> the FORTRAN-truncated integer value.
      !>
      !> Legacy interface: calls nparsi of module pepper_strings.
      subroutine nparsi (  string,  n, error, pnter)
      use pepper_strings, only: pepper_nparsi => nparsi
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax, intmin
      implicit none
      character(*) :: string  !! Character string representing a numeric value.
      integer(ip) :: n  !! Translated integer value of STRING.
      character(*) :: error  !! Message indicating what errors have occurred.
      integer(ip) :: pnter  !! Position in character string where an error occurred.
      ! PEPPER: string is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_nparsi ( (string), n, error, pnter )
      end subroutine nparsi

      !> Scan a string from a specified starting position for the
      !> end of an unsigned integer.
      !>
      !> Legacy interface: calls lx4uns of module pepper_strings.
      subroutine lx4uns ( string, first, last, nchar )
      use pepper_strings, only: pepper_lx4uns => lx4uns
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! any character string
      integer(ip) :: first  !! first character to scan from in STRING
      integer(ip) :: last  !! last character that is part of an unsigned integer
      integer(ip) :: nchar  !! number of characters in the unsigned integer.
      call pepper_lx4uns ( string, first, last, nchar )
      end subroutine lx4uns

end module pepper_legacy_strings
