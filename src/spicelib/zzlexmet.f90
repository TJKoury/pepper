!$Procedure ZZLEXMET ( Scan method string )

      subroutine zzlexmet ( method, maxn, n, begs, ends )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Scan a method input string passed by a caller to a SPICE
!     high-level geometry API. Return tokens found in the string.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     SCANNING
!     PARSING
!     CONSTANTS
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations

      implicit none

      character(*)          method
      integer(ip)           maxn
      integer(ip)           n
      integer(ip)           begs   ( * )
      integer(ip)           ends   ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     METHOD     I   Method string.
!     MAXN       I   Maximum number of tokens that can be returned.
!     N          O   Number of tokens found.
!     BEGS       O   Beginning indices of tokens.
!     ENDS       O   Ending indices of tokens.
!
! $ Detailed_Input
!
!     METHOD     is a "method" string to be parsed. This string
!                normally is passed by a caller to a SPICE high-level
!                high-level geometry routine.
!
!                METHOD indicates whether a target body surface is
!                to be modeled as an ellipsoid or by DSK data. It
!                may contain additional, computation-dependent
!                parameters.
!
!     MAXN       is the maximum number of tokens that may be returned.
!                The output arrays BEGS and ENDS must be declared with
!                size at least MAXN.  It's an error to supply output
!                arrays that are too small to hold all of the tokens in
!                the input method string.
!
! $ Detailed_Output
!
!     N          is the number of tokens found in the method string.
!
!     BEGS,
!     ENDS       are, respectively, the indices of the first and last
!                characters of each token in the input string METHOD.
!                BEGS(I) and ENDS(I) indicate the index range of the
!                Ith token.
!
!                See the Particulars section for a list of token
!                delimiters.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error is found while parsing a quoted string,
!         the error SPICE(SYNTAXERROR) is signaled.
!
!     2)  If the number of tokens found in the method string
!         is greater than the input limit MAXN, the error
!         SPICE(ARRAYTOOSMALL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports parsing of the METHOD input argument
!     used by SPICE geometry APIs such as, but not limited to:
!
!        GFOCLT
!        ILLUMF
!        ILLUMG
!        ILUMIN
!        LATSRF
!        LIMBPT
!        SINCPT
!        SUBPNT
!        SUBSLR
!        TERMPT
!
!     Token delimiters are
!
!        / = , <blank>
!
!     Strings delimited by double quote characters are treated as
!     individual tokens. There is no escape syntax for indicating a
!     doubly quoted string within a doubly quoted string.
!
! $ Examples
!
!
!     Example:
!
!       Method string =
!
!          METHOD = 'INTERCEPT / '
!          //       'DSK/UNPRIORITIZED/SURFACES = "MGS '
!          //       'MOLA 128 PIXEL/DEG", MARS_LOWRES'
!
!       Token list =
!
!          INTERCEPT
!          /
!          DSK
!          /
!          UNPRIORITIZED
!          /
!          SURFACES
!          =
!          "MGS MOLA 128 PIXEL/DEG"
!          ,
!          MARS_LOWRES
!
!
! $ Restrictions
!
!     This is a specialized scanning routine. It it meant to
!     be used only for the parsing "method" strings as
!     described in Particulars.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 02-FEB-2016 (NJB)
!
!        Based on first version 13-JAN-2015 (NJB)
!
!-&

! $ Index_Entries
!
!     scan method string for geometry api routines
!     lex method string for geometry api routines
!     extract tokens from method string for geometry apis
!-&


!
!     SPICELIB functions
!
      integer(ip)           cpos
      integer(ip)           ltrim
      integer(ip)           rtrim

      logical               return

!
!     Local parameters
!
!
!     Note the leading blank in DELIM below; this
!     indicates that the blank character is a delimiter.
!
      character(*)          delim
      parameter           ( delim  = ' /,=:' )

      character(*)          dquote
      parameter           ( dquote  = '"' )

!
!     Local variables
!
      integer(ip)           b
      integer(ip)           e
      integer(ip)           eq
      integer(ip)           i
      integer(ip)           loc
      integer(ip)           nchar
      integer(ip)           qpos
      integer(ip)           r
      integer(ip)           room
      integer(ip)           tke

      if ( return() ) return



      if ( method == ' ' ) then
!
!        No tokens here.
!
         n = 0

         return

      end if


      n    = 0
      room = maxn
      r    = rtrim( method )
      i    = 1

      do while ( i <= r )
!
!        Look ahead in the input string for the start of a
!        quoted string.
!
         qpos = cpos ( method(i:), dquote, 1 )
         b    = i

         if ( qpos == 0 ) then
!
!           There are no quoted string tokens in the input string
!           from index I onward.
!
            e = r
         else
            e = i + qpos - 2
         end if


         if ( b <= e ) then
!
!           Locate any tokens between indices B and E.
!
            i = b

            do while ( i <= e )
!
!              Find the next delimiter in the substring
!              from indices I : E.
!
               loc = cpos( method(i:e), delim, 1 )

               if ( loc == 1 ) then
!
!                 There is a delimiter character at index I in METHOD.
!
                  tke  = i - 1 + loc

               else if ( loc > 1 ) then
!
!                 There is a delimiter character at index
!
!                    I - 1 + LOC
!
!                 in METHOD.
!
                  if ( method(i:i+loc-2) /= ' ' ) then
!
!                    There's a token preceding the delimiter.
!
                     tke  = i - 2 + loc

                  else
!
!                    The delimiter is all we've got.
!
                     tke = i - 1 + loc

                  end if

               else
!
!                 The token, if any, ends at the end of
!                 substring we're considering.
!
                  tke  = e

               end if

!
!              There is a token, which may be blank, between
!              indices I and TKE. We don't return blank tokens
!              in the output array.
!
               if ( method( i : tke ) /= ' ' ) then

                  if ( room > 0 ) then

                     n       = n    + 1
                     room    = room - 1
                     begs(n) = ltrim( method(i : tke) ) + i - 1
                     ends(n) = rtrim( method(i : tke) ) + i - 1

                  else

                     call setmsg ( 'Need more room in output ' &
                     //            'arrays. Token count = #; ' &
                     //            'substring indices = #:#; ' &
                     //            'substring = #.'           )
                     call errint ( '#', n                     )
                     call errint ( '#', i                     )
                     call errint ( '#', tke                   )
                     call sigerr ( 'SPICE(ARRAYTOOSMALL)'     )
                     return

                  end if

               end if

               i = tke + 1

            end do

         end if


         if ( e < r ) then
!
!           We expect to find at least one quoted string starting
!           at index E + 1.
!
            i  = e + 1

            call lxqstr ( method(i:), dquote, 1, eq, nchar )

            if ( nchar > 0 ) then

               if ( room > 0 ) then

                  n       = n    + 1
                  room    = room - 1
                  begs(n) = i
                  ends(n) = i    - 1 + eq

               else

                  call setmsg ( 'Need more room in output ' &
                  //            'arrays. Token count = #; ' &
                  //            'substring indices = #:#; ' &
                  //            'substring = #.'           )
                  call errint ( '#', n                     )
                  call errint ( '#', i                     )
                  call errint ( '#', tke                   )
                  call errch  ( '#', method(i:tke)         )
                  call sigerr ( 'SPICE(ARRAYTOOSMALL)'     )
                  return

               end if

            else
!
!              We have a syntax error in the input string.
!
               call setmsg ( 'Invalid quoted string found ' &
               //            'starting at index #. Substring ' &
               //            'is #.'                          )
               call errint ( '#', i                           )
               call errch  ( '#', method(i:)                  )
               call sigerr ( 'SPICE(SYNTAXERROR)'             )
               return

            end if

            i = ends(n) + 1

         end if
!
!        The index I has been moved forward by at least one position.
!
      end do

      return
      end
