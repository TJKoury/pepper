!$Procedure      ZZUTCPM ( UTC Plus or Minus Parse )

      subroutine zzutcpm ( string, start, hoff, moff, last, succes )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Parse a substring of the form ::UTC[+/-]1-12:0-59
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      Time --- PRIVATE
!
! $ Declarations

      implicit none
      character(*)          string
      integer(ip)           start
      real(wp)              hoff
      real(wp)              moff
      integer(ip)           last
      logical               succes

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   is a string containing a substring ::UTC+HR:MN
!     START      I   is the location in the string to start parsing
!     HOFF       O   is the d.p. value associated with HR.
!     MOFF       O   is the d.p. value associated with MN
!     LAST       O   is the end of the time zone substring.
!     SUCCES     O   indicates that a time zone was parsed.
!
! $ Detailed_Input
!
!     STRING     is a string that has an embedded substring of the
!                form ::UTC+HR[:MN] ( or ::UTC-HR[:MN] starting at
!                character start.
!
!     START      is the location in STRING where a time zone
!                specification is believed to begin.
!
! $ Detailed_Output
!
!     HOFF       is the double precision value associated with
!                HR in the picture above.  This value will be
!                between -12 and 12 inclusive.
!
!     MOFF       is the double precision value associated with MN
!                in the picture above.  This value will be between
!                0 and 59 inclusive (or -59 and 0 inclusive) depending
!                on the sign present in the UTC+/- substring.  The
!                sign of MOFF is the same as the sign present in the
!                string.
!
!     LAST       is the last character of the time zone specification.
!                If the string doesn't have a correct format and
!                range of values, LAST is returns as START - 1.
!
!     SUCCES     is a logical which if true, indicates that a time
!                zone was successfully parsed.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) There are no exceptions.  Either the string matches
!        the template or it doesn't.  No case is regarded
!        as an error.
!
! $ Particulars
!
!     This is a private routine for parsing time zones specified
!     as UTC+/-HR:MN  where HR is an unsigned integer between 0 and
!     11.  HR must have no more than 2 digits.  MN is expected
!     to be an unsigned integer between 0 and 59 inclusive.  It must
!     have no more than 2 digits.
!
! $ Examples
!
!     See TIMOUT.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 27-SEP-1996 (WLT)
!
!
!-&
!
!     Spicelib functions
!
      logical               samch
!
!     Local Variables
!
      integer(ip)           lnsize
      parameter           ( lnsize = 80 )

      character(lnsize)     error

      integer(ip)           need
      integer(ip)           length
      integer(ip)           signat
      integer(ip)           unsat
      integer(ip)           unsto
      integer(ip)           nchar
      integer(ip)           ptr

      real(wp)              sign
      real(wp)              x

!
!     This is a special purpose routine.  The input string must have
!     exactly the right format to be a time zone substring.  If anything
!     goes wrong, we just bail out and leave HOFF and MOFF right at
!     zero.
!
      hoff   =  0.0_wp
      moff   =  0.0_wp
      last   =  start - 1
      succes = .false.

!
!     Note that NEED   = START + LEN('::UTC+x') - 1
!               SIGNAT = START + LEN('::UTC+' ) - 1
!
      length = len(string)
      need   = start + 6
      signat = start + 5
      unsat  = need

      if ( length < need ) then
         return
      end if

      if       ( ichar(string(signat:signat)) == ichar('+') ) then
         sign =  1.0_wp
      else if  ( ichar(string(signat:signat)) == ichar('-') ) then
         sign = -1.0_wp
      else
         return
      end if
!
!     So far everything looks fine, "lex" the string starting at
!     SIGNAT + 1 for an unsigned integer.
!
      call lx4uns ( string, unsat, unsto, nchar )

      if ( nchar > 0 .and. nchar < 3 ) then
         call nparsd ( string(unsat:unsto), x, error, ptr )

         if ( x >= 13.0_wp ) then
            return
         end if

         last = unsto
         hoff = sign * x

      else
         return
      end if
!
!     If we're still in the game at this point, we have at least
!     an hour offset, see if there is a minutes portion to the
!     time zone.
!
      succes = .true.

      if ( samch( string, unsto+1, ':', 1 ) ) then
         unsat = unsto + 2
      else
         return
      end if

      call lx4uns ( string, unsat, unsto, nchar )

      if ( nchar > 0 .and. nchar < 3 ) then
         call nparsd ( string(unsat:unsto), x, error, ptr )

         if ( x > 59.0_wp ) then
            return
         end if

         last = unsto
         moff = sign*x

      end if

      return
      end
