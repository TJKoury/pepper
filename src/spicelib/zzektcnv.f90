!$Procedure  ZZEKTCNV ( Private: EK, time conversion )

      subroutine zzektcnv ( timstr, et, error, errmsg )
      use pepper_kinds, only: ip, wp
      implicit none

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Convert time strings from EK query to ephemeris time.
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

      include 'ektype.inc'

      character(*)          timstr
      real(wp)              et
      logical               error
      character(*)          errmsg

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TIMSTR     I   Time string.
!     ET         O   Ephemeris time in seconds past J2000, TDB.
!     ERROR      O   Error flag.
!     ERRMSG     O   Error message.
!
! $ Detailed_Input
!
!     TIMSTR         is a string representing a time value.  The value
!                    make be an SCLK string in the form
!
!                       <clock name> SCLK <clock string>
!
!                    or may be any string acceptable to ST2ET.
!
! $ Detailed_Output
!
!     ET             is the ephemeris time equivalent to the input
!                    time.
!
!     ERROR          is a logical flag indicating whether an error was
!                    detected.  Note that a time string might be
!                    syntactically valid, but incapable of being
!                    converted to ET if the appropriate time kernels
!                    (Leapseconds or SCLK) are not loaded.
!
!     ERRMSG         is an error message describing an error in the
!                    input query, if one was detected.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If any sort of time conversion error occurs, the output flag
!         ERROR is set, and an error message is returned.
!
!         The routine attempts to avoid causing errors that must
!         be trapped by SPICELIB error handling.  Time string syntax
!         errors or missing kernel files, for example, should not trip
!         SPICELIB error handling.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Strings representing time values are interpreted as follows:
!
!        1)  The string is first examined to see whether it's an
!            SCLK string for a recognized clock; if it is, the
!            string is converted to the equivalent ET.
!
!        2)  If the string is not a SCLK string, it is expected
!            to be some sort of standard time representation.
!            The string is checked to see whether it's in a format
!            that TPARTV can handle.  If TPARTV can't deal with it,
!            the string is considered to be invalid.
!
! $ Examples
!
!     See ZZEKTRES.
!
! $ Restrictions
!
!     1) A leapseconds kernel must be loaded at the time this routine
!        is called.
!
!     2) In order to convert SCLK strings, an appropriate SCLK kernel
!        must be loaded at the time this routine is called.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 12-AUG-2001 (NJB)
!
!        Now converts standard time strings to ET via STR2ET instead
!        of the less general routines ISO2UTC and UTC2ET.
!
!        Bug fix:  modified algorithm to handle case where string
!        "SCLK" appears in SCLK name.
!
!        Bug fix:  construction of error messages in case where
!        FAILED() returns .TRUE. has been changed so that REPMC is
!        not called.  Instead, the error-free routine SUFFIX is
!        used.
!
!-    SPICELIB Version 1.0.0, 11-OCT-1995 (NJB)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 12-AUG-2001 (NJB)
!
!        Now converts standard time strings to ET via STR2ET instead
!        of the less general routines ISO2UTC and UTC2ET.
!
!        Bug fix:  modified algorithm to handle case where string
!        "SCLK" appears in SCLK name.
!
!        Bug fix:  construction of error messages in case where
!        FAILED() returns .TRUE. has been changed so that REPMC is
!        not called.  Instead, the error-free routine SUFFIX is
!        used.
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           posr
      integer(ip)           rtrim

      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip)           lnsize
      parameter           ( lnsize = 80 )

      integer(ip)           msglen
      parameter           ( msglen = 160 )

      integer(ip)           ncomp
      parameter           ( ncomp  = 10 )

      integer(ip)           maxmod
      parameter           ( maxmod = 10 )

      integer(ip)           short
      parameter           ( short  = 32 )

!
!     Local variables
!
      character(lnsize)     locstr
      character(short)      modify ( maxmod )
      character(lnsize)     pictur
      character(msglen)     sclmsg
      character(short)      type

      real(wp)              sclkdp
      real(wp)              tvec   ( ncomp )

      integer(ip)           clkid
      integer(ip)           loc
      integer(ip)           ntvec

      logical               fnd
      logical               mods
      logical               succes
      logical               yabbrv


      if ( return() ) then
         return
      end if

      call chkin ( 'ZZEKTCNV' )

!
!     No error to start with.
!
      error  =  .false.
      errmsg =  ' '

!
!     Get a left-justified, compressed, upper-case copy of
!     the string, so we can easily search it for substrings
!     that would identify it as SCLK.  If we do find a
!     match, remove the identifying substring (of the form
!     'MO SCLK', 'VGR1 SCLK', etc.).
!
      call cmprss ( ' ',   1,  timstr,  locstr )
      call ljust  ( locstr,             locstr )
      call ucase  ( locstr,             locstr )

      loc  =  posr ( locstr, 'SCLK', rtrim(locstr) )

      if ( loc > 0 ) then
!
!        It's a SCLK string.  Find the ID code, if we can.
!
         call scn2id ( locstr(:loc+3), clkid, fnd )


         if ( .not. fnd ) then
!
!           We don't recognize this SCLK type.
!
            error  = .true.

            if ( loc > 1 ) then

               errmsg = 'Time conversion failed; SCLK type '// &
                        '<#> was not recognized.'

               call repmc ( errmsg, '#', timstr(:loc-1),  errmsg )

            else

               errmsg = 'Time conversion failed; SCLK name was not ' // &
                        'supplied.'
            end if

            call chkout ( 'ZZEKTCNV' )
            return

         end if

!
!        If we got this far, we recognized the SCLK type.
!        Convert the time to ET.
!
         call scpars ( clkid, locstr(loc+4:), error, sclmsg, sclkdp )

         if ( failed() ) then
!
!           We'll arrive here if the required SCLK kernel hasn't
!           been loaded.
!
            error  =  .true.
            errmsg =  'Unexpected SPICELIB error encountered '  // &
                      'while attempting to parse the string <'
            call suffix ( timstr, 0, errmsg )
            call suffix ( '>',    0, errmsg )
            call chkout ( 'ZZEKTCNV'        )
            return

         else if ( error ) then

            errmsg =  'The string <#> didn''t parse '                 // &
                      'as a spacecraft clock string.'
            call repmc  ( errmsg, '#', timstr, errmsg )
            call suffix ( sclmsg,  3,          errmsg )
            call chkout ( 'ZZEKTCNV'                  )
            return

         else

            call sct2e ( clkid, sclkdp, et )

            if ( failed() ) then

               error  =  .true.
               errmsg =  'Unexpected SPICELIB error encountered '  // &
                         'while attempting to parse the string <'
               call suffix ( timstr, 0, errmsg )
               call suffix ( '>',    0, errmsg )
               call chkout ( 'ZZEKTCNV'        )
               return

            end if

         end if



      else
!
!        We could have a standard time string.  Make sure that the
!        time string is acceptable before actually calling STR2ET.
!
         call tpartv ( locstr, &
                       tvec,   ntvec, type, &
                       modify, mods,  yabbrv, succes, &
                       pictur, errmsg                 )

         if ( succes ) then
!
!           It's safe to pass the string to STR2ET.
!
            call str2et ( locstr, et )

            if ( failed() ) then

               error  =  .true.
               errmsg =  'Unexpected SPICELIB error encountered '  // &
                         'while attempting to parse the string <'
               call suffix ( timstr, 0, errmsg )
               call suffix ( '>',    0, errmsg )
               call chkout ( 'ZZEKTCNV'        )
               return

            end if

         else
!
!           The string cannot be parsed by STR2ET.  The error message
!           was set by TPARTV.
!
            error  =  .true.
            call chkout ( 'ZZEKTCNV' )
            return

         end if
!
!        We're done with the standard time string case.
!
      end if
!
!     We've parsed a time string, if it was an SCLK or standard string.
!
      call chkout ( 'ZZEKTCNV' )
      return
      end
