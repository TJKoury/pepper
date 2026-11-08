!$Procedure ISO2UTC ( Convert ISO time strings to UTC strings. )

      subroutine iso2utc ( tstrng, utcstr, error )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Deprecated: This routine is deprecated because all high-level
!     time conversion routines (STR2ET, UTC2ET, TPARSE) were updated
!     to accept ISO formatted times on input.  This routine is
!     supported for purposes of backward compatibility only.
!
!     Convert date-time strings represented in the format adopted by the
!     International Standards Organization (ISO) to equivalent UTC time
!     strings recognized by the SPICELIB routine TPARSE.
!
! $ Required_Reading
!
!     TIME
!
! $ Keywords
!
!     TIME
!
! $ Declarations

      implicit none

      character(*)          tstrng
      character(*)          utcstr
      character(*)          error

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TSTRNG     I   String representing a calendar or julian date epoch
!     UTCSTR     O   SPICELIB UTC string corresponding to TSTRNG
!     ERROR      O   Error message if something went wrong.
!
! $ Detailed_Input
!
!     TSTRNG   is an input time string, containing a time string
!              in ISO format. This routine is not sensitive to
!              the case of the characters that make up TSTRNG.
!              Thus 1992-192t12:29:28 and 1992-192T12:29:28
!              are equivalent.
!
!              The ISO standard time formats are:
!
!                 Year Month Day    yyyy-mm-ddThh:mm:ss[.sss...]
!                                   yyyy-mm-dd
!
!                 Day of Year       yyyy-dddThh:mm:ss[.sss...]
!                                   yyyy-ddd
!
!              The letters y,m,d,h,m,s can stand for any digit.
!              All digits are required in these formats. Moreover
!              the year portion of these strings must be between
!              1000 and 2999 inclusive.
!
!              The length of TSTRNG should not exceed 80 characters.
!
!              We point out that the format yyyy-ddd may be
!              interpreted very differently by routine UTC2ET.
!              1992-003 is interpreted by UTC2ET as March 1, 1992
!              whereas it is interpret as January 3, 1992 by ISO2ET.
!
!              User's should be aware of these differences in
!              interpretation and exercise adequate care in their
!              programs to avoid this possible confusion.
!
! $ Detailed_Output
!
!     UTCSTR   is the equivalent of TSTRNG, expressed in a UTC
!              time string that can be parsed by the SPICELIB
!              routine TPARSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the string is interpreted as an ISO format string and
!         the year portion is not within the range [1000, 2999], the
!         error SPICE(YEAROUTOFBOUNDS) is signaled. UTCSTR is
!         not changed.
!
!     2)  If the string does not clearly match the ISO format,
!         the error SPICE(NOTISOFORMAT) is signaled. UTCSTR is not
!         changed.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The input string is converted to a UTC time string as defined
!     by the SPICELIB routine TPARSE.
!
! $ Examples
!
!     To convert the time string 1992-04-03T14:12:28 to the
!     corresponding ephemeris time, execute the following instructions:
!
!        TSTRNG = '1992-04-03T14:12:28'
!
!        CALL ISO2UTC ( TSTRNG, UTCSTR, ERROR )
!
!        CALL TPARSE ( UTCSTR, UTCSEC, ERROR )
!
!        CALL DELTET ( UTCSEC, 'UTC', DELTA )
!
!        ET = DELTA + UTCSEC
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  J. Jespersen and J. Fitz-Randolph, "From Sundials to Atomic
!          Clocks, Understanding Time and Frequency," Dover
!          Publications, Inc. New York, 1877.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     J.M. Lynch         (JPL)
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 23-DEC-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Remove obsolete
!        reference to NAIF document 268 from $Literature_References.
!
!-    SPICELIB Version 1.1.2, 28-FEB-2008 (BVS)
!
!        Corrected the contents of the $Required_Reading section.
!
!-    SPICELIB Version 1.1.1, 19-SEP-2006 (EDW)
!
!        Added text to previously empty $Restrictions section.
!
!-    SPICELIB Version 1.0.0, 11-JUL-1995 (KRG) (JML)
!
!        Based on
!
!        EKLIB Version 1.1.0, 11-JUL-1995 (KRG)
!
!           Fixed a typo in the $Detailed_Output section of the header.
!           The output variable was listed as ET when it should have
!           been UTCSTR.
!
!           Changed the length of ASCII to be 100 rather than 128. This
!           removes possible compiler warning messages for truncating
!           character variables on assignments. The maximum nonblank
!           length for an input time ISO string is 80 characters, so
!           placing it into a temporary array of 100 characters should
!           pose no difficulties.
!
!        EKLIB Version 1.0.0, 25-FEB-1993 (JML)
!
!-&


! $ Index_Entries
!
!     DEPRECATED Transform ISO time strings to UTC strings
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           bsrchc
      integer(ip)           rtrim

      logical               return

!
!     Local Parameters
!
      integer(ip)           nmnths
      parameter           ( nmnths = 12 )

!
!     In-line functions.
!
      logical               digit
      logical               colon
      logical               hyphen
      logical               t

!
!     Local Variables
!
      character(100)        ascii
      character(128)        mystr

      character(3)          months ( 0 : nmnths )
      save                  months

      character(2)          imonth (     nmnths )
      save                  imonth

      integer(ip)           i
      integer(ip)           l
      integer(ip)           m

      logical               change
!
!     Initial Values
!

      data                  months  / '???', 'JAN', 'FEB', 'MAR', &
                                             'APR', 'MAY', 'JUN', &
                                             'JUL', 'AUG', 'SEP', &
                                             'OCT', 'NOV', 'DEC' /

      data                  imonth  /        '01',  '02',  '03', &
                                             '04',  '05',  '06', &
                                             '07',  '08',  '09', &
                                             '10',  '11',  '12'  /



!
!     In-line Function Definitions
!
      digit(i)  =      ichar(ascii(i:i)) >= ichar('0') &
                 .and. ichar(ascii(i:i)) <= ichar('9')

      hyphen(i) = ascii(i:i) == '-'
      colon(i)  = ascii(i:i) == ':'
      t(i)      = ascii(i:i) == 'T' .or. ascii(i:i) == 't'


!
!     Standard SPICELIB exception handling
!
      if ( return() ) return

!
!     Left justify the input time string, and determine the location of
!     it's last non-blank character.  Finally make some local copies.
!
      call      ljust ( tstrng, ascii )
      l      =  rtrim (         ascii )
      mystr  =  ascii
      change = .false.

!
!     Next check for one of the ISO allowed formats.
!
      if      ( l == 8 ) then

!
!        The possible format is: yyyy-ddd.  If we get a
!        match construct the corresponding SPICE day of
!        year format using JAN  (e.g. 1991-JAN-261).
!
         if (       digit  (1) &
              .and. digit  (2) &
              .and. digit  (3) &
              .and. digit  (4) &
              .and. hyphen (5) &
              .and. digit  (6) &
              .and. digit  (7) &
              .and. digit  (8) ) then

            mystr  =  ascii(1:5) // 'JAN' // ascii(5:)
            change = .true.

         end if

      else if ( l == 10 ) then

!
!        The possible format is: yyyy-mm-dd. If we get a match
!        construct the corresponding SPICE yyyy-mm-dd format.
!
         if (       digit  (1) &
              .and. digit  (2) &
              .and. digit  (3) &
              .and. digit  (4) &
              .and. hyphen (5) &
              .and. digit  (6) &
              .and. digit  (7) &
              .and. hyphen (8) &
              .and. digit  (9) &
              .and. digit  (10)  ) then

            m      =  bsrchc ( ascii(6:7), nmnths, imonth )
            mystr  =  ascii(1:5) // months(m) // ascii(8:)
            change = .true.

         end if

      else if ( l >= 17 ) then

!
!        There are two possible formats yyyy-dddThh:mm:ss.ssssss
!                                       yyyy-mm-ddThh:mm:ss.ssssss
!        As above, if we get a match up to the first character following
!        a 'T', convert this to a standard SPICE time string.
!
         if (       digit  (1)  .and. digit  (2) &
              .and. digit  (3)  .and. digit  (4) &
              .and. hyphen (5) &
              .and. digit  (6)  .and. digit  (7)  .and. digit  (8) &
              .and. t      (9) &
              .and. digit  (10) .and. digit  (11) &
              .and. colon  (12) &
              .and. digit  (13) .and. digit  (14) &
              .and. colon  (15) &
              .and. digit  (16) .and. digit  (17) ) then


            mystr  =  ascii(1:5) // &
                     'JAN'       // &
                      ascii(5:8) // &
                     ' '         // &
                      ascii(10:)

            change = .true.

         else if (       digit  (1)  .and. digit  (2) &
                   .and. digit  (3)  .and. digit  (4) &
                   .and. hyphen (5) &
                   .and. digit  (6)  .and. digit  (7) &
                   .and. hyphen (8) &
                   .and. digit  (9)  .and. digit  (10) &
                   .and. t      (11) &
                   .and. digit  (12) .and. digit  (13) &
                   .and. colon  (14) &
                   .and. digit  (15) .and. digit  (16) &
                   .and. colon  (17) &
                   .and. digit  (18) .and. digit  (19) ) then

            m      =  bsrchc ( ascii(6:7), nmnths, imonth )
            mystr  =  ascii(1:5)  // &
                      months(m)   // &
                      ascii(8:10) // &
                      ' '         // &
                      ascii(12:)

            change = .true.
         end if

      end if


!
!     If we didn't make some change to the input string, it's NOT
!     an ISO format string. Say so in an error message and return.
!
      if ( .not. change ) then

         error = 'The input string does not match the format '  // &
                 'expected of ISO time strings. The acceptable '// &
                 'formats are: yyyy-ddd, yyyy-mm-dd, '          // &
                 'yyyy-dddThh:mm:ss[.ss...], and '              // &
                 'yyyy-mm-ddThh:mm:ss[.ss...].  The input '     // &
                 'string was #. '

         call repmc  ( error, '#', mystr(1:l), error )
         return

      end if

!
!     Check for a year out of the range from 1000 to 2999
!
      if ( change &
           .and. (      llt ( ascii(1:4), '1000' ) &
                   .or. lgt ( ascii(1:4), '2999' ) ) ) then

         error = 'Years outside the range from 1000 '           // &
                 'to 2999 are not supported in SPICE-ISO '      // &
                 'format. You''ve '                             // &
                 'supplied a time string of the form '          // &
                 '# ... '
         call repmc ( error, '#', ascii(1:7), error )
         return

      end if


!
!     That's it.
!
      error = ' '

      utcstr = mystr


      return

      end
