!$Procedure INTTXT ( Convert an integer to text )

      subroutine inttxt ( n, string )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Convert an integer to an equivalent written phrase.
!     For example, convert 121 to 'ONE HUNDRED TWENTY-ONE'.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONVERSION
!     PARSING
!     STRING
!     UNITS
!     UTILITY
!
! $ Declarations

      implicit none

      integer(ip)           n
      character*(*)         string

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     N          I   An integer (less than 10**12 in absolute value).
!     STRING     O   An English string representing the cardinal of N.
!
! $ Detailed_Input
!
!     N        is any integer (less than 10**12 in absolute value).
!              If N is less than 0, -N must be a legitimate number.
!
! $ Detailed_Output
!
!     STRING   is the English cardinal equivalent of N. STRING will
!              contain only upper case letters.
!
!              The longest possible output string contains 145
!              characters. One such string is:
!
!                 'NEGATIVE '                                  //
!                 'SEVEN HUNDRED SEVENTY-SEVEN BILLION '       //
!                 'SEVEN HUNDRED SEVENTY-SEVEN MILLION '       //
!                 'SEVEN HUNDRED SEVENTY-SEVEN THOUSAND '      //
!                 'SEVEN HUNDRED SEVENTY-SEVEN'
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the resulting text is longer than the output string,
!         it will be truncated on the right, leaving only the most
!         significant portion of the number.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is used primarily for constructing error messages.
!     For example, an overflow message might look like the following:
!
!        'An excess of seventy-four parameters was detected.'
!
!     A second use might be to write dollar amounts: it's much harder
!     to tamper with a string like
!
!        'Two thousand four hundred seventy-one dollars'
!
!     than with the equivalent string
!
!        '$ 2471.00'
!
! $ Examples
!
!     N           STRING
!     ------      ------------------------------------------
!     -43         NEGATIVE FORTY-THREE
!      1          ONE
!      2          TWO
!      3          THREE
!      4          FOUR
!      20         TWENTY
!      21         TWENTY-ONE
!      99         NINETY-NINE
!      82131      EIGHTY-TWO THOUSAND ONE HUNDRED THIRTY-ONE
!
! $ Restrictions
!
!     1)  This routine assumes that N will always be less than
!         a trillion (10**12) in absolute value.
!
!     2)  In the event that N is less than zero, this routine assumes
!         that -N is a legitimate integer on the host machine.
!
!     3)  This routine assumes that an integer as large as 10**9
!         (one billion) is representable on the host machine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 15-AUG-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     convert an integer to text
!
!-&


!
!     Local variables
!
      integer(ip)           lngest
      parameter           ( lngest = 9 )

      character*(lngest)    number (19)
      character*(lngest)    suff
      character*(lngest)    tens   ( 9)

      integer(ip)           num
      integer(ip)           pad
      integer(ip)           space
      integer(ip)           x
      integer(ip)           y

!
!     Saved variables
!

      save                  number
      save                  tens

!
!     Initial values
!
      data       tens    /  'TEN',     'TWENTY',   'THIRTY', &
                            'FORTY',   'FIFTY',    'SIXTY', &
                            'SEVENTY', 'EIGHTY',   'NINETY' /

      data       number  / 'ONE',         'TWO', &
                           'THREE',       'FOUR', &
                           'FIVE',        'SIX', &
                           'SEVEN',       'EIGHT', &
                           'NINE',        'TEN', &
                           'ELEVEN',      'TWELVE', &
                           'THIRTEEN',    'FOURTEEN', &
                           'FIFTEEN',     'SIXTEEN', &
                           'SEVENTEEN',   'EIGHTEEN', &
                           'NINETEEN'                 /


!
!     Zero is easy.
!
      if ( n .eq. 0 ) then

         string = 'ZERO'
         return

      end if

!
!     If the number is negative, the string begins with the word
!     `NEGATIVE', and otherwise the number can be treated as though
!     it were positive.
!
      if ( n .lt. 0 ) then

         num    = - n
         string = 'NEGATIVE'

      else

         num    =   n
         string = ' '

      end if

!
!     Construct the number portion, from left to right: billions,
!     then millions, and so on. In case of overflow, SUFFIX simply
!     leaves the output string unchanged, so there is no need to
!     check explicitly for truncation.
!
      do while ( num .gt. 0 )

!
!        Find the right unit (billion, million, or whatever),
!        and the number (X) of those units. X should always
!        be between zero and 999, regardless of the units.
!
         if ( num .ge. 1000000000 ) then

            x    = num / 1000000000
            suff = 'BILLION'
            num  = num - x * 1000000000

         else if  ( num .ge. 1000000 ) then

            x    = num / 1000000
            suff = 'MILLION'
            num  = num - x * 1000000

         else if  ( num .ge. 1000 )    then

            x    = num / 1000
            suff = 'THOUSAND'
            num  = num - x * 1000

         else

            x    = num
            suff = ' '
            num  = 0

         end if

!
!        Convert X to text, ...
!
         space = 1

         do while ( x .gt. 0 )

            if ( string .eq. ' ' ) then
               pad = 0
            else
               pad = 1
            end if

            if ( x .ge. 100 ) then

               y = x / 100
               x = x - y*100

               call suffix ( number(y), pad, string )
               call suffix ( 'HUNDRED', 1,   string )

            else if ( x .ge. 20 ) then

               y = x / 10
               x = x - y*10

               call suffix ( tens(y), pad, string )

               if ( x .ne. 0 ) then
                  call suffix ( '-', 0, string )
                  space = 0
               end if

            else

               y = x
               x = 0

               if ( string .eq. ' ' ) then
                  space = 0
               end if

               call suffix ( number(y), space, string )

            end if

         end do

!
!        ... then add the units. Repeat as necessary.
!
         call suffix ( suff, 1, string )

      end do

      return
      end
