!$Procedure INTORD ( Convert an integer to ordinal text )

      subroutine intord ( n, string )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Convert an integer to an equivalent written ordinal phrase.
!     For example, convert 121 to 'ONE HUNDRED TWENTY-FIRST'.
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

      integer(ip), parameter :: maxord = 148

      integer(ip)           n
      character(*)          string

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     N          I   An integer (less than 10**12 in absolute value).
!     STRING     O   An English string representing the ordinal of N.
!
! $ Detailed_Input
!
!     N        is an integer (less than 10**12 in absolute value).
!              Moreover, if N is less than zero, -N must be a
!              a legitimate number on the host machine.
!
!              In the context of this routine N represents the
!              ranking of some item within a group.
!
! $ Detailed_Output
!
!     STRING   is the English ordinal equivalent of N. STRING will
!              contain only upper case letters.
!
! $ Parameters
!
!     MAXORD   is one more than the length of the longest ordinal
!              string that can be produced by a call to this routine:
!              One string of maximum length is:
!
!                 'NEGATIVE '                                  //
!                 'SEVEN HUNDRED SEVENTY-SEVEN BILLION '       //
!                 'SEVEN HUNDRED SEVENTY-SEVEN MILLION '       //
!                 'SEVEN HUNDRED SEVENTY-SEVEN THOUSAND '      //
!                 'SEVEN HUNDRED SEVENTY-SEVENTH'
!
!              It has 147 characters.
!
!              The parameter MAXORD is used to declare a local string
!              of sufficient length to allow the construction of
!              any ordinal string.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the resulting ordinal is longer than the output string,
!         it will be truncated on the right, leaving only the most
!         significant portion of the ordinal.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is used primarily for generating error messages. For example,
!     if the third letter or token in a string is in error, it might
!     be desirable to supply a message like the following:
!
!        'The third token of 31-JAN-198$ is not a valid year.'
!
! $ Examples
!
!     N           STRING
!     ------      -------------------------------------------
!     -6          NEGATIVE SIXTH
!      1          FIRST
!      2          SECOND
!      3          THIRD
!      4          FOURTH
!      20         TWENTIETH
!      21         TWENTY-FIRST
!      99         NINETY-NINTH
!      82131      EIGHTY-TWO THOUSAND ONE HUNDRED THIRTY-FIRST
!
! $ Restrictions
!
!     1)  Whatever restrictions apply to INTTXT apply to this routine
!         as well.
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
!     convert an integer to ordinal text
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           lastnb

!
!     Local variables
!
      integer(ip)           i
      integer(ip)           last
      character(maxord)     mystr

!
!     First get the English equivalent of the cardinal N.
!
      mystr = ' '

      call inttxt ( n, mystr )

      last  = lastnb(mystr)
      i     = last

!
!     Find the beginning of the last number of MYSTR.
!
      do while (       ( mystr(i:i) /= '-' ) &
                 .and. ( mystr(i:i) /= ' ' ) &
                 .and. ( i          >  1  )  )

         i = i - 1

      end do

      if (      ( mystr(i:i) == ' ' ) &
           .or. ( mystr(i:i) == '-' )  ) then

         i = i + 1

      end if

!
!     Now convert the last cardinal to an ordinal.
!
      if ( mystr(i:last) == 'ONE' ) then

         mystr(i:) = 'FIRST'

      else if ( mystr(i:last) == 'TWO' ) then

         mystr(i:) = 'SECOND'

      else if ( mystr(i:last) == 'THREE' ) then

         mystr(i:) = 'THIRD'

      else if ( mystr(i:last) == 'FIVE' ) then

         mystr(i:) = 'FIFTH'

      else if ( mystr(i:last) == 'EIGHT' ) then

         mystr(i:) = 'EIGHTH'

      else if ( mystr(i:last) == 'NINE' ) then

         mystr(i:) = 'NINTH'

      else if ( mystr (i:last) == 'TWELVE' ) then

         mystr(i:) = 'TWELFTH'

      else if ( mystr(last:last) == 'Y' ) then

         mystr(last:) = 'IETH'

      else

         call suffix ( 'TH', 0, mystr )

      end if

!
!     Now simply put MYSTR into STRING and return.
!
      string = mystr

      return
      end
