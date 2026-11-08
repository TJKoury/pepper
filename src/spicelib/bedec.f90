!$Procedure BEDEC  ( Be a decimal number? )

      logical function bedec  ( string )

! $ Abstract
!
!     Determine whether a string represents a decimal number.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ALPHANUMERIC
!     NUMBERS
!     SCANNING
!     UTILITY
!
! $ Declarations

      implicit none

      character*(*)         string

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   Character string.
!
!     The function returns .TRUE. if the string represents a decimal
!     number. Otherwise, it returns .FALSE.
!
! $ Detailed_Input
!
!     STRING   is any string.
!
! $ Detailed_Output
!
!     If the input string contains a decimal number (as defined
!     in $Particulars below), the function returns .TRUE. Otherwise,
!     the functions returns .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     A decimal number may be constructed by concatenating
!     the following components in the order shown.
!
!        1) A sign ('+' or '-'), or the null string.
!
!        2) An unsigned integer (as defined by function BEUNS),
!           or the null string.
!
!        3) A decimal point, or the null string.
!
!        4) An unsigned integer, or the null string.
!
! $ Examples
!
!     Four classes of numbers recognized by the various BE functions.
!
!        UNS      unsigned integer
!        INT      integer                (includes INT)
!        DEC      decimal number         (includes UNS, INT)
!        NUM      number                 (includes UNS, INT, NUM)
!
!     The following table illustrates the differences between
!     the classes. (Any number of leading and trailing blanks
!     are acceptable.)
!
!        String                  Accepted by
!        ------------------      ------------------
!        0                       UNS, INT, DEC, NUM
!        21
!        21994217453648
!
!        +0                      INT, DEC, NUM
!        -13
!        +21946
!
!        1.23                    DEC, NUM
!        12.
!        .17
!        +4.1
!        -.25
!
!        2.3e17                  NUM
!        17.D-13275849
!        -.194265E+0004
!
!     Note that the functions don't take the magnitudes of the numbers
!     into account. They may accept numbers that cannot be represented
!     in Fortran variables. (For example, '2.19E999999999999' probably
!     exceeds the maximum floating point number on any machine, but
!     is perfectly acceptable to BENUM.)
!
!     The following strings are not accepted by any of the functions.
!
!        String             Reason
!        ---------------    ----------------------------------------
!        3/4                No implied operations (rational numbers)
!        37+14              No explicit operations
!        E12                Must have mantissa
!        217,346.91         No commas
!        3.14 159 264       No embedded spaces
!        PI                 No special numbers
!        FIVE               No textual numbers
!        CXIV               No roman numerals
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 24-NOV-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 01-DEC-1995 (WLT)
!
!-&


! $ Index_Entries
!
!     determine if a string is a decimal number
!
!-&


!
!     SPICELIB functions
!
      logical               beint
      logical               beuns
      integer               pos

!
!     Local Variables
!
      integer               c
      integer               d
      integer               e
      integer               l


!
!     First determine whether or not a decimal point is present.
!
      d = pos ( string, '.', 1 )
      c = d-1
      e = d+1

      if ( d .eq. 0 ) then

!
!        If there is no decimal point just apply the integer test.
!
         bedec = beint ( string )

      else

!
!        A decimal point is present, get the length of the string
!        and see where the decimal point is relative to the last
!        character.
!
         l = len(string)

         if ( l .eq. 1 ) then

!
!           The string is one character long and a decimal point.
!           Sorry, this is not a decimal number.
!
            bedec = .false.

         else if ( d .eq. 1 ) then

!
!           The decimal point occurs as the first character of the
!           string.  The string following it must begin with
!           a non-blank character and be an unsigned integer.
!
            bedec =       string(e:e) .ne. ' ' &
                     .and. beuns  ( string(e:) )

         else if ( d .eq. l ) then

!
!           The decimal point is the last character of the string.
!           The character that precedes it must be non-blank and
!           the substring to the left must be an integer.
!
            bedec =       string(c:c) .ne. ' ' &
                     .and. beint ( string(1:c) )

         else if ( string(c:c) .eq. ' ' ) then

!
!           The decimal point occurs somewhere in the middle of the
!           string and the character preceding it is blank.
!
            bedec =        string(e:e) .ne. ' ' &
                     .and. string(1:c) .eq. ' ' &
                     .and. beuns  ( string(e:) )

         else if ( string(e:e) .eq. ' ' ) then

!
!           Again the decimal point occurs somewhere in the middle of
!           the string and the character following it is blank.
!
            bedec =        string(e:l) .eq. ' ' &
                     .and. string(c:c) .ne. ' ' &
                     .and. beint ( string(1:c) )

         else if ( string(c:c) .eq. '-' .or. string(c:c) .eq. '+' ) then

!
!           The decimal point is in the middle of the string and
!           is preceded by a '+' or '-'.  There should be nothing
!           preceding the sign and what follows the decimal point
!           should be an unsigned integer. (we already know that the
!           character following the decimal point is not a blank)
!
            if ( c .eq. 1 ) then
               bedec = beuns (string(e:l))
            else
               bedec =        beuns ( string(e:l) ) &
                        .and. string(1:c-1) .eq. ' '
            end if

         else

!
!            Last chance, the decimal point is in the middle of the
!            string.  The characters to the right and left of the
!            point are non-blank and we know the character to the
!            left of the point is not a sign.  The string left must
!            be an integer, the string to the right must be an
!            unsigned integer.
!
             bedec =       beint  ( string(1:c) ) &
                     .and. beuns  ( string(e:l) )

         end if

      end if

      return
      end
