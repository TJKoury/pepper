!$Procedure BENUM  ( Be a number? )

      logical function benum  ( string )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Determine whether a string represents a number.
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
!     The function returns .TRUE. if the string is a number.
!     Otherwise, it returns .FALSE.
!
! $ Detailed_Input
!
!     STRING   is any string.
!
! $ Detailed_Output
!
!     If the input string contains a number (as defined in
!     $Particulars below) the function returns .TRUE. Otherwise,
!     the function returns .FALSE.
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
!     A number may be either of the following:
!
!        1) A decimal number (as defined by function BEDEC).
!
!        2) A decimal number followed by an exponent character
!           ('E', 'e', 'D', or 'd') and an integer (as defined
!           by function BEINT).
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
!     determine if a string is a number
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           cpos
      logical               beint
      logical               bedec

!
!     Local Variables
!
      integer(ip)           d
      integer(ip)           e
      integer(ip)           f
      integer(ip)           l




!
!     Determine whether or not there is an exponent character in the
!     string.
!
      l = len  ( string            )
      e = cpos ( string, 'EeDd', 1 )
      d = e-1
      f = e+1


      if ( e .eq. 0 ) then

!
!        There is no exponent character, this is a number if it
!        is a decimal number.
!
         benum = bedec ( string )

      else if (     ( e .eq. 1 ) &
               .or. ( e .eq. l )  ) then

         benum = .false.

      else if (      ( string(d:d) .eq. ' ' ) &
                .or. ( string(f:f) .eq. ' ' ) ) then

         benum = .false.

      else

         benum =      bedec ( string(1:d) ) &
                .and. beint ( string(f:l) )

      end if

      return
      end
