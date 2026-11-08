!$Procedure BEINT  ( Be an Integer? )

      logical function beint  ( string )

! $ Abstract
!
!     Determine whether a string represents an integer.
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
!     The function returns .TRUE. if the string represents an integer.
!     Otherwise, it returns .FALSE.
!
! $ Detailed_Input
!
!     STRING   is any string.
!
! $ Detailed_Output
!
!     If the input string contains an integer (as defined in
!     $Particulars below), the function returns .TRUE. Otherwise,
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
!     An integer may be either of the following:
!
!        1) An unsigned integer (as defined by function BEUNS).
!
!        2) A sign ('+' or '-') followed by an unsigned
!           integer.
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
!     determine if a string is an integer
!
!-&


      integer               frstnb
      logical               beuns

      integer               i
      integer               l


      character*(1)         letter


!
!     Find the first non-blank character and the length of the
!     string.
!
      l  =  len    ( string )
      i  =  frstnb ( string )

!
!     If there isn't a non-blank character, this isn't an
!     integer.
!
      if ( i .eq. 0 ) then
         beint  = .false.
         return
      end if


!
!     Copy the first non-blank letter in the string.
!
      letter = string(i:i)

      if      ( i .lt. l ) then

!
!        The first character is not the last, so we might start with
!        a plus or minus.  If so the rest must be an unsigned integer.
!
         if (      ( letter .eq. '+' ) &
              .or. ( letter .eq. '-' ) ) then

            i    = i + 1

            if ( string(i:i) .ne. ' ' ) then
               beint = beuns ( string(i:) )
            else
               beint = .false.
            end if

         else

!
!           If the first character isn't plus (+) or minus (-)
!           the string must be an unsigned integer if its going
!           to be an integer.
!
            beint = beuns ( string(i:) )

         end if

      else

!
!        If the first (non-blank) character is the last one, then
!        it must be an unsigned integer, for the string to
!        represent an integer.
!
         beint = beuns ( letter )

      end if

      return
      end
