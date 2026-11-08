!$Procedure BEUNS  ( Be an unsigned integer? )

      logical function beuns  ( string )

! $ Abstract
!
!     Determine whether a string represents an unsigned integer.
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
!     The function returns .TRUE. if the string represents an unsigned
!     integer. Otherwise, it returns .FALSE.
!
! $ Detailed_Input
!
!     STRING   is any string.
!
! $ Detailed_Output
!
!     If STRING contains a single word made entirely from the
!     characters '0' through '9', then the function returns .TRUE.
!     Otherwise, it returns .FALSE.
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
!     By definition an unsigned integer is a word made exclusively
!     from the characters '0', '1', '2', '3', '4', '5', '6', '7', '8',
!     and '9'.
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
!     determine if a string is an unsigned integer
!
!-&


!
!     SPICE functions
!
      integer               frstnb

!
!     Local variables
!
      integer               i
      integer               l

      logical               ok



!
!     Get the length of the string and the position of its
!     first non-blank character.
!
      l     = len    ( string )
      i     = frstnb ( string )

!
!     If there isn't a non-blank character, this isn't an
!     unsigned integer.
!
      if ( i .eq. 0 ) then
         beuns  = .false.
         return
      end if

!
!     As far as we know right now, everything is ok.  Examine
!     characters until we run out of string or until we
!     hit a non-digit character.
!
      ok = .true.

      do while ( ok .and. ( i .le. l ) )

         if ( index('0123456789', string(i:i) ) .gt. 0  ) then
            i = i + 1
         else
            ok = .false.
         end if

      end do

!
!     If the string still is ok as an unsigned integer, it must be
!     one...
!
      if ( ok ) then
         beuns  = .true.
      else

!
!     ... otherwise, it's an unsigned integer if the remainder is blank.
!
         beuns  = string(i:) .eq. ' '
      end if

      return
      end
