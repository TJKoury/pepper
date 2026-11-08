!$Procedure PREFIX (Prefix a character string)

      subroutine prefix ( pref, spaces, string )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Add a prefix to a character string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASSIGNMENT
!     CHARACTER
!     STRING
!
! $ Declarations

      implicit none

      character*(*)    pref
      integer(ip)      spaces
      character*(*)    string

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PREF       I   Prefix.
!     SPACES     I   Number of spaces separating prefix and suffix.
!     STRING    I-O  Suffix on input, string on output.
!
! $ Detailed_Input
!
!     PREF     is the prefix to be added to the string. Trailing
!              blanks are ignored. (A blank prefix is interpreted
!              as a null prefix.)
!
!     SPACES   is the number of spaces (blanks) in the output
!              string separating the last non-blank character
!              of the prefix from the first (blank or non-blank)
!              character of the suffix. Typically, this will be
!              zero or one. If not positive, SPACES defaults to
!              zero.
!
!     STRING   on input is the suffix to which the prefix is to
!              be added. Leading blanks are significant.
!
! $ Detailed_Output
!
!     STRING   on output is the is the prefixed string. If STRING
!              is not large enough to contain the output string,
!              the output string is truncated on the right.
!
!              STRING may NOT overwrite PREF.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If SPACES is negative it is treated as zero.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The suffix is shifted to the right to make room for the prefix
!     and required spaces, which are then added to the front of the
!     string. (The shift operation handles any necessary truncation.)
!
! $ Examples
!
!     The following examples illustrate the use of PREFIX.
!
!           PREF         STRING (input)   SPACES    STRING (output)
!           ----------   --------------   ------    ---------------
!           'abc     '   'def    '             0    'abcdef '
!           'abc     '   'def    '             1    'abc def'
!           'abc     '   ' def   '             0    'abc def'
!           'abc     '   ' def   '             1    'abc  de'
!           ' abc    '   'def    '             0    ' abcdef'
!           ' abc    '   'def    '             1    ' abc de'
!           ' abc    '   ' def   '            -1    ' abc de'
!           '        '   'def    '             0    'def    '
!           '        '   'def    '             1    ' def   '
!           ' abc    '   '       '             0    ' abc   '
!
! $ Restrictions
!
!     1)  PREF and STRING must be distinct.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 18-MAR-2021 (JDR)
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     prefix a character_string
!
!-&


! $ Revisions
!
!-     Beta Version 1.1.0, 28-FEB-1989 (WLT)
!
!         Reference to SHIFT replaced by SHIFTL.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)      lastnb

!
!     Local variables
!
      integer(ip)      plen
      integer(ip)      slen
      integer(ip)      shift



!
!     L is the location of the last non-blank character in the prefix.
!     PLEN is the length of the prefix. Remember that a blank (null)
!     prefix has zero length.
!
      plen = lastnb ( pref )

!
!     SLEN is the allocated length of the string.
!
      slen = len ( string )

!
!     We can't just do a concatenation, because the input and output
!     strings are of indeterminate length. (This would be a violation
!     of the ANSI Fortran 77 standard.) Instead, we will shift the
!     suffix to the right in order to make room for the prefix and
!     the required number of spaces. If part of the string gets
!     truncated, well, that's life.
!
      shift = plen + max ( spaces, 0 )

      call shiftr ( string, shift, ' ', string )

!
!     Put the non-blank part of the prefix in the vacated part of
!     the string. The spaces will fill themselves in.
!
      if ( plen .gt. 0 ) then

         if ( shift .lt. slen ) then
            string(1:shift) = pref

         else
            string = pref
         end if

      end if

      return
      end
