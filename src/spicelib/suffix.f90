!$Procedure SUFFIX (Suffix a character string)

      subroutine suffix ( suff, spaces, string )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Add a suffix to a character string.
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

      character*(*)    suff
      integer(ip)      spaces
      character*(*)    string

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SUFF       I   Suffix.
!     SPACES     I   Number of spaces separating prefix and suffix.
!     STRING    I-O  Prefix on input, string on output.
!
! $ Detailed_Input
!
!     SUFF     is the suffix to be added to the string.
!              Leading blanks are significant. (A blank
!              suffix is interpreted as a null suffix.)
!
!     SPACES   is the number of spaces (blanks) in the output
!              string separating the last non-blank character
!              of the prefix from the first (blank or non-blank)
!              character of the suffix. Typically, this will be
!              zero or one. If not positive, SPACES defaults to
!              zero.
!
!     STRING   on input is the prefix to which the suffix is
!              to be added. Leading blanks are significant.
!              Trailing blanks are ignored.
!
! $ Detailed_Output
!
!     STRING   on output is the suffixed string. If STRING
!              is not large enough to contain the output string,
!              the output string is truncated on the right.
!
!              STRING may NOT overwrite SUFF.
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
!     The suffix is added to the right of the last non-blank character
!     of the prefix. (Any necessary truncation is done automatically.)
!
! $ Examples
!
!     The following examples illustrate the use of SUFFIX.
!
!           SUFF         STRING (input)   SPACES    STRING (output)
!           ----------   --------------   ------    ---------------
!           'abc     '   'def    '             0    'defabc '
!           'abc     '   'def    '             1    'def abc'
!           'abc     '   ' def   '             0    ' defabc'
!           'abc     '   ' def   '             1    ' def ab'
!           ' abc    '   'def    '             0    'def abc'
!           ' abc    '   'def    '             1    'def  ab'
!           ' abc    '   ' def   '            -1    ' def ab'
!           '        '   'def    '             0    'def    '
!           '        '   'def    '             1    'def    '
!           ' abc    '   '       '             0    ' abc   '
!           ' abc    '   '       '             1    '  abc  '
!
! $ Restrictions
!
!     1)  SUFF and STRING must be distinct.
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
!     suffix a character_string
!
!-&


!
!     SPICELIB functions
!
      integer(ip)      lastnb

!
!     Local variables
!
      integer(ip)      l
      integer(ip)      slen
      integer(ip)      end



!
!     SLEN is the allocated length of the string. L is the location of
!     the last non-blank character of the prefix.
!
      slen = len    ( string )
      l    = lastnb ( string )

!
!     Put the suffix at the end of the string. The spaces will fill
!     themselves in.
!
      end = l + max ( spaces, 0 )

      if ( end .lt. slen ) then
         string(end+1: ) = suff
      end if

      return
      end
