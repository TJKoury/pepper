!$Procedure CPOSR ( Character position, reverse )

      integer(ip) function cposr ( str, chars, start )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Find the first occurrence in a string of a character belonging
!     to a collection of characters, starting at a specified location,
!     searching in reverse.
!
! $ Required_Reading
!
!     SCANNING
!
! $ Keywords
!
!     CHARACTER
!     SEARCH
!     UTILITY
!
! $ Declarations

      implicit none

      character*(*)         str
      character*(*)         chars
      integer(ip)           start

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STR        I   Any character string.
!     CHARS      I   A collection of characters.
!     START      I   Position to begin looking for one of CHARS
!
!     The function returns the index of the last character of STR
!     at or before index START that is in the collection CHARS.
!
! $ Detailed_Input
!
!     STR      is any character string.
!
!     CHARS    is a character string containing a collection of
!              characters. Spaces in CHARS are significant.
!
!     START    is the position in STR to begin looking for one of the
!              characters in CHARS.
!
! $ Detailed_Output
!
!     The function returns the index of the last character of STR (at
!     or before index START) that is one of the characters in the
!     string CHARS. If none of the characters is found, the function
!     returns zero.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If START is less than 1, CPOSR returns zero.
!
!     2)  If START is greater than LEN(STRING), the search begins
!         at the last character of the string.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     CPOSR is case sensitive.
!
!     An entire family of related SPICELIB routines (POS, CPOS, NCPOS,
!     POSR, CPOSR, NCPOSR) is described in the Required Reading.
!
!     Those familiar with the True BASIC language should note that
!     these functions are equivalent to the True BASIC intrinsic
!     functions with the same name.
!
! $ Examples
!
!     Let STRING = 'BOB, JOHN, TED, AND MARTIN    '
!                   123456789012345678901234567890
!
!     Normal (sequential) searching:
!     ------------------------------
!
!           CPOSR( STRING, ' ,',    30 ) = 30
!
!           CPOSR( STRING, ' ,',    29 ) = 29
!
!           CPOSR( STRING, ' ,',    28 ) = 28
!
!           CPOSR( STRING, ' ,',    27 ) = 27
!
!           CPOSR( STRING, ' ,',    26 ) = 20
!
!           CPOSR( STRING, ' ,',    19 ) = 16
!
!           CPOSR( STRING, ' ,',    15 ) = 15
!
!           CPOSR( STRING, ' ,',    14 ) = 11
!
!           CPOSR( STRING, ' ,',    10 ) = 10
!
!           CPOSR( STRING, ' ,',     9 ) =  5
!
!           CPOSR( STRING, ' ,',     4 ) =  4
!
!           CPOSR( STRING, ' ,',     3 ) =  0
!
!     START out of bounds:
!     --------------------
!
!           CPOSR( STRING, ' ,',   231 ) = 30
!
!           CPOSR( STRING, ' ,',    31 ) = 30
!
!           CPOSR( STRING, ' ,',     0 ) =  0
!
!           CPOSR( STRING, ' ,',   -10 ) =  0
!
!
!     Order within CHARS
!     ------------------
!
!           CPOSR( STRING, 'JOHN',  23 ) =  18
!
!           CPOSR( STRING, 'OHNJ',  23 ) =  18
!
!           CPOSR( STRING, 'HNJO',  23 ) =  18
!
!           CPOSR( STRING, 'NJOH',  23 ) =  18
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
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.3, 31-JAN-2008 (BVS)
!
!        Removed non-standard end-of-declarations marker
!        'C%&END_DECLARATIONS' from comments.
!
!-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.1, 26-MAR-1991 (HAN)
!
!        The Required Reading file POSITION was renamed to SCANNING.
!        This header was updated to reflect the change.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     backward search for the position of a character
!
!-&


!
!     Local variables
!
      logical               found
      integer(ip)           lenstr
      integer(ip)           b


      lenstr = len ( str )

      b      = min(lenstr,start)
      found  = .false.
      cposr  = 0

      do while ( .not. found )

         if ( b .le. 0 ) then
            return
         else if ( index( chars, str(b:b) ) .ne. 0 ) then
            cposr = b
            return
         else
            b     = b - 1
         end if

      end do

      return
      end
