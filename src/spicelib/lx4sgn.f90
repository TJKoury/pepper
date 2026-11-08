!$Procedure LX4SGN (Scan for signed integer)

      subroutine lx4sgn ( string, first, last, nchar )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Scan a string from a specified starting position for the
!     end of a signed integer.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PARSING
!
! $ Declarations

      implicit none

      character*(*)         string
      integer(ip)           first
      integer(ip)           last
      integer(ip)           nchar

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   any character string
!     FIRST      I   first character to scan from in STRING
!     LAST       O   last character that is part of a signed integer
!     NCHAR      O   number of characters in the signed integer.
!
! $ Detailed_Input
!
!     STRING   is any character string.
!
!     FIRST    is the location in the string to beginning scanning
!              for a signed integer. It is assumed that the
!              signed integer begins at FIRST.
!
! $ Detailed_Output
!
!     LAST     is the last character at or after FIRST such that
!              the substring STRING(FIRST:LAST) is a signed
!              integer. If there is no such substring, LAST
!              will be returned with the value FIRST-1.
!
!     NCHAR    is the number of characters in the signed integer
!              that begins at FIRST and ends at last. If there
!              is no such string NCHAR will be given the value 0.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If FIRST is beyond either end of the string, then LAST will be
!         returned with the value FIRST-1 and NCHAR will be returned
!         with the value 0.
!
!     2)  If STRING(FIRST:FIRST) is not part of a signed integer
!         then LAST will be returned with the value FIRST-1 and NCHAR
!         will be returned with the value 0.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows you to scan forward in a string to locate
!     a signed integer that begins on the input character FIRST. Note
!     that all unsigned integers are included in the list of signed
!     integers. The signed integers may in addition have a leading
!     plus ('+') or minus ('-') sign.
!
! $ Examples
!
!     Suppose you believe that a string has the form
!
!        X%Y%Z
!
!     where X, Y, and Z are signed integers of some unknown
!     length and % stands for some non-digit character. You could
!     use this routine to locate the signed integers in the
!     string as shown below. We'll keep track of the beginning and
!     ending of the signed integers in the integer arrays B and E.
!
!     FIRST = 1
!     I     = 0
!
!     DO WHILE ( FIRST .LT. LEN(STRING) )
!
!        CALL LX4SGN ( STRING, FIRST, LAST, NCHAR )
!
!        IF ( NCHAR .GT. 0 ) THEN
!
!           I     = I    + 1
!           B(I)  = FIRST
!           E(I)  = LAST
!           FIRST = LAST + 2
!
!        ELSE
!
!           FIRST = FIRST + 1
!
!        END IF
!
!     END DO
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
!-    SPICELIB Version 1.1.0, 04-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Fixed return value for LAST in $Exceptions section entry #1.
!
!-    SPICELIB Version 1.0.0, 12-JUL-1994 (WLT)
!
!-&


! $ Index_Entries
!
!     Scan a string for a signed integer.
!
!-&


      integer(ip)           f
      integer(ip)           i
      integer(ip)           l

      last  = first - 1
      l     = len ( string )
!
!     If start is beyond the ends of the string, we  can quit now.
!
      if ( first .lt. 1 .or. first .gt. l ) then
         nchar = 0
         return
      end if
!
!     There are two cases to take care of (and in both cases
!     LX4UNS does almost all of the work).
!
      i = ichar( string(first:first) )

      if ( i .eq. ichar('+' ) .or. i .eq. ichar('-') ) then
!
!        Case 1. The string begins with a + or -.  There must
!        be an unsigned integer following.
!
         f = first + 1

         call lx4uns ( string, f, last, nchar )

         if ( nchar .eq. 0 ) then
            last  = first - 1
         else
            nchar = nchar + 1
         end if

      else
!
!        Case 2.  The leading character is not a sign character.
!        We simply check to see how much unsigned integer we have.
!
         call lx4uns ( string, first, last, nchar )

      end if

      return
      end
