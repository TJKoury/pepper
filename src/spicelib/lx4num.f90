!$Procedure LX4NUM (Scan for a number)

      subroutine lx4num ( string, first, last, nchar )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Scan a string from a specified starting position for the
!     end of a number.
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
!     LAST       O   last character that is part of a number
!     NCHAR      O   number of characters in the number.
!
! $ Detailed_Input
!
!     STRING   is any character string.
!
!     FIRST    is the location in the string to beginning scanning
!              for a  number. It is assumed that the number begins
!              at FIRST.
!
! $ Detailed_Output
!
!     LAST     is the last character at or after FIRST such that
!              the substring STRING(FIRST:LAST) is a number.
!              If there is no such substring, LAST will be returned
!              with the value FIRST-1.
!
!     NCHAR    is the number of characters in the number
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
!     2)  If STRING(FIRST:FIRST) is not part of a number
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
!     a number that begins on the input character FIRST. Note
!     that all decimal numbers are included in the list of numbers.
!     The main difference between decimal numbers and numbers is that
!     numbers may have an exponential expression attached (i.e. the
!     exponent character 'e','E','d' or 'D' followed by an signed
!     integer).
!
! $ Examples
!
!     Suppose you believe that a string has the form
!
!        X%Y%Z
!
!     where X, Y, and Z are decimal numbers of some unknown
!     length and % stands for some non-numeric character. You could
!     use this routine to locate the numbers in the
!     string as shown below. We'll keep track of the beginning and
!     ending of the numbers in the integer arrays B and E.
!
!     FIRST = 1
!     I     = 0
!
!     DO WHILE ( FIRST .LT. LEN(STRING) )
!
!        CALL LX4NUM ( STRING, FIRST, LAST, NCHAR )
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
!     Scan a string for a number.
!
!-&


      integer(ip)           f
      integer(ip)           i
      integer(ip)           l
      integer(ip)           n
      integer(ip)           temp

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
!     If this is a number, it must begin with a decimal number
!     substring.
!
      call lx4dec ( string, first, last, nchar )

      if ( nchar .gt. 0 .and. last .lt. l ) then

         f = last + 1
         i = ichar ( string(f:f) )
!
!        See if we have an exponent.
!
         if (      i .eq. ichar('e') &
              .or. i .eq. ichar('E') &
              .or. i .eq. ichar('D') &
              .or. i .eq. ichar('d') ) then
!
!           Starting after the exponent character see
!           if we have a signed integer.
!
            f = f + 1

            call lx4sgn ( string, f, temp, n )
!
!           If there was a signed integer, N will be bigger than
!           zero and TEMP will point to the last character of
!           the number.  Otherwise we just fall through and leave
!           LAST and NCHAR alone.
!
            if ( n .gt. 0 ) then

               last  = temp
               nchar = last + 1 - first

            end if

         end if

      end if


      return
      end
