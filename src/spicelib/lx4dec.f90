!$Procedure LX4DEC (Scan for signed integer)

      subroutine lx4dec ( string, first, last, nchar )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Scan a string from a specified starting position for the
!     end of a decimal number.
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
!     LAST       O   last character that is part of a decimal number
!     NCHAR      O   number of characters in the decimal number.
!
! $ Detailed_Input
!
!     STRING   is any character string.
!
!     FIRST    is the location in the string to beginning scanning
!              for a decimal number. It is assumed that the
!              decimal number begins at FIRST.
!
! $ Detailed_Output
!
!     LAST     is the last character at or after FIRST such that
!              the substring STRING(FIRST:LAST) is a decimal
!              number. If there is no such substring, LAST
!              will be returned with the value FIRST-1.
!
!     NCHAR    is the number of characters in the decimal number
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
!     2)  If STRING(FIRST:FIRST) is not part of a decimal number then
!         LAST will be returned with the value FIRST-1 and NCHAR will be
!         returned with the value 0.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows you to scan forward in a string to locate
!     a decimal number that begins on the input character FIRST. Note
!     that all signed integers are included in the list of decimal
!     numbers. See LX4SGN for a description of signed integers.
!
!     We let S stand for a signed integer and U stand for
!     an unsigned integer. With this notation, the strings
!     recognized as decimal numbers are:
!
!        U
!        S
!        S.
!        S.U
!         .U
!        -.U
!        +.U
!
! $ Examples
!
!     Suppose you believe that a string has the form
!
!        X%Y%Z
!
!     where X, Y, and Z are decimal numbers of some unknown
!     length and % stands for some non-digit character. You could
!     use this routine to locate the decimal numbers in the
!     string as shown below. We'll keep track of the beginning and
!     ending of the decimal numbers in the integer arrays B and E.
!
!     FIRST = 1
!     I     = 0
!
!     DO WHILE ( FIRST .LT. LEN(STRING) )
!
!        CALL LX4DEC ( STRING, FIRST, LAST, NCHAR )
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
!-    SPICELIB Version 1.2.0, 04-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Fixed return value for LAST in $Exceptions section entry #1.
!
!-    SPICELIB Version 1.1.0, 28-NOV-1995 (WLT)
!
!        Upgraded the routine to handle strings of the form
!        '+.01' and '-.01' which were regarded as non-decimal
!        strings before.
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
      integer(ip)           j
      integer(ip)           l
      integer(ip)           n
      integer(ip)           next

      last  = first - 1
      next  = first + 1
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
!     LX4SGN or LX4UNS do  almost all of the work).
!
      i = ichar( string(first:first) )

      if ( next .lt. l ) then
         j = ichar( string(next:next) )
      else
         j = ichar( ' ' )
      end if

      if ( i .eq. ichar('.' ) ) then
!
!        Case 1. The string begins with a decimal point.
!        There must be an unsigned integer following.
!
         f = first + 1

         call lx4uns ( string, f, last, nchar )

         if ( nchar .eq. 0 ) then
            last  = first - 1
         else
            nchar = nchar + 1
         end if

      else if (    (     i .eq. ichar('-') &
                    .or. i .eq. ichar('+') ) &
               .and.     j .eq. ichar('.')    ) then
!
!        Case 2. The string begins with a sign followed by
!        a decimal point. There must be an unsigned integer following.
!
         f = next + 1

         call lx4uns ( string, f, last, nchar )

         if ( nchar .eq. 0 ) then
            last  = first - 1
         else
            nchar = nchar + 1
         end if

      else if ( i .eq. ichar('+') .and. j .eq. ichar('.') ) then
!
!        Case 2. The string begins with a minus sign followed by
!        a decimal point. There must be an unsigned integer following.
!
         f = next + 1

         call lx4uns ( string, f, last, nchar )

         if ( nchar .eq. 0 ) then
            last  = first - 1
         else
            nchar = nchar + 1
         end if

      else
!
!        Case 3.  The leading character is not a decimal point.
!        First check to see how much signed integer we have.
!
         call lx4sgn ( string, first, last, nchar )
!
!        If we got some part of a signed integer, we next see
!        if there is a decimal point followed by an unsigned
!        integer.
!
         if ( nchar .gt. 0 .and. last .lt. l ) then

            f = last + 1
            i = ichar( string(f:f) )

            if ( i .eq. ichar( '.' ) ) then

               last = f
               f    = last + 1
!
!              After the decimal point we may have an unsigned integer.
!
               call lx4uns ( string, f, last, n )
!
!              LAST is either pointing to the decimal point or the
!              end of an unsigned integer.  In either case we need
!              to update NCHAR.
!
               nchar = last + 1 - first

            end if

         end if

      end if

      return
      end
