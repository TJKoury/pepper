!$Procedure LX4UNS (Scan for unsigned integer)

      subroutine lx4uns ( string, first, last, nchar )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Scan a string from a specified starting position for the
!     end of an unsigned integer.
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

      character(*)          string
      integer(ip)           first
      integer(ip)           last
      integer(ip)           nchar

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   any character string
!     FIRST      I   first character to scan from in STRING
!     LAST       O   last character that is part of an unsigned integer
!     NCHAR      O   number of characters in the unsigned integer.
!
! $ Detailed_Input
!
!     STRING   is any character string.
!
!     FIRST    is the location in the string to beginning scanning
!              for an unsigned integer. It is assumed that the
!              unsigned integer begins at FIRST.
!
! $ Detailed_Output
!
!     LAST     is the last character at or after FIRST such that
!              the substring STRING(FIRST:LAST) is an unsigned
!              integer. If there is no such substring, LAST
!              will be returned with the value FIRST-1.
!
!     NCHAR    is the number of characters in the unsigned integer
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
!     2)  If STRING(FIRST:FIRST) is not part of an unsigned integer then
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
!     an unsigned integer that begins on the input character FIRST.
!
! $ Examples
!
!     Suppose you believe that a string has the form
!
!        X%Y%Z
!
!     where X, Y, and Z are unsigned integers of some unknown
!     length and % stands for some non-digit character. You could
!     use this routine to locate the unsigned integers in the
!     string as shown below. We'll keep track of the beginning and
!     ending of the unsigned integers in the integer arrays B and E.
!
!     FIRST = 1
!     I     = 0
!
!     DO WHILE ( FIRST .LT. LEN(STRING) )
!
!        CALL LX4UNS ( STRING, FIRST, LAST, NCHAR )
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
!     1)  Assumes ICHAR returns values in the range [-128, 255].
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
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
!-    SPICELIB Version 1.1.0, 03-DEC-2001 (NJB)
!
!        Updated to work if non-printing characters are present in
!        the input string. Updated $Restrictions section.
!
!-    SPICELIB Version 1.0.0, 12-JUL-1994 (WLT)
!
!-&


! $ Index_Entries
!
!     Scan a string for an unsigned integer.
!
!-&


!
!     Local parameters
!
      integer(ip)           maxc
      parameter           ( maxc    =   255 )

      integer(ip)           minc
      parameter           ( minc    =  -128 )

!
!     Local variables
!
      integer(ip)           i
      integer(ip)           l

      logical               digit ( minc : maxc )
      logical               doinit

!
!     Saved variables
!
      save

!
!     Initial values
!
      data                  doinit / .true. /

!
!     First we perform some initializations that are needed on
!     each pass through this routine.
!
      if ( doinit ) then

         doinit = .false.

         do i = minc, maxc
            digit(i)  = .false.
         end do

         digit(ichar('0')) = .true.
         digit(ichar('1')) = .true.
         digit(ichar('2')) = .true.
         digit(ichar('3')) = .true.
         digit(ichar('4')) = .true.
         digit(ichar('5')) = .true.
         digit(ichar('6')) = .true.
         digit(ichar('7')) = .true.
         digit(ichar('8')) = .true.
         digit(ichar('9')) = .true.

      end if


      last  = first - 1
      l     = len ( string )

!
!     If start is beyond the ends of the string, we  can quit now.
!
      if ( first < 1 .or. first > l ) then
         nchar = 0
         return
      end if

!
!     Now for the real work of the routine. Examine characters one
!     at a time...
!
      do i = first, l

!
!        If this character is a digit, move the LAST pointer one
!        further down on the string.  Otherwise set NCHAR and return.
!
         if ( digit(ichar(string(i:i))) ) then
            last  = last + 1
         else
            nchar = last + 1 - first
            return
         end if
      end do

      nchar = last + 1 - first

      return
      end
