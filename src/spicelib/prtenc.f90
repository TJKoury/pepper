!$Procedure PRTENC ( Encode a character string, portably )

      subroutine prtenc ( number, string )

! $ Abstract
!
!     Encode a nonnegative integer number into a character string,
!     portably, using 128 as the base for encoding.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CELLS
!     CHARACTER
!
! $ Declarations

      implicit none

      integer            number
      character*(*)      string

      integer            minlen
      parameter        ( minlen = 5 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NUMBER     I   Number to be encoded.
!     STRING     O   Encoded string.
!     MINLEN     P   Minimum length of string.
!
! $ Detailed_Input
!
!     NUMBER   is an arbitrary nonnegative integer.
!
! $ Detailed_Output
!
!     STRING   is the character string implied by the ASCII
!              interpretation of NUMBER when converted to its
!              base 128 representation.
!
!              Let L be the declared length of STRING, and let
!              NUMBER be given by
!
!                                  0           1                 L-1
!                 NUMBER = a    128  + a    128  + ... + a    128
!                           1           2                 L
!
!              Then
!
!                 STRING(i:i) = CHAR(a )   for i = 1, L
!                                     i
!
!              Note that, just as for any other "numbers",
!              the "digits" in STRING are arranged from right
!              to left in order of increasing significance.
!              The string is, in effect, "padded with nulls"
!              on the left.
!
! $ Parameters
!
!     MINLEN   is the minimum length of a string into which a
!              number may be encoded. In order to avoid padding
!              long strings with hundreds, possibly thousands
!              of null characters, only the first MINLEN characters
!              of the string are actually used. Note that this
!              also allows the encoded number to be preserved
!              during assignments,
!
!                 STR1 = STR2
!
!              so long as both strings are of length MINLEN or
!              greater.
!
! $ Exceptions
!
!     1)  If the length of the output string is less than MINLEN,
!         the error SPICE(INSUFFLEN) is signaled.
!
!     2)  If the number to be encoded is negative, the error
!         SPICE(OUTOFRANGE) is signaled.
!
!                                                       MINLEN
!     3)  If the number to be encoded is larger than 128       - 1,
!         the error SPICE(OUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is identical to ENCHAR, except that this routine
!     does not use the machine-dependent encoding base returned by
!     the SPICELIB routine CHBASE. Instead, the base 128 is used.
!     This base is expected to work on all systems supporting ASCII
!     encoding of characters.
!
! $ Examples
!
!     See: SCARDC, SSIZEC.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     encode a character string, portably
!
!-&


!
!     Local parameters
!
      integer               chbase
      parameter           ( chbase = 128 )

!
!     Local variables
!
      integer               base
      integer               num
      integer               remain
      integer               i

!
!     Standard SPICE error handling.
!
      if ( len ( string ) .lt. minlen ) then
         call chkin  ( 'PRTENC' )
         call sigerr ( 'SPICE(INSUFFLEN)' )
         call chkout ( 'PRTENC' )
         return

      else if ( number .lt. 0 ) then
         call chkin  ( 'PRTENC' )
         call sigerr ( 'SPICE(OUTOFRANGE)' )
         call chkout ( 'PRTENC' )
         return
      end if

!
!     Generate the digits from right to left.
!
      base = chbase
      num  = number

      do i = minlen, 1, -1

         remain      = mod ( num, base )
         string(i:i) = char ( remain )
         num         = num / base

      end do

!
!     More error handling.
!
      if ( num .gt. 0 ) then
         call chkin  ( 'PRTENC' )
         call sigerr ( 'SPICE(OUTOFRANGE)' )
         call chkout ( 'PRTENC' )
      end if

      return




!$Procedure PRTDEC ( Decode a character string )

      entry prtdec ( string, number )

! $ Abstract
!
!     Decode a character string encoded by PRTENC.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!
! $ Declarations
!
!     CHARACTER*(*)      STRING
!     INTEGER            NUMBER
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   Encoded character string.
!     NUMBER     O   Decoded number.
!
! $ Detailed_Input
!
!     STRING   is a character string previously encoded by PRTENC.
!              This contains an integer in base 128 notation,
!              where 128 is a function of the size of the
!              available character set. See PRTENC for details
!              about the format of STRING.
!
! $ Detailed_Output
!
!     NUMBER   is the integer encoded in the input string.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the length of the input string is less than MINLEN,
!         the error SPICE(INSUFFLEN) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     PRTDEC is the inverse of PRTENC. In the example below,
!
!           CALL PRTENC (      I, STRING )
!           CALL PRTDEC ( STRING,      J )
!
!           IF ( I .EQ. J ) THEN
!            .
!            .
!           END IF
!
!     the logical test (I .EQ. J) is always true.
!
!     This routine is identical to DECHAR, except that this routine
!     does not use the machine-dependent encoding base returned by
!     the SPICELIB routine CHBASE. Instead, the base 128 is used.
!     This base is expected to work on all systems supporting ASCII
!     encoding of characters.
!
! $ Examples
!
!     See: CARDC, SIZEC.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     decode a portably encoded character string
!
!-&


      if ( len ( string ) .lt. minlen ) then
         call chkin  ( 'PRTDEC' )
         call sigerr ( 'SPICE(INSUFFLEN)' )
         call chkout ( 'PRTDEC' )
         return
      end if

!
!     Sum the products of the 'digits' and the corresponding powers
!     of NDCHAR, just like any other base conversion.
!
      base   = chbase
      number = 0

      do i = 1, minlen
         number = base*number + ichar ( string(i:i) )
      end do

      return
      end
