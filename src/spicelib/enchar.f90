!$Procedure ENCHAR ( Encode a character string )

      subroutine enchar ( number, string )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Encode a nonnegative integer number into a character string
!     as the expansion of the number in base CHBASE (a function of
!     the size of the available character set).
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

      integer(ip)        number
      character*(*)      string

      integer(ip)        minlen
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
!              base CHBASE representation.
!
!              Let L be the declared length of STRING, and let
!              NUMBER be given by
!
!                                  0           1                 L-1
!                 NUMBER = a CHBASE  + a CHBASE  + ... + a CHBASE
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
!                                                          MINLEN
!     3)  If the number to be encoded is larger than CHBASE       - 1,
!         the error SPICE(OUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The value of CHBASE, which varies from machine to machine, is
!     returned by a constant function of the same name.
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
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 31-JAN-2008 (BVS)
!
!        Changed header section title '$C Revision' to '$C Revisions'.
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
!     encode a character_string
!
!-&


! $ Revisions
!
!-    Beta Version 2.0.0, 13-JAN-1989 (IMU)
!
!        Only the first MINLEN characters of the string are now
!        used to encode the value. Also, negative values are now
!        treated as errors.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)        chbase
      logical            return

!
!     Local variables
!
      integer(ip)        base
      integer(ip)        num
      integer(ip)        remain
      integer(ip)        i

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return

      else if ( len ( string ) .lt. minlen ) then
         call chkin  ( 'ENCHAR' )
         call sigerr ( 'SPICE(INSUFFLEN)' )
         call chkout ( 'ENCHAR' )
         return

      else if ( number .lt. 0 ) then
         call chkin  ( 'ENCHAR' )
         call sigerr ( 'SPICE(OUTOFRANGE)' )
         call chkout ( 'ENCHAR' )
         return
      end if

!
!     Generate the digits from right to left.
!
      base = chbase()
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
         call chkin  ( 'ENCHAR' )
         call sigerr ( 'SPICE(OUTOFRANGE)' )
         call chkout ( 'ENCHAR' )
      end if

      return




!$Procedure DECHAR ( Decode a character string )

      entry dechar ( string, number )

! $ Abstract
!
!     Decode a character string encoded by ENCHAR.
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
!     IMPLICIT NONE
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
!     STRING   is a character string previously encoded by ENCHAR.
!              This contains an integer in base CHBASE notation,
!              where CHBASE is a function of the size of the
!              available character set. See ENCHAR for details
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
!     DECHAR is the inverse of ENCHAR. In the example below,
!
!           CALL ENCHAR (      I, STRING )
!           CALL DECHAR ( STRING,      J )
!
!           IF ( I .EQ. J ) THEN
!            .
!            .
!           END IF
!
!     the logical test (I .EQ. J) is always true.
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
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 31-JAN-2008 (BVS)
!
!        Changed header section title '$C Revision' to '$C Revisions'.
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
!     decode a character_string
!
!-&


! $ Revisions
!
!-    Beta Version 2.0.0, 13-JAN-1989 (IMU)
!
!        Changed to reflect changes in ENCHAR. In particular,
!        it now checks the length of the input string. It is
!        also an entry point of ENCHAR, to make sure they always
!        have the same value of MINLEN. (Also, if CHBASE is
!        changed, ENCHAR and DECHAR will always be recompiled
!        simultaneously.)
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return

      else if ( len ( string ) .lt. minlen ) then
         call chkin  ( 'DECHAR' )
         call sigerr ( 'SPICE(INSUFFLEN)' )
         call chkout ( 'DECHAR' )
         return
      end if

!
!     Sum the products of the 'digits' and the corresponding powers
!     of NDCHAR, just like any other base conversion.
!
      base   = chbase()
      number = 0

      do i = 1, minlen
         number = base*number + ichar ( string(i:i) )
      end do

      return
      end
