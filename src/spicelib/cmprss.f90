!$Procedure CMPRSS ( Compress a character string )

      subroutine cmprss ( delim, n, input, output )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Compress a character string by removing occurrences of
!     more than N consecutive occurrences of a specified
!     character.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASCII
!     CHARACTER
!     STRING
!
! $ Declarations

      implicit none

      character*1         delim
      integer(ip)         n
      character*(*)       input
      character*(*)       output

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     DELIM      I   Delimiter to be compressed.
!     N          I   Maximum consecutive occurrences of DELIM.
!     INPUT      I   Input string.
!     OUTPUT     O   Compressed string.
!
! $ Detailed_Input
!
!     DELIM    is the delimiter to be compressed out of the string.
!              This may be any ASCII character.
!
!     N        is the maximum number of consecutive occurrences
!              of DELIM that will be allowed to remain in the
!              output string.
!
!     INPUT    is the input string.
!
! $ Detailed_Output
!
!     OUTPUT   is the output string. This is the input string
!              with all occurrences of more than N consecutive
!              delimiters removed.
!
!              If OUTPUT is not large enough to hold the
!              compressed string, it is truncated on the right.
!
!              OUTPUT may overwrite INPUT.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the output string length is too short to contain the result
!         of compressing the input string, the result is truncated on
!         the right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Occurrences of more than N consecutive delimiters are removed
!     from the input string as it is copied to the output string.
!     If the output string is not large enough to hold the compressed
!     string, it is truncated on the right.
!
! $ Examples
!
!     Let DELIM = '.', and N = 2. Then
!
!        'ABC...DE.F...',           becomes   'ABC..DE.F..'
!        ' ...........'                       ' ..'
!        '.. ..AB....CD'                      '.. ..AB..CD'
!
!     Let DELIM = ' ', and N = 0. Then
!
!        ' DISK:[USER.  SUB  ]'     becomes   'DISK:[USER.SUB]'
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
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 09-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Added $Exceptions entry #1.
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
!     compress a character_string
!
!-&


!
!
!     Local Variables
!
      integer(ip)      i
      integer(ip)      j
      integer(ip)      count

      integer(ip)      inlen
      integer(ip)      outlen


!
!     Find out how much space there is in the INPUT and OUTPUT strings
!     and initialize the delimiter counter and output place holder.
!
      inlen  = len(input)
      outlen = len(output)

      count  = 0
      j      = 0

      do i = 1, inlen

!
!        Check each character to see if it is a delimiter or not.
!
         if ( input(i:i) .eq. delim ) then

            count = count + 1

!
!           Copy delimiters until enough consecutive delimiters
!           have been accumulated.  When enough consecutive delimiters
!           have accumulated, we no longer copy them.
!
            if ( count .le. n ) then
               j           = j + 1
               output(j:j) = input(i:i)
            end if

         else

!
!           We don't have a delimiter here so we just copy the
!           character and set the delimiter counter to zero.
!
            count       = 0
            j           = j + 1
            output(j:j) = input(i:i)

         end if

         if ( j .eq. outlen ) then
            return
         end if

      end do

!
!     Pad any left over space in the output string with blanks.
!
      if ( j .lt. outlen ) then
         output(j+1:) = ' '
      end if

      return
      end
