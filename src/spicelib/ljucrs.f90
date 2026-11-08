!$Procedure LJUCRS ( Left-justify, Uppercase, Compress )

      subroutine ljucrs ( n, input, output )

! $ Abstract
!
!     Left-justify, uppercase, and space-compress a character string.
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

      integer             n
      character*(*)       input
      character*(*)       output

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     N          I      Maximum consecutive occurrences of space.
!     INPUT      I      Input string.
!     OUTPUT     O      Output string.
!
! $ Detailed_Input
!
!     N        is the maximum number of consecutive occurrences
!              of space that will be allowed to remain in the
!              output string.
!
!     INPUT    is the input string.
!
! $ Detailed_Output
!
!     OUTPUT   is the output string. This is the input string that
!              left-justified and with all occurrences of more than
!              N consecutive spaces removed.
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
! $ Files
!
!     None.
!
! $ Particulars
!
!     The string is left-justified and uppercased. Occurrences of more
!     than N consecutive spaces are removed from the input string as it
!     is copied to the output string. If the output string is not large
!     enough to hold the compressed string, it is truncated on the
!     right.
!
! $ Examples
!
!     Let N = 1. Then
!
!         ' Abc  DE F  ',           becomes    'ABC DE F',
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
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
!
!-&


! $ Index_Entries
!
!     compress uppercase left-justify a character_string
!
!-&


!
!
!     Local Variables
!
      integer               i
      integer               j
      integer               count

      integer               inlen
      integer               outlen

      integer               ich

!
!     Saved variables
!
      integer               shift
      integer               lowa
      integer               lowz

      logical               first

      save                  first
      save                  shift
      save                  lowa
      save                  lowz

!
!     Initial Data
!
      data                  first / .true. /

!
!     Do some set up stuff the first time through so that we do not
!     need to reinitialize the boundary values used for comparisons
!     and the shift on each call.
!
      if ( first ) then
         first = .false.
         lowa  = ichar ('a')
         lowz  = ichar ('z')
         shift = ichar ('A') - lowa
      end if

!
!     Find out how much space there is in the INPUT and OUTPUT strings
!     and initialize the space counter and output place holder.
!
      inlen  = len(input)
      outlen = len(output)

      count  = 0
      j      = 0

      do i = 1, inlen

!
!        Skip leading spaces.
!
         if ( j .eq. 0 .and. input(i:i) .eq. ' ' ) then

!
!           Another leading space. Skip it.
!

         else

!
!           Check this character to see if it is a space or not.
!
            if ( input(i:i) .eq. ' ' ) then

               count = count + 1

!
!              Copy spaces until enough consecutive spaces
!              have been accumulated. When enough consecutive spaces
!              have accumulated, we no longer copy them.
!
               if ( count .le. n ) then
                  j           = j + 1
                  output(j:j) = input(i:i)
               end if

            else

!
!              We don't have a space here. Set the space counter to
!              zero.
!
               count       = 0

!
!              Copy this character while swapping lowercase with upper
!              case along the way.
!
               j           = j + 1

               ich = ichar( input(i:i) )

               if ( ( ich .ge. lowa ) .and. ( ich .le. lowz ) ) then
                  output(j:j) = char ( ich + shift )
               else
                  output(j:j) = input(i:i)
               end if

            end if

            if ( j .eq. outlen ) then
               return
            end if

         end if

      end do

!
!     Pad any left over space in the output string with blanks. Note
!     that if the input string was blank, J will be zero at this
!     point and the case below will set the whole output string to
!     blank.
!
      if ( j .lt. outlen ) then
         output(j+1:) = ' '
      end if

      return
      end
