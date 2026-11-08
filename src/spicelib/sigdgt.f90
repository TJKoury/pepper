!$Procedure SIGDGT ( Retain significant digits )

      subroutine sigdgt ( in, out )

! $ Abstract
!
!     Retain only the significant digits in a numeric string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     PARSING
!
! $ Declarations

      implicit none

      character*(*)    in
      character*(*)    out

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input numeric string.
!     OUT        O   Numeric string, with insignificant digits removed.
!
! $ Detailed_Input
!
!     IN       is a numeric string.
!
! $ Detailed_Output
!
!     OUT      is the same numeric string with insignificant
!              zeros and spaces removed. The special case '.000...'
!              becomes just '0'. OUT may overwrite IN. If the
!              output string is too long, it is truncated on the
!              right.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If IN is a non-numeric string, the contents of OUT are
!         unpredictable.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     There are only two interesting cases:
!
!        1) There is a decimal point and an exponent immediately
!           preceded by zero ('...0E', '...0D', '...0e', '...0d')
!           or by a space ('... E', '... D', '... e', '... d').
!
!        2) There is a decimal point and no exponent, and the last non-
!           blank character is a zero ('...0').
!
!     In each of these cases, go to the zero in question, and step
!     backwards until you find something other than a blank or a zero.
!
!     Finally, remove all leading spaces, and all occurrences of more
!     than one consecutive space within the string.
!
! $ Examples
!
!     The following examples illustrate the use of SIGDGT.
!
!     '0.123456000000D-04'        becomes     '0.123456D-04'
!     '  -9.2100000000000'                    '-9.21'
!     '       13'                             '13'
!     '    00013'                             '00013'
!     ' .314 159 265 300 000 e1'              '.314 159 265 3e1'
!     '   123    45     6'                    '123 45 6'
!     '  .000000000'                          '0'
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
!     H.A. Neilan        (JPL)
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
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     retain significant digits
!
!-&


! $ Revisions
!
!-     Beta Version 1.3.0, 21-MAR-1989 (WLT)
!
!         Previous fix was unbelievably bad, very buggy. This
!         has been fixed along with other bugs and non-standard
!         code has been removed.
!
!-     Beta Version 1.2.0, 28-FEB-1989 (WLT)
!
!         Reference to INSSUB replaced by SUFFIX
!
!-     Beta Version 1.1.1, 17-FEB-1989 (HAN) (NJB)
!
!         Declaration of the unused function ISRCHC removed.
!
!-&


!
!     SPICELIB functions
!
      integer               frstnb
      integer               lastnb
      integer               cpos

!
!     Local variables
!
      integer               begin
      integer               end
      character*1           lchar
      integer               zero
      integer               i
      integer               j
      integer               k
      integer               l


!
!     Find the first and last non-blank characters in the string.
!
      begin  = max ( 1, frstnb (in) )
      end    = max ( 1, lastnb (in) )
      lchar  = ' '
!
!     Trivial case.
!
      if ( begin .eq. end ) then

         out(1:1) = in(begin:begin)

         if ( len(out) .gt. 1 ) out(2:) = ' '

!
!     If there is no decimal point, all zeros are significant.
!
      else if ( index (in, '.') .eq. 0 ) then

         l      = 1
         k      = begin

         do while (       ( l .le. len(out) ) &
                    .and. ( k .le. end      ) )

            out(l:l) = in(k:k)

!
!           Don't increment L if the last item copied was a space
!           (we don't want to copy extra spaces).
!
            if ( in(k:k) .ne. ' ' .or. lchar .ne. ' ' ) &
            l        = l+1
            lchar    = in(k:k)
            k        = k+1

         end do

         if ( l .le. len(out) ) out(l:) = ' '

      else

!
!        Is there is a decimal point and an exponent immediately
!        preceded by zero ('...0E', '...0D', '...0e', '...0d') or
!        by a space ('... E', '... D', '... e', '... d')?
!
         zero = index ( in, '0E' )

         if ( zero .eq. 0 )    zero = index ( in, '0D' )
         if ( zero .eq. 0 )    zero = index ( in, '0e' )
         if ( zero .eq. 0 )    zero = index ( in, '0d' )
         if ( zero .eq. 0 )    zero = index ( in, ' E' )
         if ( zero .eq. 0 )    zero = index ( in, ' D' )
         if ( zero .eq. 0 )    zero = index ( in, ' e' )
         if ( zero .eq. 0 )    zero = index ( in, ' d' )

!
!        Begin there, and move toward the front of the string until
!        something other than a blank or a zero is encountered. Then
!        remove the superfluous characters.
!
         if ( zero .gt. 0 ) then

            j = zero + 1
            i = zero

            do while ( in(i:i) .eq. '0'  .or. in(i:i) .eq. ' ' )
               i = i - 1
            end do

            l = 1
            k = begin

            do while (       ( l .le. len(out) ) &
                       .and. ( k .le. i        ) )

               out(l:l) = in(k:k)

!
!              Don't increment L if the last item copied was a space.
!
               if ( in(k:k) .ne. ' ' .or. lchar .ne. ' ' ) &
               l        = l+1
               lchar    = in(k:k)
               k        = k+1

            end do

            k = j

            do while (       ( l .le. len(out) ) &
                       .and. ( k .le. end      ) )

               out(l:l) = in(k:k)

!
!              Increment L only if we don't have two consecutive
!              spaces.
!
               if ( in(k:k) .ne. ' ' .or. lchar .ne. ' ' ) &
               l        = l+1
               lchar    = in(k:k)
               k        = k+1

            end do

            if ( l .le. len(out) ) out(l:) = ' '
!
!
!        Is there is a decimal point and no exponent, and is the last
!        non-blank character a zero ('...0')? Then truncate the string
!        after the last character that is neither a blank nor a zero.
!
         else if (       in(end:end)            .eq. '0' &
                   .and. cpos ( in, 'EeDd', 1 ) .eq.  0 ) then

            i = end

            do while ( in(i:i) .eq. '0' .or. in(i:i) .eq. ' ' )
               i = i - 1
            end do

            l = 1
            k = begin

            do while (       ( l .le. len(out) ) &
                       .and. ( k .le. i        ) )

               out(l:l) = in(k:k)

!
!              Increment L only if we don't have two consecutive
!              spaces.
!
               if ( in(k:k) .ne. ' ' .or. lchar .ne. ' ' ) &
               l        = l+1
               lchar    = in(k:k)
               k        = k+1

            end do

            if ( l .le. len(out) ) out(l:) = ' '

         else

            l = 1
            k = begin

            do while (       ( l .le. len(out) ) &
                       .and. ( k .le. end      ) )

               out(l:l) = in(k:k)

!
!              Increment L only if we don't have two consecutive spaces.
!
               if ( in(k:k) .ne. ' ' .or. lchar .ne. ' ' ) &
               l        = l+1
               lchar    = in(k:k)
               k        = k+1

            end do

            if ( l .le. len(out) ) out(l:) = ' '

         end if

      end if

!
!     Special case. The string '.0000....' reduces to '.' after the
!     zeros are removed.
!
      if ( out .eq. '.' ) then
         out = '0'
      end if

      return
      end
