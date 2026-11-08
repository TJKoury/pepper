!$Procedure INTSTR  ( Integer to character string )

      subroutine intstr ( number, string )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Convert an integer to an equivalent character string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ALPHANUMERIC
!     CONVERSION
!
! $ Declarations

      implicit none

      integer(ip)           number
      character(*)          string

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NUMBER     I   Integer to be converted.
!     STRING     O   Equivalent character string, left justified.
!
! $ Detailed_Input
!
!     NUMBER   is the integer to be converted into a character string.
!
! $ Detailed_Output
!
!     STRING   is the character string representing the integer NUMBER.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the output character string is not large enough to
!         contain the entire character string produced, the output
!         character string will be truncated on the right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine will convert a signed integer into an equivalent
!     decimal character string representation. The decimal digits of
!     the integer NUMBER are found by repeated applications of
!     "modulus" and division operations.
!
! $ Examples
!
!     The following argument values illustrate the use of INTSTR.
!
!         NUMBER        STRING
!         ------------  ---------------------
!          1            '-1'
!         -1            '-1'
!          223          '223'
!         -32           '-32'
!          0            '0'
!          2147483647   '2147483647'   ( Maximum 32 bit integer )
!         -2147483647   '-2147483647'  ( Minimum 32 bit integer + 1 )
!         -2147483647   '-2147483648'  ( Minimum 32 bit integer )
!
! $ Restrictions
!
!     1)  This routine assumes that all signed integers will fit into a
!         character string with LINLEN or fewer digits. See the
!         parameter LINLEN below for the current value.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     H.A. Neilan        (JPL)
!     M.J. Spencer       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.2.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.1.0, 11-MAY-1993 (HAN) (MJS)
!
!        DATA statement came before the SAVE statement. This is
!        a violation of the ANSI Standard. It is now the other way
!        around.
!
!-    SPICELIB Version 2.0.0, 14-OCT-1992 (KRG)
!
!        The routine was rewritten to fix a bug concerning the minimum
!        representable integer.
!
!        This routine used to negate a negative number before it began
!        generating its digits. This was a bad thing to do, because on
!        many machines the minimum representable integer and the
!        maximum representable integer have the following relationship:
!
!           ABS( minimum integer ) = 1 + ABS( maximum integer ).
!
!        Changing the sign of a negative number before converting it
!        to a character string would cause a program to crash if it
!        were attempting to convert the minimum representable integer
!        into a character string.
!
!-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.1, 07-DEC-1990 (WLT)
!
!        References to the old name INT2CH were removed and
!        an exception added to that section.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT)
!
!-&


! $ Index_Entries
!
!     convert integer to character string
!
!-&


!
!     Local Parameters
!
      integer(ip)           base
      parameter           ( base = 10 )

      integer(ip)           strlen
      parameter           ( strlen = 80 )
!
!     Local variables
!
      character(strlen)     tmpstr

      character(1)          digits(0:9)

      integer(ip)           tmpnum
      integer(ip)           result
      integer(ip)           remndr

      integer(ip)           i

!
!    Saved values
!
      save digits


      data                  digits / &
                                     '0', '1', '2', '3', &
                                     '4', '5', '6', '7', &
                                     '8', '9' &
                                   /



!
!     The digits are generated in reverse order, so we fill the
!     character string in reverse order, from `right' to `left',
!     so that the digits are in the correct order when we are
!     done converting the integer. This is to avoid reversing the
!     character string before returning. The output character
!     string is then left justified upon exit.
!
!     Make a copy of the input so that it will not be modified.
!
      tmpnum = number
!
!     Initialize the temporary character buffer used to store the
!     character string as it is generated to blanks.
!
      tmpstr = ' '
!
!     We need to do different things for the cases where the number to
!     be converted is positive, negative, or zero. ( Actually, the
!     positive case and the zero case are the same, but since we can
!     test for integer zero exactly it will save a few arithmetic
!     operations if we treat it as a special case. ) The case for a
!     negative number is the only one which truly might cause problems,
!     because ABS(minimum integer) may equal ABS(maximum integer) + 1.
!     For 32 bit numbers, INTMIN = -214748368 and INTMAX = 214748367.
!     You should be able to see the repercussions of this.
!
      i = len( tmpstr ) + 1
      if ( tmpnum < 0 ) then
!
!        Collect all of the digits in the string.
!
         do while ( tmpnum /= 0 )

            i      = i - 1
            result = tmpnum / base
            remndr = result * base - tmpnum
            tmpnum = result

            tmpstr (i:i) = digits(remndr)

         end do
!
!        Put the minus sign in place.
!
         i           = i - 1
         tmpstr(i:i) = '-'

      else if ( tmpnum > 0 ) then
!
!        Collect all of the digits in the string.
!
         do while ( tmpnum /= 0 )

            i      = i - 1
            result = tmpnum / base
            remndr = tmpnum - result * base
            tmpnum = result

            tmpstr (i:i) = digits(remndr)

         end do

      else
!
!        Treat zero as a special case, because it's easier.
!
         i           = i - 1
         tmpstr(i:i) = digits(0)

      end if
!
!     Set the value of the output string before returning. Let the
!     Fortran string equals deal with the left justification, and the
!     truncation on the right if the string STRING is not long enough
!     to contain all of the characters necessary.
!
      string = tmpstr(i:len(tmpstr))

      return
      end
