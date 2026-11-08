!$Procedure UCASE ( Convert to uppercase )

      subroutine ucase ( in, out )

! $ Abstract
!
!     Convert the characters in a string to uppercase.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASCII
!     CHARACTER
!
! $ Declarations

      implicit none

      character*(*)    in
      character*(*)    out

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input string.
!     OUT        O   Output string, all uppercase.
!
! $ Detailed_Input
!
!     IN       is the input string.
!
! $ Detailed_Output
!
!     OUT      is the output string. This is the input string
!              with all lowercase letters converted to uppercase.
!              Non-letters are not affected.
!
!              OUT may overwrite IN.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the output string length is less than the input string
!         length, the result will be truncated on the right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Convert each lowercase character in IN to uppercase.
!
! $ Examples
!
!     'This is an example'   becomes   'THIS IS AN EXAMPLE'
!     '12345 +-=? > * $ &'             '12345 +-=? > * $ &'
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
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 05-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Added entry #1 in $Exceptions section.
!
!-    SPICELIB Version 1.1.0, 13-MAR-1996 (KRG)
!
!        Removed the calls to the string lexicographic functions.
!
!        Modified the algorithm to use the ICHAR() intrinsic function
!        and some local integer storage for the bases of the lower and
!        upper case letters.
!
!        Added a "FIRST" clause to the code so that the lower and
!        upper case bases and the separation between them are only
!        initialized the first time the subroutine is called rather
!        than every time.
!
!        These changes were made to improve the execution speed of
!        the subroutine
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     convert to uppercase
!
!-&


!
!     Local variables
!
      integer          i
      integer          ich
!
!     Saved variables
!
      integer          shift
      integer          lowa
      integer          lowz

      logical          first

      save             first
      save             shift
      save             lowa
      save             lowz
!
!     Initial Data
!
      data             first / .true. /
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
!     Move the string from IN to OUT. Step through OUT one character
!     at a time, translating letters between 'a' and 'z' to uppercase.
!
      out   = in

      do i = 1, len (out)

         ich = ichar( out(i:i) )

         if ( ( ich .ge. lowa ) .and. ( ich .le. lowz ) ) then
            out(i:i) = char ( ich + shift )
         end if

      end do

      return
      end
