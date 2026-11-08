!$Procedure LASTNB ( Last non-blank character )

      integer function  lastnb ( string )

! $ Abstract
!
!     Return the index of the last non-blank character in
!     a character string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASCII
!     CHARACTER
!     SEARCH
!
! $ Declarations

      implicit none

      character*(*)    string

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   Input character string.
!
!     The function returns the index of the last non-blank character in
!     STRING.
!
! $ Detailed_Input
!
!     STRING   is the input character string.
!
! $ Detailed_Output
!
!     The function returns the index of the last non-blank character in
!     the input string. If there are no non-blank characters in the
!     string, LASTNB is zero.
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
!     If the string is blank, return zero. Otherwise, step through
!     the string one character at a time until something other than
!     a blank is found. Return the index of that something within
!     the string.
!
! $ Examples
!
!     The following examples illustrate the use of LASTNB.
!
!           LASTNB ( 'ABCDE'              )   = 5
!           LASTNB ( 'AN EXAMPLE'         )   = 10
!           LASTNB ( 'AN EXAMPLE        ' )   = 10
!           LASTNB ( '                  ' )   = 0
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
!-    SPICELIB Version 2.1.0, 08-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
!
!        Modified the comparison to use integer values and the ICHAR()
!        function. This improves the performance of the subroutine.
!
!-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.1, 07-DEC-1990 (IMU)
!
!        Corrected a misprint in the description of LASTNB.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     last non-blank character
!
!-&


!
!     Local parameters
!
      integer               ispace
      parameter           ( ispace = 32 )

!
!     Local variables
!
      integer          i

!
!     Just like it says in the header.
!
      if ( string .eq. ' ' ) then
         lastnb = 0

      else
         do i = len (string), 1, -1

            if ( ichar(string(i:i)) .ne. ispace ) then
               lastnb = i
               return
            end if

         end do
      end if

      return
      end
