!$Procedure SAMSBI (Same substrings, case insensitive)

      logical function samsbi ( str1, b1, e1,  str2, b2, e2 )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Determine whether or not two substrings are the same up to
!     case.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      implicit none

      character(*)          str1
      integer(ip)           b1
      integer(ip)           e1
      character(*)          str2
      integer(ip)           b2
      integer(ip)           e2

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     STR1       I   A string
!     B1         I   Beginning of a substring in STR1
!     E1         I   End of s substring in STR1
!     STR2       I   A second string
!     B2         I   The beginning of a substring in STR2
!     E2         I   The end  of s substring in STR2
!
!     The function returns .TRUE. if the substrings are identical
!     up to case.
!
! $ Detailed_Input
!
!     STR1     is a character string
!
!     B1       are integers giving the beginning and ending of a
!     E1       substring in STR1
!
!     STR2     is a character string
!
!     B2       are integers giving the beginning and ending of a
!     E2       substring in STR2
!
! $ Detailed_Output
!
!     The function returns .TRUE. if the two substrings STR(B1:E1) and
!     STR(B2:E2) have the same length and the same characters up to
!     case.
!
!     If any of the indices B1, E1, B2, E2 are out of range or out
!     of order the function returns .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If any of the B1, E1, B2, E2 are out of range or if an
!         ending substring index is before a beginning substring
!         index, the function returns false.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is a macro for comparing two substrings of
!     strings and handles all of the bounds checking to avoid
!     out of range errors with string indices.
!
! $ Examples
!
!     Suppose a string contains a number of occurrences of some
!     particular substring in sequence and that you need to locate
!     the first character that is out of this sequence or the
!     end of the string.
!
!     If one ignores boundary constraints this can easily be
!     coded as shown here: We assume the particular substring is
!
!     '/beg'
!
!        B = 1
!        E = B + LEN('/beg' )
!
!        DO WHILE (       E           .LE. LEN(STR)
!                   .AND. STRING(B:E) .EQ. '/beg' )
!
!           B = B + LEN('/beg')
!           E = E + LEN('/beg')
!
!        END DO
!
!        IF ( B .LT. LEN(STR) ) THEN
!
!           we've found the start of a substring of interest
!
!        ELSE
!
!           there is no substring to find.
!
!        END IF
!
!     Unfortunately, you can't rely upon FORTRAN to check the boundary
!     condition: E .LE. LEN(STR) and skip the second test if the first
!     condition if false. As a result you can get an out of range
!     error.
!
!     Instead you could code:
!
!     B = 1
!     E = B + LEN('/beg')
!
!     IF ( E .LE. LEN(STR) ) THEN
!        ALIKE = STRINB(B:E) .EQ. '/beg'
!     ELSE
!        ALIKE = .FALSE.
!     END IF
!
!     DO WHILE ( ALIKE )
!
!           B = B + LEN('/beg')
!           E = E + LEN('/beg')
!
!        IF ( E .LE. LEN(STR) ) THEN
!           ALIKE = STRINB(B:E) .EQ. '/beg'
!        ELSE
!           ALIKE = .FALSE.
!        END IF
!
!     END DO
!
!
!     However, this is code is far more effort. Using this routine
!     you can make a much simpler block of code.
!
!     B = 1
!     E = B + LEN('/beg' )
!
!     DO WHILE ( SAMSBI(STR,B,E, '/beg',1,4 ) )
!
!        B = B + LEN('/beg')
!        E = E + LEN('/beg')
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 31-MAR-1995 (WLT)
!
!-&


! $ Index_Entries
!
!     Check case insensitive equality of two substrings.
!
!-&


      logical               nechr

      integer(ip)           i
      integer(ip)           j



      if (      ( e1      < b1           ) &
           .or. ( e2      < b2           ) &
           .or. ( b1      < 1            ) &
           .or. ( b2      < 1            ) &
           .or. ( e1      > len(str1)    ) &
           .or. ( e2      > len(str2)    ) &
           .or. ( e1 - b1 /= e2 - b2      )  ) then

         samsbi = .false.
         return

      end if

      j      =  b2
      samsbi = .false.

      do i = b1, e1
         if ( nechr( str1(i:i), str2(j:j) ) ) then
            return
         end if
         j = j + 1
      end do

      samsbi = .true.

      return
      end
