!$Procedure SAMCHI ( Same character --- case insensitive )

      logical function samchi ( str1, l1, str2, l2 )

! $ Abstract
!
!     Determine if two characters from different strings are the
!     same when the case of the characters is ignored.
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

      character*(*)         str1
      integer               l1
      character*(*)         str2
      integer               l2

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STR1       I   A character string
!     L1         I   The location (index) of a character in STR1
!     STR2       I   A character string
!     L2         I   The location (index) of a character in STR2
!
!     The function returns .TRUE. if the two characters are the
!     same up to case.
!
! $ Detailed_Input
!
!     STR1     is a character string
!
!     L1       is the location (index) of a character in STR1
!
!     STR2     is a character string
!
!     L2       is the location (index) of a character in STR2
!
! $ Detailed_Output
!
!     The function returns .TRUE. if the characters STR1(L1:L1) and
!     STR2(L2:L2) are the same when the case of the characters is
!     ignored.
!
!     If the characters are different or L1 or L2 is out of range the
!     function returns .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If either L1 or L2 is out of range the function returns
!         .FALSE.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is a utility function for determining whether or not
!     two characters in different strings are the same up to case.
!     This function is intended for situation in which you need
!     to search two strings for a match (or mismatch).
!
! $ Examples
!
!     Often you need to scan through two string comparing
!     character by character until a case insensitive mismatch
!     occurs. The usual way to code this is
!
!        DO WHILE (        L1 .LE. LEN(STR1)
!                    .AND. L2 .LE. LEN(STR2)
!                    .AND. EQCHR( STR1(L1:L1),STR2(L2:L2) ) )
!
!           L1 = L1 + 1
!           L2 = L2 + 1
!
!        END DO
!
!        Check L1, L2 to make sure we are still in range, etc.
!
!     The problem with this loop is that even though the check to make
!     sure that L1 and L2 are in range is performed, FORTRAN may
!     go ahead and compute the equality condition even though one of the
!     first two steps failed. This can lead to out of range errors
!     and possible halting of your program depending upon how
!     the routine is compiled. An alternative way to code this is
!
!        IF ( L1 .LE. LEN(STR1) .AND. L2 .LE. LEN(STR2) ) THEN
!           ALIKE = EQCHR( STR1(L1:L1),STR2(L2:L2) )
!        ELSE
!           ALIKE = .FALSE.
!        END IF
!
!        DO WHILE ( ALIKE )
!
!           L1 = L1 + 1
!           L2 = L2 + 1
!
!           IF ( L1 .LE. LEN(STR1) .AND. L2 .LE. LEN(STR2) ) THEN
!              ALIKE = EQCHR( STR1(L1:L1), STR2(L2:L2) )
!           ELSE
!              ALIKE = .FALSE.
!           END IF
!        END DO
!
!     However this is a much more complicated section of code. This
!     routine allows you to code the above loops as:
!
!
!        DO WHILE ( SAMCHI ( STR1,L1, STR2,L2 )  )
!           L1 = L1 + 1
!           L2 = L2 + 1
!        END DO
!
!     The boundary checks are automatically performed and out
!     of range errors are avoided.
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
!-    SPICELIB Version 1.0.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 31-MAR-1995 (WLT)
!
!-&


! $ Index_Entries
!
!     Check two character substrings for case insensitive equal
!
!-&


!
!     Spicelib Functions
!
      logical               eqchr


      if (      l1 .lt. 1 &
           .or. l2 .lt. 1 &
           .or. l1 .gt. len(str1) &
           .or. l2 .gt. len(str2)  ) then

         samchi = .false.
         return
      end if

      samchi = eqchr( str1(l1:l1), str2(l2:l2) )
      return

      end
