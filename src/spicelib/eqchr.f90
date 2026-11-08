!$Procedure EQCHR (Equivalent characters)

      logical function eqchr ( a, b )

! $ Abstract
!
!     Return .TRUE. if two given characters are equivalent when the
!     case of the characters is ignored.
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

      implicit none

      character*(1)         a
      character*(1)         b

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A          I   one of the characters to check
!     B          I   the other character to check
!
!     The function returns .TRUE. if the characters are equivalent
!
! $ Detailed_Input
!
!     A,
!     B        are two characters that are to be compared to see
!              if they are the same letter (although possibly
!              having different case such as 'a' and 'A')
!
! $ Detailed_Output
!
!     The function returns the value .TRUE. if the two input characters
!     are the same or can be made the same by converting both to
!     upper or lower case.
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
!     This is a utility routine for comparing two characters to
!     see if they are the same when converted to upper case. It
!     is particularly useful when writing string analysis routines
!     that should be case insensitive. Instead of writing the
!     expression
!
!        A .EQ. B
!
!     use the expression
!
!        EQCHR ( A, B )
!
!     in all tests of equivalence for characters.
!
! $ Examples
!
!     Suppose you want to determine whether or not two strings
!     are the same if differences in the case of letters are ignored.
!     The following code fragment shows how you can use this routine
!     to check for the equivalence of character strings.
!
!        MORE  = .TRUE.
!        SAME  = .TRUE.
!        L1    =  LEN(STR1)
!        L2    =  LEN(STR2)
!        CHECK = MIN ( L1, L2 )
!
!        DO WHILE ( SAME .AND. MORE )
!
!           SAME = EQCHR( STR1(I:I), STR2(I:I) )
!           I    = I + 1
!           MORE = I .LT. CHECK
!
!        END DO
!
!        IF ( .NOT. SAME ) THEN
!
!           There's nothing to do, we already know the strings
!           are not the same.
!
!        ELSE IF ( L1 .LT. L2 ) THEN
!
!           The only way the strings can be regarded as being equal
!           is if the extra unchecked characters in STR2 are all blank.
!
!           SAME = STR2(I:) .EQ. ' '
!
!        ELSE
!
!           Same test as previous one but with STR1 this time.
!
!           SAME = STR1(I:) .EQ. ' '
!
!        END IF
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 17-SEP-1998 (EDW)
!
!        Replace the UVALUE data statement with a loop to fill
!        UVALUE. The Absoft Mac compiler failed to compile the
!        data statement correctly, and so this function failed
!        to work properly in all situations on the Mac. The
!        corrects the problem and functions on all platforms.
!
!-    SPICELIB Version 1.0.0, 16-MAY-1995 (WLT)
!
!-&


! $ Index_Entries
!
!     Check two characters for case insensitive equality
!
!-&


!
!     Entry points.
!
      logical               nechr
!
!     Range of characters
!
      integer               lstchr
      parameter           ( lstchr = 255 )
!
!     Local Variables
!
!     The array UVALUE contains the ICHAR values for the upper case
!     version of each character.
!
      integer               uvalue ( 0 : lstchr )
      integer               i
      integer               j

      logical               first
      save

      data first / .true. /


!
!     The first time through the loop we set the upper case values
!     for each of the lower case letters.
!
      if ( first ) then

         do i = 0, lstchr
            uvalue(i) = i
         end do

         first = .false.

         uvalue( ichar('a') ) = ichar( 'A' )
         uvalue( ichar('b') ) = ichar( 'B' )
         uvalue( ichar('c') ) = ichar( 'C' )
         uvalue( ichar('d') ) = ichar( 'D' )
         uvalue( ichar('e') ) = ichar( 'E' )
         uvalue( ichar('f') ) = ichar( 'F' )
         uvalue( ichar('g') ) = ichar( 'G' )
         uvalue( ichar('h') ) = ichar( 'H' )
         uvalue( ichar('i') ) = ichar( 'I' )
         uvalue( ichar('j') ) = ichar( 'J' )
         uvalue( ichar('k') ) = ichar( 'K' )
         uvalue( ichar('l') ) = ichar( 'L' )
         uvalue( ichar('m') ) = ichar( 'M' )
         uvalue( ichar('n') ) = ichar( 'N' )
         uvalue( ichar('o') ) = ichar( 'O' )
         uvalue( ichar('p') ) = ichar( 'P' )
         uvalue( ichar('q') ) = ichar( 'Q' )
         uvalue( ichar('r') ) = ichar( 'R' )
         uvalue( ichar('s') ) = ichar( 'S' )
         uvalue( ichar('t') ) = ichar( 'T' )
         uvalue( ichar('u') ) = ichar( 'U' )
         uvalue( ichar('v') ) = ichar( 'V' )
         uvalue( ichar('w') ) = ichar( 'W' )
         uvalue( ichar('x') ) = ichar( 'X' )
         uvalue( ichar('y') ) = ichar( 'Y' )
         uvalue( ichar('z') ) = ichar( 'Z' )

      end if

      i     = ichar(a)
      j     = ichar(b)

      if ( i .gt. lstchr .or. j .gt. lstchr ) then
         eqchr = i .eq. j
      else
         eqchr = uvalue(i) .eq. uvalue(j)
      end if

      return



!$Procedure NECHR (Not Equivalent characters)

      entry nechr ( a, b )

! $ Abstract
!
!     Return .TRUE. if two given characters are not equivalent if the
!     case of the characters is ignored.
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
!     CHARACTER*(1)         A
!     CHARACTER*(1)         B
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A          I   one of the characters to check
!     B          I   the other character to check
!
!     The function returns .TRUE. if the characters are not equivalent
!
! $ Detailed_Input
!
!     A,
!     B        are two characters that are to be compared to see
!              if they are different letters. Letters that have
!              the same value when converted to uppercase are
!              considered to be equivalent.
!
! $ Detailed_Output
!
!     The function returns the value .FALSE. if the two input characters
!     are the same or can be made the same by converting both to
!     upper or lower case. Otherwise it returns .TRUE.
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
!     This routine simply determines the truth value of .NOT. EQCHR.
!     See the entry point EQCHR for a discussion of that function.
!
! $ Examples
!
!     Suppose you want to determine whether or not two strings
!     are the same up to differences in case. The following
!     code fragment shows how you can use this routine to check
!     for the equivalence of character strings.
!
!        MORE  = .TRUE.
!        SAME  = .TRUE.
!        L1    =  LEN(STR1)
!        L2    =  LEN(STR2)
!        CHECK = MIN ( L1, L2 )
!
!        DO WHILE ( SAME .AND. MORE )
!
!           IF ( NECHR(STR1(I:I),STR2(I:I) ) THEN
!              SAME = .FALSE.
!           END IF
!
!           I    = I + 1
!           MORE = I .LT. CHECK
!
!        END DO
!
!        IF ( .NOT. SAME ) THEN
!
!           There's nothing to do, we already know the strings
!           are not the same.
!
!        ELSE IF ( L1 .LT. L2 ) THEN
!
!           The only way the strings can be regarded as being equal
!           is if the extra unchecked characters in STR2 are all blank.
!
!           SAME = STR2(I:) .EQ. ' '
!
!        ELSE
!
!           Same test as previous one but with STR1 this time.
!
!           SAME = STR1(I:) .EQ. ' '
!
!        END IF
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 17-SEP-1998 (EDW)
!
!        Replace the UVALUE data statement with a loop to fill
!        UVALUE. The Absoft Mac compiler failed to compile the
!        data statement correctly, and so this function failed
!        to work properly in all situations on the Mac. The
!        corrects the problem and functions on all platforms.
!
!-    SPICELIB Version 1.0.0, 16-MAY-1995 (WLT)
!
!-&


! $ Index_Entries
!
!     Check two characters for case insensitive not equal
!
!-&


      if ( first ) then

         first = .false.

         do i = 0, lstchr
            uvalue(i) = i
         end do

         uvalue( ichar('a') ) = ichar( 'A' )
         uvalue( ichar('b') ) = ichar( 'B' )
         uvalue( ichar('c') ) = ichar( 'C' )
         uvalue( ichar('d') ) = ichar( 'D' )
         uvalue( ichar('e') ) = ichar( 'E' )
         uvalue( ichar('f') ) = ichar( 'F' )
         uvalue( ichar('g') ) = ichar( 'G' )
         uvalue( ichar('h') ) = ichar( 'H' )
         uvalue( ichar('i') ) = ichar( 'I' )
         uvalue( ichar('j') ) = ichar( 'J' )
         uvalue( ichar('k') ) = ichar( 'K' )
         uvalue( ichar('l') ) = ichar( 'L' )
         uvalue( ichar('m') ) = ichar( 'M' )
         uvalue( ichar('n') ) = ichar( 'N' )
         uvalue( ichar('o') ) = ichar( 'O' )
         uvalue( ichar('p') ) = ichar( 'P' )
         uvalue( ichar('q') ) = ichar( 'Q' )
         uvalue( ichar('r') ) = ichar( 'R' )
         uvalue( ichar('s') ) = ichar( 'S' )
         uvalue( ichar('t') ) = ichar( 'T' )
         uvalue( ichar('u') ) = ichar( 'U' )
         uvalue( ichar('v') ) = ichar( 'V' )
         uvalue( ichar('w') ) = ichar( 'W' )
         uvalue( ichar('x') ) = ichar( 'X' )
         uvalue( ichar('y') ) = ichar( 'Y' )
         uvalue( ichar('z') ) = ichar( 'Z' )

      end if

      i     = ichar(a)
      j     = ichar(b)

      if ( i .gt. lstchr .or. j .gt. lstchr ) then
         nechr = i .ne. j
      else
         nechr = uvalue(i) .ne. uvalue(j)
      end if

      return


      end
