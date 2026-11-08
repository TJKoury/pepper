!$Procedure NECHR (Not Equivalent characters)

      logical function nechr ( a, b )

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


      use pepper_kinds, only: ip
      implicit none
      character(1)          a
      character(1)          b
      integer(ip)           lstchr
      parameter           ( lstchr = 255 )
      integer(ip)           i
      integer(ip)           j
      integer(ip), parameter :: uvalue ( 0 : lstchr ) = &
         [ ( merge( i - ( iachar('a') - iachar('A') ), i, &
                    i >= iachar('a') .and. i <= iachar('z') ), i = 0, lstchr ) ]


      i     = ichar(a)
      j     = ichar(b)

      if ( i > lstchr .or. j > lstchr ) then
         nechr = i /= j
      else
         nechr = uvalue(i) /= uvalue(j)
      end if

      return


      end
