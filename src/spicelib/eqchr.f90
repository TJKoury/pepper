!$Procedure EQCHR (Equivalent characters)

      LOGICAL FUNCTION EQCHR ( A, B )

! $ Abstract
!
!     Return .TRUE. if two given characters are equivalent when the
!     case of the characters is ignored.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      IMPLICIT NONE

      CHARACTER*(1)         A
      CHARACTER*(1)         B

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
      LOGICAL               NECHR
!
!     Range of characters
!
      INTEGER               LSTCHR
      PARAMETER           ( LSTCHR = 255 )
!
!     Local Variables
!
!     The array UVALUE contains the ICHAR values for the upper case
!     version of each character.
!
      INTEGER               UVALUE ( 0 : LSTCHR )
      INTEGER               I
      INTEGER               J

      LOGICAL               FIRST
      SAVE

      DATA FIRST / .TRUE. /


!
!     The first time through the loop we set the upper case values
!     for each of the lower case letters.
!
      IF ( FIRST ) THEN

         DO I = 0, LSTCHR
            UVALUE(I) = I
         END DO

         FIRST = .FALSE.

         UVALUE( ICHAR('a') ) = ICHAR( 'A' )
         UVALUE( ICHAR('b') ) = ICHAR( 'B' )
         UVALUE( ICHAR('c') ) = ICHAR( 'C' )
         UVALUE( ICHAR('d') ) = ICHAR( 'D' )
         UVALUE( ICHAR('e') ) = ICHAR( 'E' )
         UVALUE( ICHAR('f') ) = ICHAR( 'F' )
         UVALUE( ICHAR('g') ) = ICHAR( 'G' )
         UVALUE( ICHAR('h') ) = ICHAR( 'H' )
         UVALUE( ICHAR('i') ) = ICHAR( 'I' )
         UVALUE( ICHAR('j') ) = ICHAR( 'J' )
         UVALUE( ICHAR('k') ) = ICHAR( 'K' )
         UVALUE( ICHAR('l') ) = ICHAR( 'L' )
         UVALUE( ICHAR('m') ) = ICHAR( 'M' )
         UVALUE( ICHAR('n') ) = ICHAR( 'N' )
         UVALUE( ICHAR('o') ) = ICHAR( 'O' )
         UVALUE( ICHAR('p') ) = ICHAR( 'P' )
         UVALUE( ICHAR('q') ) = ICHAR( 'Q' )
         UVALUE( ICHAR('r') ) = ICHAR( 'R' )
         UVALUE( ICHAR('s') ) = ICHAR( 'S' )
         UVALUE( ICHAR('t') ) = ICHAR( 'T' )
         UVALUE( ICHAR('u') ) = ICHAR( 'U' )
         UVALUE( ICHAR('v') ) = ICHAR( 'V' )
         UVALUE( ICHAR('w') ) = ICHAR( 'W' )
         UVALUE( ICHAR('x') ) = ICHAR( 'X' )
         UVALUE( ICHAR('y') ) = ICHAR( 'Y' )
         UVALUE( ICHAR('z') ) = ICHAR( 'Z' )

      END IF

      I     = ICHAR(A)
      J     = ICHAR(B)

      IF ( I .GT. LSTCHR .OR. J .GT. LSTCHR ) THEN
         EQCHR = I .EQ. J
      ELSE
         EQCHR = UVALUE(I) .EQ. UVALUE(J)
      END IF

      RETURN



!$Procedure NECHR (Not Equivalent characters)

      ENTRY NECHR ( A, B )

! $ Abstract
!
!     Return .TRUE. if two given characters are not equivalent if the
!     case of the characters is ignored.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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


      IF ( FIRST ) THEN

         FIRST = .FALSE.

         DO I = 0, LSTCHR
            UVALUE(I) = I
         END DO

         UVALUE( ICHAR('a') ) = ICHAR( 'A' )
         UVALUE( ICHAR('b') ) = ICHAR( 'B' )
         UVALUE( ICHAR('c') ) = ICHAR( 'C' )
         UVALUE( ICHAR('d') ) = ICHAR( 'D' )
         UVALUE( ICHAR('e') ) = ICHAR( 'E' )
         UVALUE( ICHAR('f') ) = ICHAR( 'F' )
         UVALUE( ICHAR('g') ) = ICHAR( 'G' )
         UVALUE( ICHAR('h') ) = ICHAR( 'H' )
         UVALUE( ICHAR('i') ) = ICHAR( 'I' )
         UVALUE( ICHAR('j') ) = ICHAR( 'J' )
         UVALUE( ICHAR('k') ) = ICHAR( 'K' )
         UVALUE( ICHAR('l') ) = ICHAR( 'L' )
         UVALUE( ICHAR('m') ) = ICHAR( 'M' )
         UVALUE( ICHAR('n') ) = ICHAR( 'N' )
         UVALUE( ICHAR('o') ) = ICHAR( 'O' )
         UVALUE( ICHAR('p') ) = ICHAR( 'P' )
         UVALUE( ICHAR('q') ) = ICHAR( 'Q' )
         UVALUE( ICHAR('r') ) = ICHAR( 'R' )
         UVALUE( ICHAR('s') ) = ICHAR( 'S' )
         UVALUE( ICHAR('t') ) = ICHAR( 'T' )
         UVALUE( ICHAR('u') ) = ICHAR( 'U' )
         UVALUE( ICHAR('v') ) = ICHAR( 'V' )
         UVALUE( ICHAR('w') ) = ICHAR( 'W' )
         UVALUE( ICHAR('x') ) = ICHAR( 'X' )
         UVALUE( ICHAR('y') ) = ICHAR( 'Y' )
         UVALUE( ICHAR('z') ) = ICHAR( 'Z' )

      END IF

      I     = ICHAR(A)
      J     = ICHAR(B)

      IF ( I .GT. LSTCHR .OR. J .GT. LSTCHR ) THEN
         NECHR = I .NE. J
      ELSE
         NECHR = UVALUE(I) .NE. UVALUE(J)
      END IF

      RETURN


      END
