!$Procedure SIGDGT ( Retain significant digits )

      SUBROUTINE SIGDGT ( IN, OUT )

! $ Abstract
!
!     Retain only the significant digits in a numeric string.
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
!     PARSING
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    IN
      CHARACTER*(*)    OUT

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
      INTEGER               FRSTNB
      INTEGER               LASTNB
      INTEGER               CPOS

!
!     Local variables
!
      INTEGER               BEGIN
      INTEGER               END
      CHARACTER*1           LCHAR
      INTEGER               ZERO
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L


!
!     Find the first and last non-blank characters in the string.
!
      BEGIN  = MAX ( 1, FRSTNB (IN) )
      END    = MAX ( 1, LASTNB (IN) )
      LCHAR  = ' '
!
!     Trivial case.
!
      IF ( BEGIN .EQ. END ) THEN

         OUT(1:1) = IN(BEGIN:BEGIN)

         IF ( LEN(OUT) .GT. 1 ) OUT(2:) = ' '

!
!     If there is no decimal point, all zeros are significant.
!
      ELSE IF ( INDEX (IN, '.') .EQ. 0 ) THEN

         L      = 1
         K      = BEGIN

         DO WHILE (       ( L .LE. LEN(OUT) ) &
                    .AND. ( K .LE. END      ) )

            OUT(L:L) = IN(K:K)

!
!           Don't increment L if the last item copied was a space
!           (we don't want to copy extra spaces).
!
            IF ( IN(K:K) .NE. ' ' .OR. LCHAR .NE. ' ' ) &
            L        = L+1
            LCHAR    = IN(K:K)
            K        = K+1

         END DO

         IF ( L .LE. LEN(OUT) ) OUT(L:) = ' '

      ELSE

!
!        Is there is a decimal point and an exponent immediately
!        preceded by zero ('...0E', '...0D', '...0e', '...0d') or
!        by a space ('... E', '... D', '... e', '... d')?
!
         ZERO = INDEX ( IN, '0E' )

         IF ( ZERO .EQ. 0 )    ZERO = INDEX ( IN, '0D' )
         IF ( ZERO .EQ. 0 )    ZERO = INDEX ( IN, '0e' )
         IF ( ZERO .EQ. 0 )    ZERO = INDEX ( IN, '0d' )
         IF ( ZERO .EQ. 0 )    ZERO = INDEX ( IN, ' E' )
         IF ( ZERO .EQ. 0 )    ZERO = INDEX ( IN, ' D' )
         IF ( ZERO .EQ. 0 )    ZERO = INDEX ( IN, ' e' )
         IF ( ZERO .EQ. 0 )    ZERO = INDEX ( IN, ' d' )

!
!        Begin there, and move toward the front of the string until
!        something other than a blank or a zero is encountered. Then
!        remove the superfluous characters.
!
         IF ( ZERO .GT. 0 ) THEN

            J = ZERO + 1
            I = ZERO

            DO WHILE ( IN(I:I) .EQ. '0'  .OR. IN(I:I) .EQ. ' ' )
               I = I - 1
            END DO

            L = 1
            K = BEGIN

            DO WHILE (       ( L .LE. LEN(OUT) ) &
                       .AND. ( K .LE. I        ) )

               OUT(L:L) = IN(K:K)

!
!              Don't increment L if the last item copied was a space.
!
               IF ( IN(K:K) .NE. ' ' .OR. LCHAR .NE. ' ' ) &
               L        = L+1
               LCHAR    = IN(K:K)
               K        = K+1

            END DO

            K = J

            DO WHILE (       ( L .LE. LEN(OUT) ) &
                       .AND. ( K .LE. END      ) )

               OUT(L:L) = IN(K:K)

!
!              Increment L only if we don't have two consecutive
!              spaces.
!
               IF ( IN(K:K) .NE. ' ' .OR. LCHAR .NE. ' ' ) &
               L        = L+1
               LCHAR    = IN(K:K)
               K        = K+1

            END DO

            IF ( L .LE. LEN(OUT) ) OUT(L:) = ' '
!
!
!        Is there is a decimal point and no exponent, and is the last
!        non-blank character a zero ('...0')? Then truncate the string
!        after the last character that is neither a blank nor a zero.
!
         ELSE IF (       IN(END:END)            .EQ. '0' &
                   .AND. CPOS ( IN, 'EeDd', 1 ) .eq.  0 ) THEN

            I = END

            DO WHILE ( IN(I:I) .EQ. '0' .OR. IN(I:I) .EQ. ' ' )
               I = I - 1
            END DO

            L = 1
            K = BEGIN

            DO WHILE (       ( L .LE. LEN(OUT) ) &
                       .AND. ( K .LE. I        ) )

               OUT(L:L) = IN(K:K)

!
!              Increment L only if we don't have two consecutive
!              spaces.
!
               IF ( IN(K:K) .NE. ' ' .OR. LCHAR .NE. ' ' ) &
               L        = L+1
               LCHAR    = IN(K:K)
               K        = K+1

            END DO

            IF ( L .LE. LEN(OUT) ) OUT(L:) = ' '

         ELSE

            L = 1
            K = BEGIN

            DO WHILE (       ( L .LE. LEN(OUT) ) &
                       .AND. ( K .LE. END      ) )

               OUT(L:L) = IN(K:K)

!
!              Increment L only if we don't have two consecutive spaces.
!
               IF ( IN(K:K) .NE. ' ' .OR. LCHAR .NE. ' ' ) &
               L        = L+1
               LCHAR    = IN(K:K)
               K        = K+1

            END DO

            IF ( L .LE. LEN(OUT) ) OUT(L:) = ' '

         END IF

      END IF

!
!     Special case. The string '.0000....' reduces to '.' after the
!     zeros are removed.
!
      IF ( OUT .EQ. '.' ) THEN
         OUT = '0'
      END IF

      RETURN
      END
