!$Procedure MATCHW ( Match string against wildcard template )

      LOGICAL FUNCTION MATCHW ( STRING, TEMPL, WSTR, WCHR )

! $ Abstract
!
!     Determine whether a string is matched by a template containing
!     wild cards.
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
!     COMPARE
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    STRING
      CHARACTER*(*)    TEMPL
      CHARACTER*1      WSTR
      CHARACTER*1      WCHR

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   String to be tested.
!     TEMPL      I   Template (with wild cards) to test against STRING.
!     WSTR       I   Wild string token.
!     WCHR       I   Wild character token.
!
!     The function returns .TRUE. if STRING matches TEMPL and otherwise
!     returns .FALSE.
!
! $ Detailed_Input
!
!     STRING   is the input character string to be tested for a match
!              against the input template. Leading and trailing blanks
!              are ignored.
!
!     TEMPL    is the input template to be tested for a match against
!              the input string. TEMPL may contain wild cards. Leading
!              and trailing blanks are ignored.
!
!     WSTR     is the wild string token used in the input template. The
!              wild string token may represent from zero to any number
!              of characters.
!
!     WCHR     is the wild character token used in the input template.
!              The wild character token represents exactly one
!              character.
!
! $ Detailed_Output
!
!     The function returns .TRUE. when the input string matches the
!     input template, and .FALSE. otherwise. The string and template
!     match whenever the template can expand (through replacement of its
!     wild cards) to become the input string.
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
!     MATCHW ignores leading and trailing blanks in both the string
!     and the template. All of the following are equivalent (they
!     all return .TRUE.).
!
!        MATCHW ( 'ALCATRAZ',     'A*Z',      '*', '%' )
!        MATCHW ( '  ALCATRAZ  ', 'A*Z',      '*', '%' )
!        MATCHW ( 'ALCATRAZ',     '  A*Z  ',  '*', '%' )
!        MATCHW ( '  ALCATRAZ  ', '  A*Z  ',  '*', '%' )
!
!     MATCHW is case-sensitive:  uppercase characters do not match
!     lowercase characters, and vice versa. Wild characters match
!     characters of both cases.
!
! $ Examples
!
!     Let
!
!        STRING  = '  ABCDEFGHIJKLMNOPQRSTUVWXYZ  '
!        WSTR    = '*'
!        WCHR    = '%'
!
!     Then
!
!        if TEMPL is  '*A*'        MATCHW is   T
!                     'A%D*'                     F
!                     'A%C*'                   T
!                     '%A*'                      F
!                         '%%CD*Z'                 T
!                         '%%CD'                     F
!                         'A*MN*Y*Z'               T
!                         'A*MN*Y*%Z'                F
!                         '*BCD*Z*'                T
!                         '*bcd*z*'                  F
!                         ' *BCD*Z*  '             T
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
!-    SPICELIB Version 1.4.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary entries from $Revisions section.
!
!-    SPICELIB Version 1.3.1, 11-NOV-2005 (NJB)
!
!        Corrected example calls in header; made other minor
!        edits to header.
!
!-    SPICELIB Version 1.3.0, 08-JUN-1999 (WLT)
!
!        Fixed comments in detailed output and example sections.
!
!-    SPICELIB Version 1.2.0, 15-MAY-1995 (WLT)
!
!        Direct substring comparisons were replaced with the logical
!        function SAMCH in several cases so as to avoid out of range
!        errors when examining substrings.
!
!-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
!
!        Set the default function value to either 0, 0.0D0, .FALSE.,
!        or blank depending on the type of the function.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     match string against wildcard template
!     test whether a string matches a wildcard template
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 06-OCT-1988 (WLT)
!
!        The old algorithm just did not work. Strings with wild
!        characters at the beginning or end of the string were not
!        matched correctly. For example, A% matched APPROX, if the
!        wild character token was % and the wild string token was *.
!        Needless to say, a new algorithm was developed.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               FRSTNB
      INTEGER               LASTNB
      LOGICAL               SAMCH


!
!     Local Variables
!
      INTEGER               SFIRST
      INTEGER               TFIRST
      INTEGER               SLAST
      INTEGER               TLAST

      INTEGER               SLEN
      INTEGER               TLEN

      INTEGER               SCUR
      INTEGER               TCUR

      INTEGER               LEFT
      INTEGER               RIGHT

      INTEGER               I
      INTEGER               J

      LOGICAL               NOSUBM

!
!     Set the default function value to be FALSE.
!
      MATCHW = .FALSE.

!
!     First let's get everybody's measurements.
!
      SFIRST = FRSTNB ( STRING )
      SLAST  = LASTNB ( STRING )
      TFIRST = FRSTNB ( TEMPL  )
      TLAST  = LASTNB ( TEMPL  )

      TLEN   = TLAST - TFIRST + 1
      SLEN   = SLAST - SFIRST + 1

      SCUR   = MAX ( 1, SFIRST )
      TCUR   = TFIRST


!
!     A blank template matches a blank string, and nothing else.
!
      IF ( ( TLAST .EQ. 0 ) .AND. ( SLAST .EQ. 0 ) ) THEN

         MATCHW = .TRUE.
         RETURN

      ELSE IF ( TLAST .EQ. 0 ) THEN

         MATCHW = .FALSE.
         RETURN

      END IF


!
!     The beginning of the string and template must be identical
!     up to the first occurrence of a wild string.
!

      DO WHILE (       ( TCUR             .LE. TLAST ) &
                 .AND. ( SCUR             .LE. SLAST ) &
                 .AND. ( .NOT. SAMCH( TEMPL,TCUR, WSTR,1) ) )

         IF (       ( TEMPL(TCUR:TCUR) .NE. STRING(SCUR:SCUR) ) &
              .AND. ( TEMPL(TCUR:TCUR) .NE. WCHR              ) ) THEN

            MATCHW = .FALSE.
            RETURN

         ELSE

            TCUR = TCUR + 1
            SCUR = SCUR + 1

         END IF

      END DO

!
!     There are a three ways we could have finished the loop above
!     without hitting a wild string.
!
!     Case 1.  Both the string and template ran out of characters at
!     the same time without running into a wild string in the template.
!
      IF (       ( TCUR .GT. TLAST ) &
           .AND. ( SCUR .GT. SLAST ) ) THEN

         MATCHW = .TRUE.
         RETURN

      END IF

!
!     Case 2. The template ran out of characters while there were still
!     characters remaining in the in the string.  No match.
!
      IF (       ( TCUR .GT. TLAST ) &
           .AND. ( SCUR .LE. SLAST ) ) THEN

         MATCHW = .FALSE.
         RETURN

      END IF

!
!     Case 3. The string ran out of characters while non-wild characters
!     remain in the template.
!
!     We have to check to see if any non-wild-string characters
!     remain.  If so, we DO NOT have a match.  On the other hand if
!     only wild string characters remain we DO have a match.
!
      IF (       ( TCUR .LE. TLAST ) &
           .AND. ( SCUR .GT. SLAST ) ) THEN

         MATCHW = .TRUE.

         DO I = TCUR, TLAST
            MATCHW = MATCHW .AND. ( TEMPL(I:I) .EQ. WSTR )
         END DO

         RETURN

      END IF

!
!     OK. There is only one way that you can get to this point.
!     It must be the case that characters remain in both the template
!     (TCUR .LE. TLAST) and the string (SCUR .LE. SLAST).  Moreover,
!     to get out of the first loop you had to hit a wild string
!     character.  Find the first non-wild-string character in the
!     template. (If there isn't one, we have a match.)
!

      DO WHILE (       ( TCUR             .LE. TLAST ) &
                 .AND. ( SAMCH ( TEMPL,TCUR, WSTR,1 )  ) )

         TCUR = TCUR + 1

      END DO

      IF ( TCUR .GT. TLAST ) THEN

         MATCHW = .TRUE.
         RETURN

      END IF

!
!     Still here? Ok. We have a non-wild-string character at TCUR. Call
!     this position left and look for the right end of the maximum
!     length substring of TEMPL (starting at left) that does not have
!     a wild string character.
!
      LEFT = TCUR

      DO WHILE (       ( TCUR             .LE.   TLAST   ) &
                 .AND. ( .NOT. SAMCH(TEMPL,TCUR, WSTR,1) ) )

         TCUR = TCUR + 1

      END DO

      RIGHT = TCUR - 1


      DO WHILE ( LEFT .LE. TLAST )

!
!        First see if there is enough room left in the string to
!        match TEMPL(LEFT:RIGHT)
!
         IF ( SLAST - SCUR .LT. RIGHT - LEFT ) THEN

            MATCHW = .FALSE.
            RETURN

         END IF

!
!        The substring TEMPL(LEFT:RIGHT) might be the end of the
!        string.  In such a case the ends of STRING must match
!        exactly with the end of TEMPL.
!
         IF ( RIGHT .EQ. TLAST ) THEN

            I = SLAST
            J = TLAST

            DO WHILE ( J .GE. LEFT )

               IF (      SAMCH (TEMPL,J, WCHR,  1 ) &
                    .OR. SAMCH (TEMPL,J, STRING,I )  ) THEN

                  J = J - 1
                  I = I - 1

               ELSE

                  MATCHW = .FALSE.
                  RETURN

               END IF

            END DO

!
!           If we made it through the loop, we've got a match.
!
            MATCHW = .TRUE.
            RETURN
         ELSE

!
!           In this case TEMPL(LEFT:RIGHT) is in between wild string
!           characters.  Try to find a substring at or to the right
!           of SCUR in STRING that matches TEMPL(LEFT:RIGHT)
!
            NOSUBM = .TRUE.

            DO WHILE ( NOSUBM )

               I = SCUR
               J = LEFT

               DO WHILE (     ( J           .LE. RIGHT           ) &
                         .AND.(      SAMCH ( STRING,I, TEMPL,J ) &
                                .OR. SAMCH ( WCHR,  1, TEMPL,J ) ) )

                  I = I + 1
                  J = J + 1

               END DO

!
!              If J made it past RIGHT, we have a substring match
!
               IF ( J .GT. RIGHT ) THEN

                  SCUR   = I
                  NOSUBM = .FALSE.

!
!              Otherwise, try the substring starting 1 to the right
!              of where our last try began.
!
               ELSE

                  SCUR = SCUR + 1

!
!                 Make sure there's room to even attempt a match.
!
                  IF ( SLAST - SCUR .LT. RIGHT - LEFT ) THEN

                     MATCHW = .FALSE.
                     RETURN

                  END IF

               END IF
            END DO

         END IF

!
!        If you have reached this point there must be something left
!        in the template and that something must begin with a wild
!        string character.  Hunt for the next substring that doesn't
!        contain a wild string character.
!
         DO WHILE (       ( TCUR             .LE. TLAST ) &
                    .AND.   SAMCH ( TEMPL,TCUR,  WSTR,1) )

            TCUR = TCUR + 1

         END DO

         IF ( TCUR .GT. TLAST ) THEN

!
!           All that was left was wild string characters.  We've
!           got a match.
!
            MATCHW = .TRUE.
            RETURN

         END IF

!
!        Still here? Ok. We have a non-wild-string character at TCUR.
!        Call this position left and look for the right end of the
!        maximum length substring of TEMPL (starting at left) that
!        does not have a wild string character.
!
         LEFT = TCUR

         DO WHILE (       ( TCUR             .LE. TLAST ) &
                    .AND. ( .NOT. SAMCH( TEMPL,TCUR, WSTR,1 ) ) )

            TCUR = TCUR + 1

         END DO

         RIGHT = TCUR - 1

      END DO

      END
