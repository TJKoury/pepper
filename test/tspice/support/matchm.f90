!$Procedure MATCHM ( Match string against multiple wildcard templates )

      LOGICAL FUNCTION MATCHM ( STRING, TEMPL, WSTR, &
                                               WCHR, &
                                               NOTCHR, &
                                               ORCHR )
      IMPLICIT NONE

! $ Abstract
!
!      Determines whether or not a string matches any of a
!      collection of templates containing wildcard characters.
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
!      None
!
! $ Keywords
!
!      SEARCH
!      UTILITY
!
! $ Declarations

      CHARACTER*(*)         STRING
      CHARACTER*(*)         TEMPL
      CHARACTER*1           WSTR
      CHARACTER*1           WCHR
      CHARACTER*1           NOTCHR
      CHARACTER*1           ORCHR

! $ Brief_I/O
!
!      Variable  I/O  Description
!      --------  ---  --------------------------------------------------
!      STRING     I   String to be matched against templates.
!      TEMPL      I   Collection of templates.
!      WSTR       I   Wild string: represents any number of characters.
!      WCHR       I   Wild character: represents exactly one character.
!      NOTCHR     I   NOT character: negates one or more templates.
!      ORCHR      I   OR character: separates individual templates.
!
! $ Detailed_Input
!
!      STRING     is a character string to be checked for a match
!                 against the specified collection of templates.
!                 Leading and trailing blanks are ignored.
!
!      TEMPL      is a collection of individual templates to be
!                 compared against the specified string. Leading
!                 and trailing blanks are ignored. An empty (blank)
!                 template collection matches only an empty (blank)
!                 string.
!
!      WSTR       is the wild string token used in the templates.
!                 It represents from zero to any number of characters.
!                 Spaces may not be used as wild strings.
!
!      WCHR       is the wild character token used in the templates.
!                 It represents exactly one character. Spaces may not
!                 be used as wild characters.
!
!      NOTCHR     is the NOT character used in the templates.
!                 When it appears at the beginning of a template,
!                 it negates the template: that is, a string matches
!                 the negated template if it does not match the
!                 template itself. When it appears after the first
!                 character in a template, it is treated as an
!                 ordinary character. Spaces between a not character
!                 and the rest of a template are ignored.
!
!                 In addition, the NOT character may be used to negate
!                 the entire collection of templates by placing it by
!                 itself at the head of the collection.
!
!                 Spaces may not be used as NOT characters.
!
!      ORCHR      is the OR character used to separate individual
!                 templates in the collection. Spaces adjacent to
!                 the OR character are ignored. Consecutive OR
!                 characters separated only by zero or more spaces
!                 are considered to delimit a single blank template.
!
!                 Spaces may not be used as OR characters.
!
! $ Detailed_Output
!
!      The function is TRUE whenever the string matches the collection
!      of templates, and is FALSE otherwise.
!
! $ Exceptions
!
!      1) If the four special characters are not distinct, the error
!         SPICE(AMBIGTEMPL) is signalled.
!
!      2) If any of the four special characters is a space, the error
!         SPICE(ILLEGTEMPL) is signalled.
!
! $ Files
!
!      None.
!
! $ Particulars
!
!      MATCHM is an extension of MATCHI, which matches a string against
!      a single template. The major differences are the addition of the
!      NOT character, and the ability to match against combinations of
!      individual templates.
!
!      Like MATCHI, MATCHM is case-insensitive. Uppercase templates
!      match lowercase strings, and vice versa.
!
!      In the following discussion, we will assume that the four
!      special characters are defined as follows.
!
!            WCHR    = '%'
!            WSTR    = '*'
!            NOTCHR  = '~'
!            ORCHR   = '|'
!
!
!      1. Normal Templates
!      -------------------
!
!      A normal individual template contains some combination of
!      ordinary characters, wild characters, and wild strings.
!      The rules governing these templates are identical to those
!      used by MATCHI.
!
!
!      2. Negated Templates
!      --------------------
!
!      Any normal individual template may be negated by prefixing
!      it with the NOT character. The NOT character, when embedded
!      within either a normal or a negated template, is treated as
!      an ordinary character. For example, the template
!
!         '~*WN%.FOR'
!
!      is negated, and matches any string that does NOT match the
!      normal template
!
!         '*WN%.FOR'
!
!      while the template
!
!         'Dr.~Sm%th*'
!
!      is not negated. In particular, double negations are not
!      recognized. That is, the template
!
!         '~~X*'
!
!      means "not like ~X*".
!
!      The NOT character, when it appears by itself,
!
!         '~'
!
!      is equivalent to the template
!
!         '~*'
!
!      which does not match any string.
!
!
!      3. Combining Templates
!      ----------------------
!
!      Frequently, you will wish to determine whether a string matches
!      any of a number of individual templates: for example, whether a
!      file name matches any of the templates
!
!         '*.FOR'
!         '*.F77'
!         '*.INC'
!
!      The individual templates can be collected together into a
!      single string, separated by the OR character,
!
!         '*.FOR  |  *.F77  |  *.INC'
!
!      (Spaces adjacent to the separators are ignored. That is, the
!      collection
!
!         '*.FOR|*.F77|*.INC'
!
!      is messier than, but equivalent to, the previous collection.)
!
!      Note that conssecutive OR characters separated by zero or
!      more blanks are considered to delimit a blank template.
!      Thus, the following, which match a blank string, are all
!      equivalent
!
!         '*.FOR || *.F77'
!         '*.FOR | | *.F77'
!         '*.FOR || | *.F77'
!         '*.FOR |||||| *.F77'
!
!
!      4. Combining Negated Templates
!      ------------------------------
!
!      Both normal and negated templates may be combined using the
!      OR character. However, negated templates should be combined
!      with great care. Recalling that the logical expression
!
!         ( ~A  |  ~B  |  ~C )
!
!      is equivalent to the expression
!
!         ~ ( A  &  B  &  C )
!
!      convince yourself that the collection
!
!         '~X* | ~Y*'
!
!      meaning "not like X* or not like Y*", really means "not like
!      both X* and Y*", and matches EVERY string. This is not to say
!      that such collections do not have their uses. Combinations
!      of negated templates are used to find strings for which
!      combinations of constraints are not met simultaneously.
!      For example, the collection
!
!         '~[* | ~*]'
!
!      ("does not begin with a left bracket, or does not end with
!      a right bracket", or "does not both begin with a left bracket
!      and end with a right bracket") may be used to detect strings
!      which cannot be VMS directory specifications.
!
!
!      5. Negating Combined Templates
!      ------------------------------
!
!      It is easy to mistakenly expect a combination of negated
!      templates to act like the negation of a combination of
!      templates, but they are very different things. Continuing
!      our example of Section 3, we may wish to know which file
!      names do NOT match any of our templates. Clearly
!
!         '~*.FOR | ~*.F77 | ~*.INC'
!
!      will not do the trick, as it matches every possible file name.
!      We need instead to group the individual templates under a single
!      negation:
!
!         '~( *.FOR | *.F77 | *.INC )'
!
!      However, this grouping is not indicated with parentheses,
!      but rather by placing a lone NOT character at the head of
!      the collection,
!
!         '~ | *.FOR | *.F77 | *.INC'
!
!      This syntax, while not immediately obvious, has at least
!      two advantages. First, it does not require any new special
!      characters. Second, it makes adding new individual templates
!      to the end of the list a trivial operation.
!
!
!      6. Advanced Topics
!      ------------------
!
!      The final level in the construction of template collections
!      involves the combination of normal and negated templates.
!      Consider the templates 'A*' and '*.FOR'. The set of strings
!      matching the collection
!
!         'A* | ~*.FOR'
!
!      ("begins with A or is not like *.FOR") is just the UNION
!      of the sets of the strings matching the individual templates.
!      This is true for any set of templates, negated or normal.
!
!      But there's more. De Morgan's Laws tell us that the complement
!      (negation) of a union of sets (templates) is the same as the
!      intersection of the complements of the sets. Thus, by negating
!      the original templates, and by negating the collection of the
!      negated templates, we end up with
!
!          '~ | ~A* | *.FOR'
!
!      meaning "not (does not begin with A or is like *.FOR)".
!      But this means "both begins with A and is not like *.FOR".
!      So the set of strings matching the collection is just the
!      INTERSECTION of the sets of strings matching the original
!      templates.
!
! $ Examples
!
!      The following examples are grouped according to the discussion
!      of the Particulars section. The nominal values of the special
!      characters are the same, namely
!
!         WCHR    = '%'
!         WSTR    = '*'
!         NOTCHR  = '~'
!         ORCHR   = '|'
!
!
!      1. Normal Templates
!      -------------------
!
!      Consider the following string
!
!         '  ABCDEFGHIJKLMNOPQRSTUVWXYZ '
!
!      and the following templates.
!
!         Template         Matches STRING?
!         ---------------  ---------------
!         '*A*'            Yes
!         'A%D*'           No
!         'A%C*'           Yes
!         '%A*'            No
!         ' A*   '         Yes
!
!         '%%CD*Z'         Yes
!         '%%CD'           No
!         'A*MN*Y*Z'       Yes
!         'A*MN*Y%Z'       No
!         '*BCD*Z*'        Yes
!         '*bcd*z*'        Yes
!
!
!      2. Negated Templates
!      --------------------
!
!      Consider the same string, and the following templates.
!
!         Template         Matches STRING?
!         ---------------  ---------------
!         '~%B*D'          Yes
!         '~%B*D*'         No
!         '~ABC'           Yes
!         '~ABC*'          No
!         '~~B*'           Yes
!
!      Note that in the final example, the second '~' is treated not as
!      a second negation but as an ordinary character.
!
!
!      3. Combining Templates
!      ----------------------
!
!      Consider the following strings and templates.
!
!           String          Template             Matches?
!           --------------  -------------------  --------
!           AKRON           *A*|*B*              Yes
!           BELOIT          *B*|*I*              Yes
!           CHAMPAGNE       *B*|*I*              No
!
!
!      4. Combining Negated Templates
!      ------------------------------
!
!      Consider the following strings and templates.
!
!           String          Template             Matches?
!           --------------  -------------------  --------
!           SEQUIOA         ~*A*|~*E*|~*I*       No
!           SAINT PAUL      ~*A*|~*E*|~*I*       Yes
!           HOUSTON         ~*A*|~*E*|~*I*       Yes
!
!
!      5. Negating Combined Templates
!      ------------------------------
!
!      Consider the following strings and templates.
!
!           String          Template             Matches?
!           --------------  -------------------  --------
!           DETROIT         ~|B*|D*              No
!           EUGENE          ~|B*|D*              Yes
!           FAIRBANKS       ~|*A*|*I*|*O*|*U*    No
!           GREENBELT       ~|*A*|*I*|*O*|*U*    Yes
!
! $ Restrictions
!
!      None.
!
! $ Parameters
!
!      None.
!
! $ Author_and_Institution
!
!      W.L. Taber     (JPL)
!      I.M. Underwood (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version
!
!-     SUPPORT Version 2.3.0, 10-MAY-2006 (EDW)
!
!         Added logic to prevent the evaluation of TEMPL(BEG:BEG)
!         if BEG exceeds the length of TEMPL. Functionally, the
!         evaluation had no effect on MATCHM's output, but the ifort
!         F95 compiler flagged the evaluation as an array
!         overrun error. This occurred because given:
!
!             A .AND. B
!
!         ifort evaluates A then B then performs the logical
!         comparison.
!
!         Edited header to match expected SPICE format.
!
!-     META/2 Configured Version 2.2.0, 28-DEC-1994 (WLT)
!
!         An initial value is given to MATCHM so that it will
!         have a value even if return mode is in effect.
!
!-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 9, 1994
!
!
!-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of META/2
!         software as of May 3, 1994
!
!
!      Version B 1.0.0, 31-MAR-1988
!
!-&

! $ Index_Entries
!
!     string match to templates
!
!-&

!
!     SPICELIB functions
!
      INTEGER               FRSTNB
      INTEGER               QLSTNB
      LOGICAL               MATCHI
      INTEGER               UPTO
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               BEG
      INTEGER               END
      INTEGER               B
      INTEGER               E

      LOGICAL               MATCH
      LOGICAL               NEGATE
      LOGICAL               LOOP

!
!     Give the function an initial value.
!
      MATCHM = .FALSE.

!
!     Standard SPICE error handling
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'MATCHM' )
      END IF

!
!     Reject bad inputs.
!
      IF (      WSTR   .EQ. ' ' &
           .OR. WCHR   .EQ. ' ' &
           .OR. NOTCHR .EQ. ' ' &
           .OR. ORCHR  .EQ. ' ' ) THEN

         CALL SIGERR ( 'SPICE(ILLEGTEMPL)' )
         CALL CHKOUT ( 'MATCHM' )
         RETURN

      ELSE IF (      WSTR   .EQ. WCHR &
                .OR. WSTR   .EQ. NOTCHR &
                .OR. WSTR   .EQ. ORCHR &
                .OR. WCHR   .EQ. NOTCHR &
                .OR. WCHR   .EQ. ORCHR &
                .OR. NOTCHR .EQ. ORCHR   ) THEN

         CALL SIGERR ( 'SPICE(AMBIGTEMPL)' )
         CALL CHKOUT ( 'MATCHM' )
         RETURN

      END IF

!
!     Ignore leading and trailing spaces in the collection.
!
      BEG  = FRSTNB ( TEMPL  )
      END  = QLSTNB ( TEMPL  )

!
!     A blank collection matches ONLY a blank string.
!
      IF ( BEG .EQ. 0 ) THEN
         MATCHM = ( STRING .EQ. ' ' )
         CALL CHKOUT ( 'MATCHM' )
         RETURN
      END IF

!
!     If the first template is the NOT character, the entire collection
!     is negated, and we can begin with the next template. Otherwise,
!     just start at the beginning again.
!
      B = BEG
      E = UPTO ( TEMPL(1:END), ORCHR, B )

      IF( E .GE. LEN(TEMPL) ) THEN
         NEGATE = .FALSE.
         BEG    = B
      ELSE IF ( TEMPL(B:E) .EQ. NOTCHR .AND. &
                TEMPL(E+1:E+1) .EQ. ORCHR) THEN
         NEGATE = .TRUE.
         BEG    = E + 2
      ELSE
         NEGATE = .FALSE.
         BEG    = B
      END IF

!
!     Grab one template at a time, comparing them against the string
!     until a match has occured or until no templates remain.
!
      MATCH = .FALSE.

      DO WHILE ( ( BEG .LE. END ) .AND. (.NOT. MATCH ) )

         B = BEG
         E = UPTO ( TEMPL(1:END), ORCHR, B )

!
!        If we started on an OR character, then either we are
!        at the beginning of a string that starts with one,
!        or we just passed one and found another either next to
!        it, or separated by nothing but spaces. By convention,
!        either case is interpreted as a blank template.
!
         IF ( TEMPL(B:B) .EQ. ORCHR ) THEN

            MATCH  = ( STRING .EQ. ' ' )
            BEG    = BEG + 1

!
!        If this is a negated template, negate the results.
!        Remember that a NOT character by itself does not
!        matches anything.
!
         ELSE IF ( TEMPL(B:B) .EQ. NOTCHR ) THEN

            IF ( TEMPL(B:E) .EQ. NOTCHR ) THEN
               MATCH = .FALSE.
            ELSE
               MATCH = .NOT. MATCHI ( STRING, TEMPL(B+1:E), WSTR, WCHR )
            END IF

            BEG    = E + 2

!
!        Or a normal one?
!
         ELSE

            MATCH  = MATCHI ( STRING, TEMPL(B:E), WSTR, WCHR )
            BEG    = E + 2

         END IF

!
!        Skip any blanks before the next template.
!        The logic ensures no evaluation of TEMPL(BEG:BEG)
!        if BEG > LEN(TEMPL).
!
         LOOP = BEG .LT. END
         IF( LOOP ) THEN
            LOOP = LOOP .AND.  TEMPL(BEG:BEG) .EQ. ' '
         END IF

         DO WHILE ( LOOP )
            BEG = BEG + 1

            IF( BEG .GE. END ) THEN
               LOOP = .FALSE.
            ELSE IF ( TEMPL(BEG:BEG) .NE. ' ' ) THEN
                LOOP = .FALSE.
            ELSE
                LOOP = .TRUE.
            END IF

         END DO

      END DO

!
!     It doesn't happen often, but occasionally a template ends with
!     the OR character. This implies a blank template at the end of
!     the collection.
!
      IF ( TEMPL(END:END) .EQ. ORCHR ) THEN
         IF ( .NOT. MATCH ) THEN
            MATCH = ( STRING .EQ. ' ' )
         END IF
      END IF

!
!     Negate the results, if appropriate.
!
      IF ( NEGATE ) THEN
         MATCHM = .NOT. MATCH
      ELSE
         MATCHM =       MATCH
      END IF

      CALL CHKOUT ( 'MATCHM' )
      RETURN
      END
