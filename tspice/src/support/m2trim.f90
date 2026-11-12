!$Procedure      M2TRIM ( META/2 trim the name portion from a word )

      SUBROUTINE M2TRIM ( WORD, ROOT )
      use pepper_legacy
      IMPLICIT NONE
! $ Abstract
!
!     Extract the "root" of a META/2 template word.  That is trim off
!     the name portion of a template word.
!
! $ Required_Reading
!
!    META/2
!
! $ Keywords
!
!     META1
!
! $ Declarations

      CHARACTER*(*)         WORD
      CHARACTER*(*)         ROOT

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     WORD       I   A word from a META/2 template.
!     ROOT       O   The input word trimmed of any name specification.
!
! $ Detailed_Input
!
!     WORD       is a word from a META/2 template.  It may or may not
!                looklike   ROOT // '[name]'
!
! $ Detailed_Output
!
!     ROOT       is the portion of the input word that precedes the
!                name portion of the input WORD.  ROOT may overwrite
!                WORD.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     If ROOT is not sufficiently large to contain all of the output,
!     it will be truncated on the right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     META/2 template words can have appended to them "variable" names
!     that will be used to store substring boundaries of STRINGS matched
!     against META/2 templates.  For example
!
!           FIND @name[WINDOW]
!           SEPARATION  (2:2){ OF   @int[BODY1] @int[BODY2]
!                            | FROM @int[OBSERVER]          }
!
!     the words
!
!         @name[WINDOW], @int[BODY1], @int[BODY2], @int[OBSERVER]
!
!     all have "varialbe" name substrings.  They are:
!
!         WINDOW, BODY1, BODY2, and OBSERVER respectively.
!
!     The routine removes variable names and associated brackets in WORD
!     if they exist.
!
! $ Examples
!
!     Below is a table descibing sample inputs and outputs.
!
!         WORD                ROOT
!         ---------------     ------------------
!         @int[SPUD]          @int
!         @name[WINDOW]       @name
!         SEARCH[GET]         SEARCH
!         @name               @name
!         @body(2:4)[LIST]    @body(2:4)
!
! $ Restrictions
!
!     None.
!
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber     (JPL)
!
! $ Version
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
!-    Beta Version 1.0.0, 21-NOV-1991 (WLT)
!
!-&
!
! $ Index_Entry
!
!     Extract the root of a META/2 template word.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               QRTRIM


!
!     Local variables
!
      INTEGER               E
      INTEGER               B
      INTEGER               LBRACE
      INTEGER               RBRACE
      INTEGER               BLANK

      SAVE



      ROOT   = WORD
      LBRACE = ICHAR ( '['  )
      RBRACE = ICHAR ( ']'  )
      BLANK  = ICHAR ( ' '  )
      E      = LEN   ( WORD )
!
!     This loop is the same as RTRIM only faster.
!
      E = QRTRIM ( WORD )


!
!     If the length is not at least 4 or the last character is not
!     a right brace, there is no name associated with this word.
!
      IF (      ( ICHAR(WORD(E:E)) .EQ. RBRACE ) &
          .AND. ( E                .GE. 4      ) ) THEN
!
!        Ok. We have a chance at getting a name.  Look for
!        a left brace and if found blank out the end portion of
!        ROOT.
!
         B = 2

         DO WHILE (B .LT. E - 1)

            IF ( ICHAR(WORD(B:B)) .EQ. LBRACE ) THEN
!
!              We've found the beginning of the name portion
!              of the word.  Record the end of the meta-2
!              word and then reset L so that we exit this loop.
!
               ROOT(B:) = ' '
               B        = E
            END IF

            B = B + 1

         END DO

      END IF

      RETURN
      END
