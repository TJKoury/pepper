!$Procedure      NEWFIL_1 ( Generate a filename that does not exist )

      SUBROUTINE NEWFIL_1 ( PATTRN, FILE )

! $ Abstract
!
!     This routine generates a filename that is derived from
!     the input PATTRN and returns the name that was generated
!     in FILE.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!       FILES
!
! $ Declarations

      CHARACTER*(*)         PATTRN
      CHARACTER*(*)         FILE

! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      PATTRN     I   is a name pattern following the rules of MAKSTR
!      FILE       O   the name of the file generated.
!
! $ Detailed_Input
!
!     PATTRN      The description below is lifted without change
!                 from the routine MAKSTR.
!
!                 PATTRN is a string that specifies a pattern that
!                 all strings in a sequence must match. There are
!                 several special substrings in PATTRN that must
!                 be recognized.
!
!                 1) A substring of the form '<*>' (where * is used
!                    as a variable length wildcard character) is called
!                    an expansion. The substring that occurs between
!                    the angle brackets < > is called the invisible
!                    portion of the expansion.  When the tokens of
!                    PATTRN are counted the invisible portion of the
!                    expansion is not counted.  Thus an expansion has
!                    exactly two tokens '<' and '>'  The invisible
!                    portion of the expansion must not contain
!                    any of the characters '<', '>', '{', or '}'.
!
!                 2) A substring of the form '{#-$}' where # and $
!                    stand for any chacter from the set
!                    '0', ... , '9', 'a', ... , 'z' is called a
!                    restriction.
!
!                 A pattern may consist of any collection of
!                 characters.  However, the characters '<' and
!                 '>' must always occur in balanced pairs with '<'
!                 on the left and '>' on the right. Moreover, they
!                 cannot be nested even if they are balanced. Similary
!                 '{' and '}' must always appear as a balanced pair
!                 and have exactly 3 characters between them.  The
!                 first is a lower case letter or a digit.  The second
!                 letter may be anything (usually a hyphen, colon or
!                 comma).  The third character must
!                 also be a letter between 0, ... ,9, a, b, ... , z
!                 and must occur later in the collating sequence than
!                 the first letter in the triple that occurs between
!                 '{' and '}'.
!
!                 For example the following are valid patterns
!
!                 PAT_<Value: >_{0-9}{a-z}{a-d}
!                 COUNTER{0-9}{0-9}{0-9}{0-9}
!                 COUNTER{0:9}{0,9}{a;b}
!
!                 but the following are not
!
!                 PAT_<<>>_{0-9}{a-z}{a-d}    --- Nested < >
!                 COUNTER{9-0}                --- 9 before 0
!                 PAT_{0to0}                  --- 4 characters between{}
!                 PAT_{A-Z}                   --- uppercase letters in{}
!                 PAT_{+-$}                   --- bad characters in {}
!
!                 Pattern should be viewed as consisting of a sequence
!                 of tokens.  The tokens consist of characters that
!                 are not part of an expansion or restriction
!                 restrictions and the '<' and '>' characters of
!                 any expansion.
!
! $ Detailed_Output
!
!     FILE        is a string that is the name of the file that was
!                 generated.  The name of the file will match the
!                 input PATTRN and will be the first name generated
!                 from PATTRN that does not exist.  See the routine
!                 MAKSTR for a more detailed explanation of the names
!                 that are generated using FSTSTR and NXTSTR.
!
! $ Parameters
!
!      None.
!
! $ Files
!
!      None.
!
! $ Exceptions
!
!     None.
!
! $ Particulars
!
!     This is a utility routine for creating a file name that
!     can be opened without fear of name collisions, i.e., it
!     creates tha name of a file that does not exist, thus
!     guaranteeing that you can open the file.
!
! $ Examples
!
!     Suppose that you need a utility file for holding some
!     temporary data structure in a program that makes use
!     of NSPIO for its IO.  Then you could make the following
!     call
!
!        PATTRN = 'util{0-9}{0-9}{0-9}{0-9}.tmp'
!
!        CALL NEWFIL ( PATTRN, FILE )
!
!     If successful, FILE will hold the name of the new file.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!      K.R. Gehringer  (JPL)
!      W.L. Taber      (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version
!
!-     Beta Version 1.0.0, 30-MAY-1996 (KRG) (WLT)
!
!-&

! $ Index_Entries
!
!     Create a new file name from a pattern
!
!-&

!
!     Spicelib routines.
!
      LOGICAL               EXISTS
      LOGICAL               RETURN
!
!     Local Parameters
!
!     Length of a filename.
!
      INTEGER               FNMLEN
      PARAMETER           ( FNMLEN = 255 )
!
!     Local Variables
!
      CHARACTER*(FNMLEN)    FNAME
      CHARACTER*(FNMLEN)    NAME
      CHARACTER*(FNMLEN)    THIS

      LOGICAL               DONE
      LOGICAL               NOMORE

      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'NEWFIL_1' )
      END IF

      FNAME = ' '
!
!     Get the first filename in the pattern space.
!
      CALL FSTSTR ( PATTRN, FNAME )

      NAME   = FNAME
      NOMORE = .FALSE.
      DONE   = .FALSE.
!
!     Look for a file name that does not already exist.
!
      DO WHILE ( .NOT. DONE )

         THIS = NAME
         NAME = ' '

         CALL NXTSTR ( PATTRN, THIS, NAME )

         DONE = NAME .EQ. FNAME

         IF ( .NOT. DONE ) THEN
            IF ( .NOT. EXISTS(NAME) ) THEN
               DONE = .TRUE.
            END IF
         ELSE
            NOMORE = .TRUE.
         END IF

      END DO

      IF ( NOMORE ) THEN
         FILE = ' '
         CALL SETMSG ( 'It was not possible to create a file' &
         //            ' name using ''#'' as the pattern. All' &
         //            ' of the file names that can be generated' &
         //            ' from this pattern already exist.'         )
         CALL ERRCH  ( '#', PATTRN                                 )
         CALL SIGERR ( 'SPICE(CANNOTMAKEFILE)'                     )
         CALL CHKOUT ( 'NEWFIL_1'                                  )
         RETURN
      END IF

      FILE = NAME

      CALL CHKOUT ( 'NEWFIL_1' )
      RETURN

      END






