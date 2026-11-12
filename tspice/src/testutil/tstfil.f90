!$Procedure      TSTFIL ( Open a new file on the specified port )
!
      SUBROUTINE TSTFIL ( PATTRN, PORT, FILE )
      use pepper_legacy
!
! $ Abstract
!
!     This routine opens a port with a file that is created from
!     the input PATTRN and returns the name of the FILE attached
!     to the port.
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
!
      IMPLICIT NONE
      CHARACTER*(*)         PATTRN
      CHARACTER*(*)         PORT
      CHARACTER*(*)         FILE

!
! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      PATTRN     I   is a name pattern following the rules of MAKSTR
!      PORT       I   the port to which the FILE should be attached.
!      FILE       O   the name of the file attached to the port.
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
!     PORT        is the name of an TSTIO port that will be opened
!                 with the file name generated from PATTRN.
!
!     FILE        is a string that is the name of the file that is
!                 open and attached to the specified PORT.  The
!                 name of the file will match the input PATTRN
!                 and will be the first name generated from PATTRN
!                 that can be opened.  See the routine MAKSTR for
!                 a more detailed explanation of the names
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
!     1) If the file cannot be opened, the error
!        CMLOOP(CANNOTMAKEFILE) will be signalled.
!
! $ Particulars
!
!     This is a utility routine for creating a file name that
!     can be opened without fear of name collisions and attached
!     to one of the file ports supported by TSTIO.  In this way
!     you have a high likelyhood of success in opening a log file
!     or utility file for use by your program (this assumes that
!     you have adequate privelege to open a file in the directory
!     implied or specified by PATTRN).
!
! $ Examples
!
!     Suppose that you need a utility file for holding some
!     temporary data structure in a program that makes use
!     of TSTIO for its IO.  Then you could make the following
!     call
!
!        PATTRN = 'util{0-9}{0-9}{0-9}{0-9}.tmp'
!
!        CALL TSTFIL ( PATTRN, 'UTILITY', FILE )
!
!     If successful, FILE will hold the name of the file that
!     was opened and is attached to the UTILITY port of TSTIO.
!     Otherwise FILE will be returned as a blank and the
!     FAILED flag will have been set by the call to SIGERR
!     made in this routine.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
!
!-    SPICELIB Version 1.0.0, 21-APR-1994 (WLT)
!
!
!-&
!
! $ Index_Entries
!
!     Create a file name and attach it to an I/O port
!
!-&

      INTEGER               FILEN
      PARAMETER           ( FILEN = 127 )
!
!     Spicelib routines.
!

!
!     Local Parameters
!
      INTEGER               MAXTRY
      PARAMETER           ( MAXTRY = 20 )

      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     NAME
      CHARACTER*(FILEN)     THIS

      INTEGER               BADOPN

      LOGICAL               MORE



      CALL CHKIN  ( 'TSTFIL' )

      FNAME = ' '
      NAME  = ' '
      THIS  = ' '

      CALL FSTSTR ( PATTRN, FNAME )
      NAME   =  FNAME
      MORE   = .TRUE.
      BADOPN =  0

      DO WHILE ( BADOPN .LT. MAXTRY )
!
!        Look for a file name that does not already exist.
!
         DO WHILE ( EXISTS(NAME) .AND. MORE )
            THIS = NAME
            NAME = ' '
            CALL NXTSTR ( PATTRN, THIS, NAME )
            MORE = NAME .NE. FNAME
         END DO

         IF ( .NOT. MORE ) THEN
            FILE = ' '
            CALL SETMSG ( 'It was not possible to create a # ' &
            //            'file as specified. All appropriately ' &
            //            'named files already exist.' )
            CALL ERRCH  ( '#', PORT )
            CALL SIGERR ( 'CMLOOP(CANNOTMAKEFILE)' )
            CALL CHKOUT ( 'TSTFIL' )
            RETURN
         ELSE
            FILE = NAME
         END IF

!
!        Ok.  We've got a good candidate, try to attach it to the
!        specified port.
!
         CALL TSTOPN ( PORT, FILE )

         IF ( FAILED() ) THEN

            BADOPN = BADOPN + 1
!
!           We will try a few more times on the off chance that
!           some other program used the same name first.  This
!           is not likely, file protection problems or PATTRN
!           specifications are a more probable cause of the trouble,
!           but we try anyway.
!

            IF ( BADOPN .LT. MAXTRY ) THEN
               CALL RESET
            END IF

         ELSE
!
!           We were successful in opening the port with the
!           specified name.  We can quit now.
!
            CALL TSTSLF (  FILE    )
            CALL CHKOUT ( 'TSTFIL' )
            RETURN
         END IF

      END DO
!
!     If you get to this point, a file was not succesfully
!     attached to PORT.  But TSTIO has already diagnosed
!     the problem as much as we're going to.  Just set FILE
!     to a blank and return.
!
      FILE = ' '
      CALL CHKOUT ( 'TSTFIL' )


      RETURN

      END






