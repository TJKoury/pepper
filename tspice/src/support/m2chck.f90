!$Procedure      M2CHCK ( Meta-2, check a table of syntax definitions )

      SUBROUTINE M2CHCK ( STATMN, SYNKEY, SYNPTR, SYNVAL, ERROR )
      IMPLICIT NONE

! $ Abstract
!
!     Using a symbol table of syntax definition statement indexed by
!     initial keyword, determine if the input statement is syntactically
!     correct.
!
! $ Required_Reading
!
!     META-2 A command definition language and parser.
!
! $ Keywords
!
!     META-2
!     PARSING
!
! $ Declarations

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         STATMN
      CHARACTER*(*)         SYNKEY ( LBCELL : * )
      INTEGER               SYNPTR ( LBCELL : * )
      CHARACTER*(*)         SYNVAL ( LBCELL : * )
      CHARACTER*(*)         ERROR  (          * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     STATMN     I   A statement to check for syntactic correctness.
!     SYNKEY     I   A symbol table of syntax definitions.
!     SYNPTR
!     SYNVAL
!     ERROR      O   Blank if STATMN correct, diagnosis otherwise.
!
! $ Detailed_Input
!
!     STATMN     is a string that is a candidate for a syntactically
!                correct statement.
!
!     SYNKEY     is a symbol table.  It is indexed by the initial
!     SYNPTR     keywords of META-2 syntax definition statements.
!     SYNVAL     This table is best prepared using the routine
!                M2INTS.
!
! $ Detailed_Output
!
!     ERROR      is an array of character strings that are used to
!                diagnose how well a STATMN matches one of the
!                syntax specificiations in the input symbol table.
!                If the STATMN is syntactically correct ERROR(1)
!                is returned as a blank.  Otherwise it is returned
!                with a diagnosis of why STATMN failed to be
!                syntactically correct.
!
!                Parsing of STATMN is usually accomplished by using
!                the various M2GET routines.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine can be used to compare a statement with a large
!     collection of syntax definitions provided all of the definitions
!     begin with a keyword.  To make use of this routine, you must first
!     prepare the symbol table.  The easiest way to to this is to use
!     the routine M2INTS.
!
!     To parse the input statement once it has been determine that it
!     is syntactically correct, one can use the M2GET routines to locate
!     the various substring corresponding to the meaning of STATMN.
!
! $ Examples
!
!     Typical useage looks like this:
!
!           IF ( FIRST ) THEN
!
!              CALL     M2INTS ( NSYN, SYNKEY, SYNPTR, SYNVAL )
!              FIRST = .FALSE.
!
!           END IF
!
!           CALL M2CHCK ( STATMN, SYNKEY, SYNPTR, SYNVAL, ERROR )
!
!           IF ( ERROR(1) .NE. ' ' ) THEN
!              CALL PREFIX ( 'MYNAME:', 1, ERROR(1) )
!              RETURN
!           END IF
!
!           Still here?  Determine what the string actually meant.
!
! $ Restrictions
!
!     To make use of STATMN for parsing with the M2GET routines, you
!     should not alter it after the call to M2CHCK until you have
!     finished parsing it.
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
!-    Beta Version 1.0.0, 4-MAY-1992 (WLT)
!
!-&

! $ Index_Entries
!
!     Check a statement against a set of syntax definitions
!
!-&



!
!     Spicelib functions
!
      INTEGER               CARDI
      LOGICAL               RETURN

!
!     Local Variables
!
      INTEGER               ROOM
      PARAMETER           ( ROOM = 10 )

      INTEGER               BEST   ( LBCELL : ROOM )
      INTEGER               SCORES ( LBCELL : ROOM )

      INTEGER               B
      INTEGER               BST
      INTEGER               CUTOFF
      INTEGER               E
      INTEGER               I
      INTEGER               LOOKAT
      INTEGER               MXSCOR
      INTEGER               N
      INTEGER               PTR

      LOGICAL               FOUND

      LOGICAL               UNKNWN

      CHARACTER*(32)        KEYWRD
      CHARACTER*(160)       MSSG





      IF ( RETURN() ) THEN
         ERROR(1) = 'M2CHCK: The function RETURN was set to .TRUE. '  // &
                    'This situation is not supposed to happen.'
         RETURN
      END IF

!
!     Initialize the cell BEST and SCORES.
!
      CALL SSIZEI ( ROOM, BEST   )
      CALL SSIZEI ( ROOM, SCORES )

!
!     Get the first word of the input string.
!
      CALL FNDNWD ( STATMN,  1,  B, E   )
      CALL UCASE  ( STATMN(B:E), KEYWRD )

!
!     Find the syntax templates that match the first word of the
!     command.
!
      CALL SYPTRC ( KEYWRD, SYNKEY, &
                            SYNPTR, &
                            SYNVAL, PTR, N, FOUND )

!
!     If we didn't find our word, then we look for a word that
!     comes close spelling-wise
!
      IF ( .NOT. FOUND ) THEN

         CUTOFF = 70
         CALL BESTWD ( KEYWRD, SYNKEY, CUTOFF, BEST, SCORES, MSSG )

         IF ( CARDI(BEST) .EQ. 0 ) THEN
            UNKNWN = .TRUE.
         ELSE IF ( SCORES(1) .LT. 50 ) THEN
            UNKNWN = .TRUE.
         ELSE
            UNKNWN = .FALSE.
         END IF

         IF ( UNKNWN ) THEN

            ERROR (1) = 'Sorry but I didn''t recognize the word'
            CALL SUFFIX ( KEYWRD, 1, ERROR(1) )
            CALL SUFFIX ( 'as the beginning of any valid statement. ', &
                           1,        ERROR(1) )
            RETURN

         END IF

!
!        Still here? fetch the set of likely syntax statements to check.
!
         MXSCOR = 0

         DO I = 1, CARDI(BEST)
            IF ( SCORES(I) .GT. MXSCOR )  THEN
               MXSCOR = SCORES(I)
               LOOKAT = I
            END IF
         END DO

         KEYWRD = SYNKEY( BEST(LOOKAT) )

         CALL SYPTRC ( KEYWRD, SYNKEY, &
                               SYNPTR, &
                               SYNVAL, PTR, N, FOUND )

      END IF

!
!     Until we find out otherwise, we shall assume that we have
!     a syntactically correct input statement.
!
      CALL META_2( STATMN, SYNVAL(PTR), N, SYNVAL(LBCELL), BST, ERROR )


      IF ( ERROR(1) .NE. ' ' ) THEN
         CALL PREFIX ( 'M2CHCK:', 1, ERROR(2) )
      END IF

      RETURN
      END
