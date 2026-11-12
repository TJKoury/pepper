!$Procedure      M2INTS (Meta 2 --- initialize syntax table)

      SUBROUTINE M2INTS ( NSYN, SYNKEY, SYNPTR, SYNVAL )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Construct a symbol table that uses the initial keywords of
!     META-2 syntax definitions as the keys to the same a set of
!     META-2 syntax definitions.
!
! $ Required_Reading
!
!     META-2 A language definition language and parser.
!
! $ Keywords
!
!     INITIALIZATION
!     PARSING
!     UTILITY
!
! $ Declarations


      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               NSYN

      CHARACTER*(*)         SYNKEY ( LBCELL : * )
      INTEGER               SYNPTR ( LBCELL : * )
      CHARACTER*(*)         SYNVAL ( LBCELL : * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NSYN       I   The number of syntax definition statements.
!     SYNKEY     O   The key (names) portion of a symbol table.
!     SYNPTR     O   The pointer portion of a symbol table.
!     SYNVAL    I/O  The Meta-2 syntax statements.
!
! $ Detailed_Input
!
!     NSYN       is the number of syntax statements that will be
!                organized into an initial keyword based symbol table
!
!     SYNVAL     is a cell containing syntax definintion statements.
!                The defitions should be located at indices 1 through
!                NSYN.
!
! $ Detailed_Output
!
!     SYNKEY     is the names portion of a symbol table.  The names
!                in this array will be the initial keywords of the
!                syntax definition statments stored in SYNVAL.  Each
!                initial keyword will be associated with those
!                collection of definitions that begin with that keyword.
!
!     SYNPTR     is the pointer cell of the symbol table
!                SYNKEY, SYNPTR, SYNVAL
!
!     SYNVAL     is the input cell organized now as the values cell
!                of the symbol table SYNKEY, SYNPTR, SYNVAL.
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
!     This routine serves to initialize a syntax cell and list of
!     initial known keywords.  This is useful primarily for META2
!     languages that have all syntax definitions beginning with a
!     diverse set of keywords.  It is anticipated that users will
!     use this once in a module that accepts language statements.
!
!           if ( first ) then
!
!              first = .false.
!              call m2intp ( nsyn, synkey, synptr, synval )
!           end if
!
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     This routine is intended only for use with META-2 derived
!     languages whose syntax statements all begin with keywords.
!     It is assumed that all keywords are 32 or fewer characters
!     in length.
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
!
!     Initialize an intial keyword based META-2 syntax table
!-&



!
!     Spicelib functions.
!

!
!     Local variables.
!
      CHARACTER*(32)        KEYWRD
      CHARACTER*(32)        LSTKEY

      INTEGER               B
      INTEGER               E
      INTEGER               I
      INTEGER               PUT


!
!     Initialize the symbol table size attributes.
!
      CALL SSIZEC ( NSYN,   SYNKEY )
      CALL SSIZEI ( NSYN,   SYNPTR )
      CALL SSIZEC ( NSYN,   SYNVAL )

!
!     Just in case, left justify everything in the values cell
!     and set all of the pointer values to 0.
!
      DO I = 1, NSYN
         CALL LJUST ( SYNVAL(I), SYNVAL(I)  )
         SYNPTR(I) = 0
      END DO

!
!     Turn the collection of syntax definitions into an array ordered
!     by initial keyword (minus any labels).
!
      CALL M2SHLL ( NSYN, SYNVAL(1) )
!
!     Remove any duplicates including a blank at the beginning if
!     there is one.
!
      PUT       = 0
      SYNVAL(0) = ' '
      DO I = 1, NSYN
         IF ( SYNVAL(I) .NE. SYNVAL(I-1) ) THEN
            PUT         = PUT + 1
            SYNVAL(PUT) = SYNVAL(I)
         END IF
      END DO

      CALL SSIZEC ( NSYN,   SYNVAL )
      CALL SCARDC ( PUT,    SYNVAL )

!
!     Now we will construct the symbol table to go with this collection
!     of syntax definitions.
!
      LSTKEY = ' '
      PUT    =  0

      DO I = 1, CARDC(SYNVAL)

!
!        Get the first word, and trim off any attached label.  Note that
!        since this is supposed to be a keyword, there are no range
!        templates or qualifiers attached.
!
         CALL FNDNWD ( SYNVAL(I),      1,  B,  E )
         CALL M2TRIM ( SYNVAL(I)(B:E), KEYWRD    )
         CALL UCASE  ( KEYWRD,         KEYWRD    )

!
!        If this is a new keyword, put it into the list of keywords and
!        change the last keyword.
!
         IF ( KEYWRD .NE. LSTKEY ) THEN
            PUT         = PUT + 1
            SYNKEY(PUT) = KEYWRD
            LSTKEY      = KEYWRD
         END IF

!
!        Increment the value in the pointer array.
!
         SYNPTR(PUT) = SYNPTR(PUT) + 1

      END DO

!
!     Set the cardinality of the name and pointer cells.
!
      CALL SCARDC ( PUT, SYNKEY )
      CALL SCARDI ( PUT, SYNPTR )

!
!     Finally, blank out all of the non-used parts of the values cell.
!
      DO I = -5, -2
         SYNVAL(I) = ' '
      END DO

      RETURN
      END
