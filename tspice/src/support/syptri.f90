!$Procedure       SYPTRI ( Symbol table, fetch pointers, generic )

      SUBROUTINE  SYPTRI ( NAME, SYMNAM, SYMPTR, SYMVAL, PTR, N, FOUND )
      use pepper_legacy
      IMPLICIT NONE


! $ Abstract
!
!     Return the address of the first value associated with a symbol
!     and the number of values associated with the symbol.
!
! $ Required_Reading
!
!     SYMBOLS
!
! $ Keywords
!
!     SYMBOLS
!
! $ Declarations

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         NAME
      CHARACTER*(*)         SYMNAM ( LBCELL : * )
      INTEGER               SYMPTR ( LBCELL : * )
      INTEGER               SYMVAL ( LBCELL : * )

      INTEGER               PTR
      INTEGER               N
      LOGICAL               FOUND

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NAME       I   The name of a symbol.
!     SYMNAM     I   The name cell of symbol table.
!     SYMPTR     I   The pointer cell of a symbol table.
!     SYMVAL     I   The value cell of a symbol table.
!     PTR        O   The index of the first value associated with NAME.
!     N          O   The number of values associated with NAME.
!     FOUND      O   TRUE if NAME is in the symbol table, else FALSE
!
! $ Detailed_Input
!
!     NAME       is a string representing the name of some symbol that
!                might be in the symbol table SYMNAM, SYMPTR, ...
!
!     SYMNAM     is a symbol table.
!     SYMPTR
!     SYMVAL
!
!
! $ Detailed_Output
!
!     PTR        is the location in the values cell of the symbol table
!                where the values associated with NAME begin.
!
!     N          is the number of values in the symbol table
!                associated with NAME.
!
!     FOUND      is TRUE if NAME is the name of a symbol. Otherwise,
!                it is FALSE.
!
!
! $ Parameters
!
!
!     None.
!
! $ Exceptions
!
!     1) If NAME is not present in the symbol table, N and PTR will
!        both be returned with the value 0.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine returns the index of the first value associated with
!     a particular name in a symbol table.  It also returns the number
!     of values associated with the name.  In this way, routines that
!     "read" the values associated with a symbol table name, can read
!     them directly without having to declare local storage for these
!     values.
!
! $ Examples
!
!     Suppose that you need to count the number of values associated
!     with NAME that satisfy some property (computed by a logical
!     function PROP that you have written). The following block of code
!     would do the job.
!
!           COUNT = 0
!
!           CALL SYPTRI ( NAME, SYMNAM, SYMPTR, SYMVAL, PTR, N, FOUND )
!
!           DO I = PTR, PTR + N - 1
!
!              IF ( PROP(SYMVAL(I)) ) THEN
!                 COUNT = COUNT + 1
!              END IF
!
!           END DO
!
!
! $ Restrictions
!
!     User's should not attempt to access values beyond those in the
!     range returned returned by this routine. Also, any action that is
!     to be performed with the values associated with NAME should
!     be performed within a scope in which the symbol table cannot
!     be altered by other calls to symbol table routine.
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
!-    Beta Version 1.0.0, 4-MAY-1992 (WLT)
!
!-&

! $ Index_Entries
!
!     Find pointers to values in a symbol table.
!
!-&




      INTEGER               LOC
      INTEGER               NUMBER

      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYPTRI' )
      END IF
!
!     We don't use the values of the symbol table in this routine
!     but it is passed for the sake of uniformity in the symbol
!     table routine calling sequences.  However, some compilers
!     generate warnings if a variable isn't used.  So we touch
!     the values cell to fake out the compiler.
!
      SYMVAL(LBCELL) = TOUCHI ( SYMVAL(LBCELL) )

!
!     Now for the real work of this routine.
!
      NUMBER = CARDC  (                SYMNAM    )
      LOC    = BSRCHC ( NAME,  NUMBER, SYMNAM(1) )

      IF ( LOC .EQ. 0 ) THEN
         FOUND = .FALSE.
         PTR   =  0
         N     =  0
         CALL CHKOUT ( 'SYPTRI' )
         RETURN
      END IF

      PTR   =  SUMAI  ( SYMPTR(1), LOC-1 ) + 1
      N     =  SYMPTR ( LOC )
      FOUND = .TRUE.

      CALL CHKOUT ( 'SYPTRI' )

      RETURN
      END
