!$Procedure SYPOPC ( Pop a value from a particular symbol )

      SUBROUTINE SYPOPC ( NAME, TABSYM, TABPTR, TABVAL, VALUE, FOUND )

! $ Abstract
!
!     Pop a value associated with a particular symbol in a character
!     symbol table. The first value associated with the symbol is
!     removed, and subsequent values are moved forward.
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

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         NAME
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      CHARACTER*(*)         TABVAL     ( LBCELL:* )
      CHARACTER*(*)         VALUE
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol whose associated value is to be
!                    popped.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!     VALUE      O   Value that was popped.
!     FOUND      O   .TRUE. if the symbol exists, .FALSE. otherwise.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose associated value is to
!              be popped.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a character symbol table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a character symbol table.
!
!              On output, the value is removed from the symbol table,
!              and the remaining values associated with the symbol are
!              moved forward in the value table. If no other values are
!              associated with the symbol, the symbol is removed from
!              the symbol table.
!
!     VALUE    is the value that was popped. This value was the first
!              value in the symbol table that was associated with the
!              symbol NAME.
!
!     FOUND    is .TRUE. if NAME is in the symbol table, otherwise it is
!              .FALSE.
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
!     If there are no remaining values associated with the symbol
!     after VALUE has been popped, the symbol is removed from the
!     symbol table.
!
! $ Examples
!
!     The contents of the symbol table are:
!
!        BOHR      -->   HYDROGEN ATOM
!        EINSTEIN  -->   SPECIAL RELATIVITY
!                        PHOTOELECTRIC EFFECT
!                        BROWNIAN MOTION
!        FERMI     -->   NUCLEAR FISSION
!
!     The call,
!
!        CALL SYPOPC ( 'EINSTEIN', TABSYM, TABPTR, TABVAL,
!       .                                  VALUE,  FOUND  )
!
!     modifies the contents of the symbol table to be:
!
!        BOHR      -->   HYDROGEN ATOM
!        EINSTEIN  -->   PHOTOELECTRIC EFFECT
!                        BROWNIAN MOTION
!        FERMI     -->   NUCLEAR FISSION
!
!     FOUND is .TRUE., and VALUE is 'SPECIAL RELATIVITY'.
!
!
!     The next call,
!
!        CALL SYPOPC ( 'FERMI', TABSYM, TABPTR, TABVAL, VALUE, FOUND )
!
!     modifies the contents of the symbol table to be:
!
!        BOHR      -->   HYDROGEN ATOM
!        EINSTEIN  -->   SPECIAL RELATIVITY
!                        PHOTOELECTRIC EFFECT
!                        BROWNIAN MOTION
!
!      FOUND is .TRUE., and VALUE is 'NUCLEAR FISSION'. Note that
!      because "FERMI" had only one value associated with it, it was
!      removed from the symbol table.
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
!
!-&


! $ Index_Entries
!
!     pop a value from a particular symbol
!
!-&


!
!     SPICELIB functions
!
      INTEGER               BSRCHC
      INTEGER               CARDC
      INTEGER               CARDI
      LOGICAL               RETURN
      INTEGER               SUMAI

!
!     Local variables
!
      INTEGER               NSYM
      INTEGER               NPTR
      INTEGER               NVAL

      INTEGER               LOCSYM
      INTEGER               LOCVAL



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYPOPC' )
      END IF


!
!     How many symbols to start with?
!
      NSYM = CARDC ( TABSYM )
      NPTR = CARDI ( TABPTR )
      NVAL = CARDC ( TABVAL )

!
!     Is this symbol even in the table?
!
      LOCSYM = BSRCHC ( NAME, NSYM, TABSYM(1) )

!
!     If it's not in the table, it's definitely a problem.
!
      IF ( LOCSYM .EQ. 0 ) THEN

         FOUND = .FALSE.

!
!     If it is in the table, we can proceed without fear of overflow.
!
      ELSE

         FOUND = .TRUE.

!
!        Begin by saving and removing the initial value for this
!        symbol from the value table.
!
         LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1
         VALUE  = TABVAL(LOCVAL)

         CALL REMLAC ( 1, LOCVAL, TABVAL(1), NVAL )
         CALL SCARDC ( NVAL, TABVAL )

!
!        If this was the sole value for the symbol, remove the
!        symbol from the name and pointer tables. Otherwise just
!        decrement the dimension.
!
         IF ( TABPTR(LOCSYM) .EQ. 1 ) THEN

            CALL REMLAC ( 1, LOCSYM, TABSYM(1), NSYM )
            CALL SCARDC ( NSYM, TABSYM )

            CALL REMLAI ( 1, LOCSYM, TABPTR(1), NPTR )
            CALL SCARDI ( NPTR, TABPTR )

         ELSE
            TABPTR(LOCSYM) = TABPTR(LOCSYM) - 1
         END IF

      END IF


      CALL CHKOUT ( 'SYPOPC' )
      RETURN
      END
