!$Procedure SYPSHC ( Push a value onto a particular symbol )

      SUBROUTINE SYPSHC ( NAME, VALUE, TABSYM, TABPTR, TABVAL )

! $ Abstract
!
!     Push a value onto a particular symbol in a character symbol table.
!     The previous value(s) associated with the symbol is extended at
!     the front. A new symbol is created if necessary.
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
      CHARACTER*(*)         VALUE
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      CHARACTER*(*)         TABVAL     ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol onto which the value is to be
!                    pushed.
!     VALUE      I   Value that is to be pushed onto the symbol NAME.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol onto which the value is to
!              be pushed.
!
!              If NAME is not in the symbol table, a new symbol is
!              created.
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
!              The value is added to the symbol table at the front of
!              the previous value(s) associated with the symbol NAME. If
!              NAME is not originally in the symbol table, a new symbol
!              is created.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the addition of the new value to the symbol table causes an
!         overflow in the value table, the error SPICE(VALUETABLEFULL)
!         is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     If the symbol NAME is not in the symbol table, a new symbol
!     is created.
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
!        PAULI     -->   EXCLUSION PRINCIPLE
!
!     The call,
!
!        CALL SYPSHC ( 'PAULI', 'NEUTRINO', TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        BOHR      -->   HYDROGEN ATOM
!        EINSTEIN  -->   SPECIAL RELATIVITY
!                        PHOTOELECTRIC EFFECT
!                        BROWNIAN MOTION
!        FERMI     -->   NUCLEAR FISSION
!        PAULI     -->   NEUTRINO
!                        EXCLUSION PRINCIPLE
!
!     The next call,
!
!        CALL SYPSHC ( 'MILLIKAN', 'PHOTOELECTRIC EFFECT',
!       .               TABSYM,     TABPTR,                TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        BOHR      -->   HYDROGEN ATOM
!        EINSTEIN  -->   SPECIAL RELATIVITY
!                        PHOTOELECTRIC EFFECT
!                        BROWNIAN MOTION
!        FERMI     -->   NUCLEAR FISSION
!        MILLIKAN  -->   PHOTOELECTRIC EFFECT
!        PAULI     -->   NEUTRINO
!                        EXCLUSION PRINCIPLE
!
!     Note that a new symbol "MILLIKAN" was created by the last call.
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
!     push a value onto a particular symbol
!
!-&


!
!     SPICELIB functions
!
      INTEGER               CARDC
      INTEGER               LSTLEC
      LOGICAL               RETURN
      INTEGER               SIZEC
      INTEGER               SUMAI

!
!     Local variables
!
      INTEGER               NSYM
      INTEGER               NVAL

      INTEGER               LOCSYM
      INTEGER               LOCVAL
      LOGICAL               OLDSYM


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYPSHC' )
      END IF

!
!     How many symbols to start with?
!
      NSYM = CARDC ( TABSYM )
      NVAL = CARDC ( TABVAL )

!
!     Where does this symbol belong? Is it already in the table?
!
      LOCSYM = LSTLEC ( NAME, NSYM, TABSYM(1) )
      OLDSYM = ( LOCSYM .NE. 0  .AND.  TABSYM(LOCSYM) .EQ. NAME )

!
!     If it's not already in the table, use SET to create a brand new
!     symbol.
!
      IF ( .NOT. OLDSYM ) THEN

         CALL SYSETC ( NAME, VALUE, TABSYM, TABPTR, TABVAL )

!
!     If it is in the table, we can't proceed unless we know that we
!     have enough room for one extra addition in the value table.
!
      ELSE IF (  NVAL .GE. SIZEC ( TABVAL ) ) THEN

         CALL SETMSG ( 'SYPSHC: The addition of the value $ to the '  // &
                       'symbol # causes an overflow in the value '    // &
                       'table.' )
         CALL ERRCH  ( '$', VALUE )
         CALL ERRCH  ( '#', NAME  )
         CALL SIGERR ( 'SPICE(VALUETABLEFULL)' )


!
!     If there's room, add the new value to the value table. Add one
!     to the dimension, and put the value in the right place.
!
      ELSE

         LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1

         CALL INSLAC ( VALUE, 1, LOCVAL, TABVAL(1), NVAL )
         CALL SCARDC ( NVAL, TABVAL )

         TABPTR(LOCSYM) = TABPTR(LOCSYM) + 1

      END IF


      CALL CHKOUT ( 'SYPSHC' )
      RETURN
      END
