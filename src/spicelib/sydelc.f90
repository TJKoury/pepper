!$Procedure SYDELC ( Delete a symbol from the symbol table )

      SUBROUTINE SYDELC ( NAME, TABSYM, TABPTR, TABVAL )

! $ Abstract
!
!     Delete a symbol from a character symbol table. The symbol and its
!     associated values are deleted.
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

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol to be deleted.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol to be deleted from the symbol
!              table.
!
!              If the symbol does not exist, the symbol table remains
!              unchanged. This subroutine is case sensitive. NAME must
!              the symbol exactly.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a character symbol table.
!
!              On input, the table may or may not contain the symbol
!              NAME.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a character symbol table.
!
!              On output, the symbol table no longer contains the symbol
!              NAME or its associated values. If NAME is not a symbol,
!              the components of the symbol table remain unchanged.
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
!     None.
!
! $ Examples
!
!     In the following example the subroutine SYDELC is used to delete
!     the symbol "BOHR" and its values from the symbol table.
!
!     The contents of the symbol table are:
!
!        BOHR      -->   HYDROGEN ATOM
!        EINSTEIN  -->   SPECIAL RELATIVITY
!                        PHOTOELECTRIC EFFECT
!                        BROWNIAN MOTION
!        FERMI     -->   NUCLEAR FISSION
!
!     The call
!
!        CALL SYDELC ( 'BOHR', TABSYM, TABPTR, TABVAL )
!
!     deletes the symbol "BOHR" from the symbol table. The components
!     of the symbol table on output are:
!
!        EINSTEIN  -->   SPECIAL RELATIVITY
!                        PHOTOELECTRIC EFFECT
!                        BROWNIAN MOTION
!        FERMI     -->   NUCLEAR FISSION
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
!     delete a symbol from a symbol table
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
      INTEGER               DIMVAL



!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYDELC' )
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
!     If it's not in the table, we're done. If it is, we can proceed
!     without fear of overflow.
!
      IF ( LOCSYM .GT. 0 ) THEN

         LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1
         DIMVAL = TABPTR(LOCSYM)

         CALL REMLAC ( 1, LOCSYM, TABSYM(1), NSYM )
         CALL SCARDC ( NSYM, TABSYM )

         CALL REMLAI ( 1, LOCSYM, TABPTR(1), NPTR )
         CALL SCARDI ( NPTR, TABPTR )

         CALL REMLAC ( DIMVAL, LOCVAL, TABVAL(1), NVAL )
         CALL SCARDC ( NVAL, TABVAL )

      END IF


      CALL CHKOUT ( 'SYDELC' )
      RETURN
      END
