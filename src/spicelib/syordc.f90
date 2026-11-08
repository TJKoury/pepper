!$Procedure SYORDC ( Order the components of a single symbol )

      SUBROUTINE SYORDC ( NAME, TABSYM, TABPTR, TABVAL )

! $ Abstract
!
!     Order the components of a single symbol in a character symbol
!     table. The components are ordered according to the ASCII collating
!     sequence.
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
!     NAME       I   Name of the symbol whose components are to be
!                    ordered.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose components are to be
!              ordered.
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
!              On output, the components of the symbol are sorted
!              according to ASCII collating sequence. If NAME is not in
!              the symbol table, the symbol table is not modified.
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
!     If the symbol NAME is not in the symbol table, the symbol table
!     is not modified.
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
!        CALL SYORDC ( 'EINSTEIN', TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        BOHR      -->   HYDROGEN ATOM
!        EINSTEIN  -->   BROWNIAN MOTION
!                        PHOTOELECTRIC EFFECT
!                        SPECIAL RELATIVITY
!        FERMI     -->   NUCLEAR FISSIONC
!
!
!     Note that the call,
!
!        CALL SYORDC ( 'MAXWELL', TABSYM, TABPTR, TABVAL )
!
!     will not modify the symbol table because the symbol "MAXWELL" is
!     not in the symbol table.
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
!     order the components of a single symbol
!
!-&


!
!     SPICELIB functions
!
      INTEGER               BSRCHC
      INTEGER               CARDC
      LOGICAL               RETURN
      INTEGER               SUMAI

!
!     Local variables
!
      INTEGER               NSYM

      INTEGER               LOCSYM
      INTEGER               LOCVAL
      INTEGER               N



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYORDC' )
      END IF

!
!     How many symbols?
!
      NSYM = CARDC ( TABSYM )

!
!     Is this symbol even in the table?
!
      LOCSYM = BSRCHC ( NAME, NSYM, TABSYM(1) )

!
!     If so, sort the components in place.
!
      IF ( LOCSYM .GT. 0 ) THEN

         LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1
         N      = TABPTR(LOCSYM)

         CALL SHELLC ( TABPTR(LOCSYM), TABVAL(LOCVAL) )

      END IF


      CALL CHKOUT ( 'SYORDC' )
      RETURN
      END
