!$Procedure SYDIMC ( Return the dimension of a symbol )

      INTEGER FUNCTION SYDIMC ( NAME, TABSYM, TABPTR, TABVAL )

! $ Abstract
!
!     Return the dimension of a particular symbol in a character symbol
!     table. If the symbol is not found, the function returns the value
!     zero.
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
!     NAME       I   Name of the symbol whose dimension is desired.
!     TABSYM,
!     TABPTR,
!     TABVAL     I   Components of the symbol table.
!
!     The function returns the dimension of the symbol NAME. If NAME is
!     not in the symbol table, the function returns the value zero.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose dimension is to be
!              returned. If the symbol is not in the symbol table, the
!              function returns the value zero. This function is case
!              sensitive, NAME must match a symbol exactly.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a character symbol table. The table
!              may or may not contain the symbol NAME.
!
! $ Detailed_Output
!
!     The function returns the dimension of the symbol NAME. The
!     dimension of a symbol is the number of values associated with
!     that symbol. If NAME is not in the symbol table, the function
!     returns the value zero.
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
!     The contents of the symbol table are:
!
!        BOHR      -->   HYDROGEN ATOM
!        EINSTEIN  -->   SPECIAL RELATIVITY
!                        PHOTOELECTRIC EFFECT
!                        BROWNIAN MOTION
!        FERMI     -->   NUCLEAR FISSION
!
!
!     Perhaps we want to know how many subjects are associated with
!     certain scientists. The following code returns the values of
!     NUMSUB indicated in the table.
!
!     NUMSUB = SYDIMC ( 'EINSTEIN', TABSYM, TABPTR, TABVAL )
!     NUMSUB = SYDIMC ( 'BOHR',     TABSYM, TABPTR, TABVAL )
!     NUMSUB = SYDIMC ( 'FERMI',    TABSYM, TABPTR, TABVAL )
!     NUMSUB = SYDIMC ( 'MILLIKAN', TABSYM, TABPTR, TABVAL )
!
!
!     ----SYMBOL----------NUMSUB------
!     | EINSTEIN     |       3       |
!     | BOHR         |       1       |
!     | FERMI        |       1       |
!     | MILLIKAN     |       0       |
!     --------------------------------
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
!-    SPICELIB Version 1.2.0, 07-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Updated
!        $Brief_I/O to indicate that TABSYM, TABPTR, TABVAL are input
!        arguments.
!
!-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
!
!        If the value of the function RETURN is .TRUE. upon execution of
!        this module, this function is assigned a default value of
!        either 0, 0.0D0, .FALSE., or blank depending on the type of
!        the function.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!         Comment section for permuted index source lines was added
!         following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
!
!-&


! $ Index_Entries
!
!     fetch the dimension of a symbol
!
!-&


!
!     SPICELIB functions
!
      INTEGER               BSRCHC
      INTEGER               CARDC
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               NSYM
      INTEGER               LOCSYM



!
!     Standard SPICE error handling
!
      IF ( RETURN() ) THEN
         SYDIMC = 0
         RETURN
      ELSE
         CALL CHKIN ( 'SYDIMC' )
      END IF

!
!     How many symbols to start with?
!
      NSYM = CARDC ( TABSYM )

!
!     Is this symbol even in the table?
!
      LOCSYM = BSRCHC ( NAME, NSYM, TABSYM(1) )

!
!     If it's not in the table, return zero. Otherwise, look up
!     the dimension directly.
!
      IF ( LOCSYM .EQ. 0 ) THEN
         SYDIMC = 0

      ELSE
         SYDIMC = TABPTR(LOCSYM)
      END IF


      CALL CHKOUT ( 'SYDIMC' )
      RETURN
      END
