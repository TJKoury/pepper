!$Procedure SYSELC ( Select a subset of the values of a symbol )

      SUBROUTINE SYSELC ( NAME, &
                          BEGIN, &
                          END, &
                          TABSYM, TABPTR, TABVAL, &
                          VALUES, &
                          FOUND                     )

! $ Abstract
!
!     Select a subset of the values associated with a particular
!     symbol in a character symbol table.
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
      INTEGER               BEGIN
      INTEGER               END
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      CHARACTER*(*)         TABVAL     ( LBCELL:* )
      CHARACTER*(*)         VALUES     (        * )
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol whose associated values are to
!                    be returned.
!     BEGIN      I   Index of the first associated value to be returned.
!     END        I   Index of the last associated value to be returned.
!     TABSYM,
!     TABPTR,
!     TABVAL     I   Components of the symbol table.
!     VALUES     O   Subset of the values associated with the symbol
!                    NAME.
!     FOUND      O   .TRUE. if the subset of values exists.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose subset of associated
!              values to be returned. If NAME is not in the symbol
!              table, FOUND is .FALSE.
!
!     BEGIN    is the index of the first associated value to be
!              returned. If BEGIN is out of range (BEGIN < 1 or
!              BEGIN > END) FOUND is .FALSE.
!
!     END      is the index of the last associated value to be
!              returned. If END is out of range (END < 1 or
!              END > is greater than the dimension of NAME)
!              FOUND is .FALSE.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are components of the character symbol table.
!
! $ Detailed_Output
!
!     VALUES   is a subset of the values associated with the
!              symbol NAME. If the subset specified by BEGIN and
!              END exists, as many values as will fit in VALUES
!              are returned. If the subset does not exist, no
!              values are returned and FOUND is .FALSE.
!
!     FOUND    is .TRUE. if the subset of values is exists.
!              FOUND is .FALSE. if BEGIN < 1, BEGIN > END, END < 1,
!              END > the dimension of NAME, or NAME is not
!              in the symbol table.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If there is an issue while reading the components of a
!         character symbol table, an error is signaled by a routine in
!         the call tree of this routine. This normally indicates that
!         the character symbol table is corrupted.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     FOUND will be false if the bounds of the subset specified by
!     BEGIN and END are out of range. Values of BEGIN and END that
!     specify bounds out of range are BEGIN < 1, BEGIN > END,
!     END < 1, or END > the dimension of NAME. FOUND is also false
!     if the symbol NAME is not in the symbol table.
!
! $ Examples
!
!     The contents of the symbol table are:
!
!        BOHR      -->   HYDROGEN ATOM
!        EINSTEIN  -->   SPECIAL RELATIVITY
!                        PHOTOELECTRIC EFFECT
!                        BROWNIAN MOTION
!                        GENERAL RELATIVITY
!        FERMI     -->   NUCLEAR FISSION
!        PAULI     -->   EXCLUSION PRINCIPLE
!                        NEUTRINO
!
!     Let the dimension of the array VALUES be 4.
!
!
!     The ouput values of VALUES and FOUND for the input values of
!     NAME, BEGIN, and END are contained in this table:
!
!        NAME         BEGIN    END        VALUES               FOUND
!        -----------  -----    ---    ---------------------   -------
!        EINSTEIN       2       3     PHOTOELECTRIC EFFECT     .TRUE.
!                                     BROWNIAN MOTION
!
!        EINSTEIN       1       4     SPECIAL RELATIVITY       .TRUE.
!                                     PHOTOELECTRIC EFFECT
!                                     BROWNIAN MOTION
!                                     GENERAL RELATIVITY
!
!        MAXWELL        1       5                             .FALSE.
!
!        PAULI          2       1                             .FALSE.
!
!        PAULI          1      -2                             .FALSE.
!
!        BOHR           1       5                             .FALSE.
!        ------------------------------------------------------------
!
!
!     Note that FOUND is .FALSE. for examples 3 through 6 because:
!
!     -  In the 3rd example, the symbol 'MAXWELL' is not in the symbol
!        table.
!
!     -  In the 4th example, BEGIN > END.
!
!     -  In the 5th example, END < 0.
!
!     -  In the 6th example, END is greater than the dimension of the
!        symbol 'BOHR'.
!
! $ Restrictions
!
!     1)  This subroutine does not check to see if the output array
!         VALUES is large enough to hold the selected set of values.
!         The caller must provide the required space.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Moved entry
!        from $Exceptions to $Restrictions and added entry #1 in
!        $Exceptions.
!
!-    SPICELIB Version 1.0.2, 03-NOV-2005 (NJB)
!
!        Various header corrections were made. In particular,
!        the header no longer asserts that this routine will
!        "return as many values as will fit" in the output array
!        VALUES.
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
!     select a subset of the values of a symbol
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
         CALL CHKIN ( 'SYSELC' )
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
!     If it's not in the table, it's definitely a problem.
!
      IF ( LOCSYM .EQ. 0 ) THEN

         FOUND = .FALSE.

      ELSE

!
!        We could still have a problem: do these components exist?
!        Does this request even make sense?
!
         N = TABPTR(LOCSYM)

         IF (       BEGIN .GE. 1 &
              .AND. BEGIN .LE. N &
              .AND. END   .GE. 1 &
              .AND. END   .LE. N &
              .AND. BEGIN .LE. END ) THEN

            FOUND  = .TRUE.
            LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1

            CALL MOVEC ( TABVAL(LOCVAL+BEGIN-1), END-BEGIN+1, VALUES )

         ELSE
            FOUND = .FALSE.
         END IF

      END IF


      CALL CHKOUT ( 'SYSELC' )
      RETURN
      END
