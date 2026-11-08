!$Procedure SYFETI ( Fetch the Nth symbol in the table )

      SUBROUTINE SYFETI ( NTH, TABSYM, TABPTR, TABVAL, NAME, FOUND )

! $ Abstract
!
!     Fetch the Nth symbol in an integer symbol table.
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

      INTEGER               NTH
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      INTEGER               TABVAL     ( LBCELL:* )
      CHARACTER*(*)         NAME
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NTH        I   Index of symbol to be fetched.
!     TABSYM,
!     TABPTR,
!     TABVAL     I   Components of the symbol table.
!     NAME       O   Name of the NTH symbol in the symbol table.
!     FOUND      O   .TRUE. if the NTH symbol is in the symbol table.
!
! $ Detailed_Input
!
!     NTH      is the index of the symbol to be fetched. If the NTH
!              symbol does not exist, FOUND is .FALSE.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of an integer symbol table.
!              The NTH symbol may or may not be in the symbol
!              table. The symbol table is not modified by this
!              subroutine.
!
! $ Detailed_Output
!
!     NAME     is the name of the NTH symbol in the symbol table.
!
!     FOUND    is .TRUE. if the NTH symbol is in the symbol table.
!              If the NTH symbol is not in the table, FOUND is .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If there is an issue while reading the components of a
!         integer symbol table, an error is signaled by a routine in
!         the call tree of this routine. This normally indicates that
!         the integer symbol table is corrupted.
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
!         books   -->   5
!         erasers -->   6
!         pencils -->  12
!         pens    -->  10
!                      12
!                      24
!
!      The calls,
!
!      CALL SYFETI (  2,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
!      CALL SYFETI (  3,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
!      CALL SYFETI ( -1,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
!      CALL SYFETI (  6,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
!
!      result in the values for NAME and FOUND:
!
!         NAME        FOUND
!         ----------  -------
!         erasers      .TRUE.
!         pencils      .TRUE.
!                     .FALSE.
!                     .FALSE.
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
!        Edited the header to comply with NAIF standard. Added entry #1
!        in $Exceptions section.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) (NJB)
!
!-&


! $ Index_Entries
!
!     fetch the nth symbol in the table
!
!-&


! $ Revisions
!
!-     Beta Version 1.1.0, 17-FEB-1989 (NJB)
!
!         Declaration of the unused variable SUMAI removed.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               CARDC
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               NSYM



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYFETI' )
      END IF

!
!     How many symbols to start with?
!
      NSYM = CARDC ( TABSYM )

!
!     If the value of NTH is out of range, that's a problem.
!
      IF ( NTH .LT. 1  .OR.  NTH .GT. NSYM ) THEN

         FOUND = .FALSE.

!
!     Otherwise, we can proceed without fear of error. Merely locate
!     and return the appropriate component from the values table.
!
      ELSE

         FOUND = .TRUE.
         NAME  = TABSYM(NTH)

      END IF


      CALL CHKOUT ( 'SYFETI' )
      RETURN
      END
