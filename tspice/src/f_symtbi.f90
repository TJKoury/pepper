!$Procedure F_SYMTBI ( Test SPICELIB integer symbol table routines )

      SUBROUTINE F_SYMTBI ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the SPICELIB symbol table routines of type INTEGER.
!
! $ Required_Reading
!
!     SYMBOL
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Particulars
!
!     This routine tests the SPICELIB symbol table routines
!
!        SYDELI
!        SYDIMI
!        SYDUPI
!        SYENQI
!        SYFETI
!        SYGETI
!        SYNTHI
!        SYORDI
!        SYPOPI
!        SYPSHI
!        SYPUTI
!        SYRENI
!        SYSELI
!        SYSETI
!        SYTRNI
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    TSPICE Version 1.1.0, 18-JUL-2007 (NJB)
!
!        Bug fix: modified digit computation in utility
!        routine T_ITHSYM to guard against round-off
!        when taking a base 10 log of an integral power
!        of 10.
!
!-    TSPICE Version 1.0.0, 01-NOV-2005 (NJB)
!
!
!-&

! $ Revisions
!
!-    TSPICE Version 1.1.0, 18-JUL-2007 (NJB)
!
!        Bug fix: modified digit computation in utility
!        routine T_ITHSYM to guard against round-off
!        when taking a base 10 log of an integral power
!        of 10.
!
!        The new computation is
!
!           NDIGIT =  INT (  LOG10 ( DBLE(NMAX)+0.5D0 )  )  +  1
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 500 )

      INTEGER               MAXTAB
      PARAMETER           ( MAXTAB = 100 )

      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL = 10000 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 45 )

!
!     Local Variables
!
      CHARACTER*(NAMLEN)    NAME
      CHARACTER*(NAMLEN)    NEWNAM
      CHARACTER*(NAMLEN)    SYNAMS ( LBCELL : MAXTAB )
      CHARACTER*(NAMLEN)    XNAMES (          MAXTAB )

      CHARACTER*(MSGLEN)    TITLE

      INTEGER               I
      INTEGER               IVALS  (          MAXVAL )
      INTEGER               J
      INTEGER               K
      INTEGER               NVALS
      INTEGER               SIZE
      INTEGER               START
      INTEGER               SYPTRS ( LBCELL : MAXTAB )
      INTEGER               SYVALS ( LBCELL : MAXVAL )
      INTEGER               TABSIZ
      INTEGER               TO
      INTEGER               UB
      INTEGER               VAL
      INTEGER VAL_A1 ( 1 )
      EQUIVALENCE ( VAL, VAL_A1 )
      INTEGER               XCARD
      INTEGER               XPTRS  (          MAXTAB )
      INTEGER               XVAL
      INTEGER XVAL_A1 ( 1 )
      EQUIVALENCE ( XVAL, XVAL_A1 )
      INTEGER               XVALS  (          MAXVAL )

      LOGICAL               FOUND

!
!     Saved values
!
!     To avoid memory problems under cygwin, save everything.
!
      SAVE

!
!     Initial values
!

!
!     *********************************************************
!     Note:  the order of the test cases is significant in that
!     some cases depend on results from other cases.  Take care
!     to preserve side effects when modifying this code!
!     *********************************************************
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_SYMTBI' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYSETI test:  Populate a simple symbol table ' // &
                   'with scalar-valued symbols.'                   )

!
!     Initialize the symbol table.
!
      CALL SSIZEC ( MAXTAB, SYNAMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( MAXTAB, SYPTRS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( MAXVAL, SYVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, MAXTAB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME
         XPTRS(I)  =  1
         XVALS(I)  = -I

!
!        Associate the value -I with the Ith symbol.  Add this
!        symbol to the table.
!
         CALL SYSETI ( NAME, -I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check the structure of the cells we've populated.
!
!     The symbol name cell first:
!
      CALL CHCKSI ( 'Card(SYNAMS)', CARDC(SYNAMS), '=', MAXTAB, 0, OK )

      DO I = 1, CARDC(SYNAMS)

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, SYNAMS(I), '=', XNAMES(I), OK )

      END DO

!
!     Then the "pointer" cell.  The pointers are actually element
!     counts.
!
      CALL CHCKSI ( 'Card(SYPTRS)', CARDI(SYPTRS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYPTRS', SYPTRS(1), '=', XPTRS, MAXTAB, OK )

!
!     Then the value cell.
!
      CALL CHCKSI ( 'Card(SYVALS)', CARDI(SYVALS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYSETI test:  Populate a simple symbol table ' // &
                   'with scalar-valued symbols.  Insert symbols '  // &
                   'in reverse order.'                             )

!
!     Clear out the symbol table.
!
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

!
!     Use the symbol names and values from the previous test.
!
      DO I = 1, MAXTAB
!
!        Let J be the index of the symbol to insert.
!
         J = MAXTAB + 1 - I
!
!        Associate the value -J with the Jth symbol.  Add this
!        symbol to the table.
!
         CALL SYSETI ( XNAMES(J), -J, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     We should end up with the same symbol table as in the
!     previous test case.
!
!
!     Check the structure of the cells we've populated.
!
!     The symbol name cell first:
!
      CALL CHCKSI ( 'Card(SYNAMS)', CARDC(SYNAMS), '=', MAXTAB, 0, OK )

      DO I = 1, CARDC(SYNAMS)

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, SYNAMS(I), '=', XNAMES(I), OK )

      END DO

!
!     Then the "pointer" cell.  The pointers are actually element
!     counts.
!
      CALL CHCKSI ( 'Card(SYPTRS)', CARDI(SYPTRS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYPTRS', SYPTRS(1), '=', XPTRS, MAXTAB, OK )

!
!     Then the value cell.
!
      CALL CHCKSI ( 'Card(SYVALS)', CARDI(SYVALS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYPUTI test:  Populate a simple symbol table ' // &
                   'with scalar-valued symbols.'                   )

!
!     This is essentially a reprise of the previous SYSETI test.
!
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )


      DO I = 1, MAXTAB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME
         XPTRS(I)  =  1
         XVALS(I)  = -I

!
!        Associate the value -I with the Ith symbol.  Add this
!        symbol to the table.
!
         CALL SYPUTI ( NAME, XVALS(I), 1, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check the structure of the cells we've populated.
!
!     The symbol name cell first:
!
      CALL CHCKSI ( 'Card(SYNAMS)', CARDC(SYNAMS), '=', MAXTAB, 0, OK )

      DO I = 1, CARDC(SYNAMS)

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, SYNAMS(I), '=', XNAMES(I), OK )

      END DO

!
!     Then the "pointer" cell.  The pointers are actually element
!     counts.
!
      CALL CHCKSI ( 'Card(SYPTRS)', CARDI(SYPTRS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYPTRS', SYPTRS(1), '=', XPTRS, MAXTAB, OK )

!
!     Then the value cell.
!
      CALL CHCKSI ( 'Card(SYVALS)', CARDI(SYVALS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYFETI test:  Fetch names from symbol table ' // &
                   'with scalar-valued symbols.'                   )


      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYFETI( I, SYNAMS, SYPTRS, SYVALS, NAME, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL CHCKSC ( TITLE, NAME, '=', XNAMES(I), OK )

      END DO


!
!     Also look for a symbol we know isn't there.
!
      I     =  MAXTAB+1
      TITLE = 'Was name # found?'
      CALL REPMI  ( TITLE, '#',    I,      TITLE )

      CALL SYFETI( MAXTAB+1, SYNAMS, SYPTRS, SYVALS, NAME, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYGETI test:  Fetch values from symbol table ' // &
                   'with scalar-valued symbols.'                   )


      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYGETI( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, VAL_A1, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         TITLE = 'Value of symbol #'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL CHCKSI ( TITLE, VAL, '=', XVALS(I), 0, OK )

      END DO

!
!     Also look for a symbol we know isn't there.
!
      NAME = 'NOT_THERE'

      CALL SYGETI( NAME, &
                   SYNAMS, SYPTRS, SYVALS, NVALS, VAL_A1, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TITLE = 'Was name # found?'
      CALL REPMC  ( TITLE, '#',    NAME,   TITLE )
      CALL CHCKSL ( TITLE, FOUND, .FALSE., OK    )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYRENI test:  Rename symbols in a simple ' // &
                   'symbol table with scalar-valued symbols.' )


      DO I = 1, MAXTAB
!
!        Get the name and value of the ith symbol.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         CALL SYGETI ( NAME,  SYNAMS, SYPTRS, SYVALS, &
                       NVALS, XVAL_A1,   FOUND           )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

!
!        Create the replacement name:
!
         CALL REPLCH ( NAME, 'X', 'Y', NEWNAM )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Change the name of the symbol.
!
         CALL SYRENI ( NAME, NEWNAM, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Make sure the old symbol is gone.
!
         CALL SYGETI ( NAME,  SYNAMS, SYPTRS, SYVALS, &
                       NVALS, VAL_A1,    FOUND           )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'Was name # found?'
         CALL REPMC ( TITLE, '#', NAME, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )

!
!        Get the value associated with the new name.
!
!
!        Get the name and value of the ith symbol.
!
         CALL SYGETI ( NEWNAM,  SYNAMS, SYPTRS, SYVALS, &
                       NVALS,   VAL_A1,    FOUND           )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

!
!        Check the value.
!
         CALL CHCKSI ( 'VAL', VAL, '=', XVAL, 0, OK )

!
!        Change the name back to its original value.
!
         CALL SYRENI ( NEWNAM, NAME, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYDELI test:  Delete symbols from symbol table ' // &
                   'with scalar-valued symbols.'                   )

      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYDELI( XNAMES(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Make sure the symbol is gone.
!
         TITLE = 'Was (deleted symbol) name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYGETI( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, VAL_A1, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )

!
!        Validate the remaining symbol table.
!
         DO J = I+1, MAXTAB

            TITLE = 'Was (remaining symbol) name # found?'
            CALL REPMI ( TITLE, '#', I, TITLE )

            CALL SYGETI ( XNAMES(J), &
                          SYNAMS, SYPTRS, SYVALS, NVALS, VAL_A1, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

            TITLE = 'Remaining symbol # value'
            CALL REPMI ( TITLE, '#', I, TITLE )

            CALL CHCKSI ( TITLE, VAL, '=', XVALS(J), 0, OK )

         END DO

      END DO


!
!     At this point, we need to work with symbol tables containing
!     symbols having multiple values associated with them.  We'll
!     build a symbol table whose nth symbol has n associated values.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYPUTI test:  Create symbol table ' // &
                   'with array-valued symbols.'         )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )


      TO = 1

      DO I = 1, MAXTAB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME
         XPTRS(I)  = I
         START     = TO

         DO J = 1, I

            XVALS( TO )  =  - MAXTAB * I - J
            TO           =    TO + 1

         END DO

!
!        Add the symbol to the table.
!
         CALL SYPUTI ( NAME, XVALS(START), I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
!     Check the structure of the cells we've populated.
!
!     The symbol name cell first:
!
      CALL CHCKSI ( 'Card(SYNAMS)', CARDC(SYNAMS), '=', MAXTAB, 0, OK )

      DO I = 1, CARDC(SYNAMS)

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, SYNAMS(I), '=', XNAMES(I), OK )

      END DO

!
!     Then the "pointer" cell.  The pointers are actually element
!     counts.
!
      CALL CHCKSI ( 'Card(SYPTRS)', CARDI(SYPTRS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYPTRS', SYPTRS(1), '=', XPTRS, MAXTAB, OK )

!
!     Then the value cell.
!
      XCARD  =  MAXTAB * ( MAXTAB + 1 ) / 2

      CALL CHCKSI ( 'Card(SYVALS)', CARDI(SYVALS), '=', XCARD, 0, OK )

      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYPUTI test:  Populate a simple symbol table ' // &
                   'with scalar-valued symbols.  Insert symbols '  // &
                   'in reverse order.'                             )

!
!     Clear out the symbol table.
!
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

      CALL CLEARI ( MAXVAL, XVALS )

!
!     Use the symbol names and values from the previous test.
!
      SIZE  = MAXTAB * ( MAXTAB + 1 ) / 2

      START = SIZE - MAXTAB + 1

      DO I = MAXTAB, 1, -1
!
!        Add the symbol to the table.
!
         CALL SYPUTI ( XNAMES(I), XVALS(START), I, &
                       SYNAMS,    SYPTRS,       SYVALS )

         CALL CHCKXC ( .FALSE., ' ', OK )

         START = START - ( I - 1 )

      END DO

!
!     We should end up with the same symbol table as in the
!     previous test case.
!
!
!     Check the structure of the cells we've populated.
!
!     The symbol name cell first:
!
      CALL CHCKSI ( 'Card(SYNAMS)', CARDC(SYNAMS), '=', MAXTAB, 0, OK )

      DO I = 1, CARDC(SYNAMS)

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, SYNAMS(I), '=', XNAMES(I), OK )

      END DO

!
!     Then the "pointer" cell.  The pointers are actually element
!     counts.
!
      CALL CHCKSI ( 'Card(SYPTRS)', CARDI(SYPTRS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYPTRS', SYPTRS(1), '=', XPTRS, MAXTAB, OK )

!
!     Then the value cell.
!
      XCARD  =  MAXTAB * ( MAXTAB + 1 ) / 2

      CALL CHCKSI ( 'Card(SYVALS)', CARDI(SYVALS), '=', XCARD, 0, OK )

      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYGETI test:  validate array-valued symbol '// &
                   'table by fetching values associated with '  // &
                   'each symbol.'                               )


      START = 1

      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYGETI( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, IVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol #'
            CALL REPMI ( TITLE, '#', I, TITLE )

            CALL CHCKAI ( TITLE, IVALS, '=', XVALS(START), I, OK )

         END IF

         START = START + I

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYNTHI test:  validate array-valued symbol '// &
                   'table by fetching values associated with '  // &
                   'each symbol.'                               )


      START = 1

      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYGETI( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, IVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN
!
!           Get the size of the current symbol.
!
            SIZE = SYDIMI ( XNAMES(I), SYNAMS, SYPTRS, SYVALS )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO J = 1, SIZE
!
!              Fetch each value.
!
               TITLE = 'Value # of symbol #'
               CALL REPMI ( TITLE, '#', I,         TITLE )
               CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL SYNTHI( XNAMES(I), J,     SYNAMS, SYPTRS, SYVALS, &
                            VAL,       FOUND                          )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

               CALL CHCKSI ( TITLE, VAL, '=', XVALS(START+J-1), 0, OK )

            END DO

!
!           Try to fetch an element we know isn't there.
!
            J = SIZE + 1

            TITLE = 'Value # of symbol #'
            CALL REPMI ( TITLE, '#', J,         TITLE )
            CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL SYNTHI( XNAMES(I), J, SYNAMS, SYPTRS, SYVALS, &
                            VAL,       FOUND                   )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )

         END IF

         START = START + I

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYENQI test:  Create symbol table ' // &
                   'with array-valued symbols.'         )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )


      TO = 1

      DO I = 1, MAXTAB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME
         XPTRS(I)  = I

         DO J = 1, I

            XVALS( TO )  =  - MAXTAB * I - J

            CALL SYENQI ( NAME, XVALS(TO), SYNAMS, SYPTRS, SYVALS )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TO           =    TO + 1

         END DO

      END DO


!
!     Check the structure of the cells we've populated.
!
!     The symbol name cell first:
!
      CALL CHCKSI ( 'Card(SYNAMS)', CARDC(SYNAMS), '=', MAXTAB, 0, OK )

      DO I = 1, CARDC(SYNAMS)

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, SYNAMS(I), '=', XNAMES(I), OK )

      END DO

!
!     Then the "pointer" cell.  The pointers are actually element
!     counts.
!
      CALL CHCKSI ( 'Card(SYPTRS)', CARDI(SYPTRS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYPTRS', SYPTRS(1), '=', XPTRS, MAXTAB, OK )

!
!     Then the value cell.
!
      XCARD  =  MAXTAB * ( MAXTAB + 1 ) / 2

      CALL CHCKSI ( 'Card(SYVALS)', CARDI(SYVALS), '=', XCARD, 0, OK )

      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYPSHI test:  Create symbol table ' // &
                   'with array-valued symbols.'         )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )


      TO = 0

      DO I = 1, MAXTAB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME
         XPTRS(I)  = I
         TO        = TO + I

!
!        At the start of the loop, TO points to the element of XVALS
!        that will hold the last value associated with the Ith symbol.
!
         DO J = I, 1, -1

            XVALS( TO )  =  - MAXTAB * I - J

            CALL SYPSHI ( NAME, XVALS(TO), SYNAMS, SYPTRS, SYVALS )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TO           =    TO - 1

         END DO

         TO = TO + I

      END DO


!
!     Check the structure of the cells we've populated.
!
!     The symbol name cell first:
!
      CALL CHCKSI ( 'Card(SYNAMS)', CARDC(SYNAMS), '=', MAXTAB, 0, OK )

      DO I = 1, CARDC(SYNAMS)

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, SYNAMS(I), '=', XNAMES(I), OK )

      END DO

!
!     Then the "pointer" cell.  The pointers are actually element
!     counts.
!
      CALL CHCKSI ( 'Card(SYPTRS)', CARDI(SYPTRS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYPTRS', SYPTRS(1), '=', XPTRS, MAXTAB, OK )

!
!     Then the value cell.
!
      XCARD  =  MAXTAB * ( MAXTAB + 1 ) / 2

      CALL CHCKSI ( 'Card(SYVALS)', CARDI(SYVALS), '=', XCARD, 0, OK )

      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYDUPI test:  Create symbol table '         // &
                   'with array-valued symbols.  Then duplicate '// &
                   'each symbol.'                               )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )


!
!     We'll set the cardinality upper bound UB to MAXTAB/2, so
!     we'll have room for the duplicate symbols.
!
      TO = 1
      UB = MAXTAB/2

      DO I = 1, UB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, UB, NAME )

         XNAMES(I) = NAME
         XPTRS(I)  = I

         DO J = 1, I

            XVALS( TO )  =  - UB * I - J

            CALL SYENQI ( NAME, XVALS(TO), SYNAMS, SYPTRS, SYVALS )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TO           =    TO + 1

         END DO

      END DO

!
!     Validate the symbol table at this stage.
!
      START = 1

      DO I = 1, UB

         TITLE = 'Was name # found? (0)'
         CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )

         CALL SYGETI( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, IVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (0)'
            CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )

            CALL CHCKAI ( TITLE, IVALS, '=', XVALS(START), I, OK )

         END IF

         START = START + I

      END DO

!
!     Look up each symbol; add to the symbol table a duplicate symbol
!     with a new name.
!
      DO I = 1, UB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, UB, NAME )

         NEWNAM = NAME
         NEWNAM(1:2) = '2_'

         CALL SYDUPI ( NAME, NEWNAM, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Now validate the symbol table.  First make sure the original
!     symbols are intact.
!
      START = 1

      DO I = 1, UB

         TITLE = 'Was name # found? (1)'
         CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )

         CALL SYGETI( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, IVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (1)'
            CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )

            CALL CHCKAI ( TITLE, IVALS, '=', XVALS(START), I, OK )

         END IF

         START = START + I

      END DO

!
!     Now check the duplicate symbols.
!
      START = 1

      DO I = 1, UB

         NEWNAM = XNAMES(I)
         NEWNAM(1:2) = '2_'

         TITLE = 'Was name # found? (2)'
         CALL REPMC ( TITLE, '#', NEWNAM, TITLE )

         CALL SYGETI( NEWNAM, &
                      SYNAMS, SYPTRS, SYVALS, NVALS, IVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (2)'
            CALL REPMC ( TITLE, '#', NEWNAM, TITLE )

            CALL CHCKAI ( TITLE, IVALS, '=', XVALS(START), I, OK )

         END IF

         START = START + I

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYDUPI test:  Create symbol table '             // &
                   'with array-valued symbols.  Duplicate these '   // &
                   'symbols.  Negate the values of the original '   // &
                   'symbols.  Then duplicate the duplicate '        // &
                   'the symbols, overwriting the original symbols.' )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

!
!     We'll set the cardinality upper bound UB to MAXTAB/2, so
!     we'll have room for the duplicate symbols.
!
      TO = 1
      UB = MAXTAB/2

      DO I = 1, UB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, UB, NAME )

         XNAMES(I) = NAME
         XPTRS(I)  = I

         DO J = 1, I

            XVALS( TO )  =  - UB * I - J

            CALL SYENQI ( NAME, XVALS(TO), SYNAMS, SYPTRS, SYVALS )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TO           =    TO + 1

         END DO

      END DO

!
!     Look up each symbol; add to the symbol table a duplicate symbol
!     with a new name.
!
      DO I = 1, UB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, UB, NAME )

         NEWNAM = NAME
         NEWNAM(1:2) = '2_'

         CALL SYDUPI ( NAME, NEWNAM, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Negate the values of the symbols of the original symbols.
!
      DO I = 1, UB

         CALL SYGETI( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, IVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, NVALS
            IVALS(J) = -IVALS(J)
         END DO

         CALL SYPUTI( XNAMES(I),  IVALS,   NVALS, &
                      SYNAMS,     SYPTRS,  SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Duplicate the duplicate symbols, overwriting the originals.
!
      DO I = 1, UB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, UB, NAME )

         NEWNAM = NAME
         NEWNAM(1:2) = '2_'

         CALL SYDUPI ( NEWNAM, NAME, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Now validate the symbol table.  First make sure the
!     first set of duplicate symbols is intact.
!
      START = 1

      DO I = 1, UB

         CALL T_ITHSYM ( I, UB, NAME )

         NEWNAM = NAME
         NEWNAM(1:2) = '2_'

         TITLE = 'Was name # found? (1)'
         CALL REPMC ( TITLE, '#', NEWNAM, TITLE )

         CALL SYGETI( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, IVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (1)'
            CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )

            CALL CHCKAI ( TITLE, IVALS, '=', XVALS(START), I, OK )

         END IF

         START = START + I

      END DO

!
!     Now check the duplicated duplicate symbols.
!
      START = 1

      DO I = 1, UB

         TITLE = 'Was name # found? (2)'
         CALL REPMC ( TITLE, '#', NAME, TITLE )

         CALL SYGETI( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, IVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (2)'
            CALL REPMC ( TITLE, '#', NEWNAM, TITLE )

            CALL CHCKAI ( TITLE, IVALS, '=', XVALS(START), I, OK )

         END IF

         START = START + I

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYORDI test:  Create symbol table '      // &
                   'with array-valued symbols; sort values ' // &
                   'associated with each symbol.'            )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

      CALL CLEARI ( MAXVAL, XVALS )

!
!     Create the symbol table to be sorted.
!
      DO I = 1, MAXTAB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME

         DO J = 1, I

            VAL  =  - MAXTAB * I - J

            CALL SYENQI ( NAME, VAL, SYNAMS, SYPTRS, SYVALS )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

!
!     Sort the symbol values.
!
      DO I = 1, MAXTAB

         CALL SYORDI  ( XNAMES(I), SYNAMS, SYPTRS, SYVALS )

      END DO

!
!     Now we must validate the ordered symbol table.  Create
!     an array containing the expected values in the table.
!
      START = 1

      DO I = 1, MAXTAB

         DO J = 1, I
!
!           Fill in the values for the Ith symbol in the
!           opposite order from that in which they were inserted
!           into the symbol table.
!
            TO         =    START  + I - J

            XVALS(TO)  =  - MAXTAB * I - J

         END DO

         START = START + I

      END DO

!
!     Check the symbol table values against the value array.
!
      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYDIMI test:  Find dimensions of symbols in ' // &
                   'symbol table with array-valued symbols.'      )


      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

      CALL CLEARI ( MAXVAL, XVALS )


      DO I = 1, MAXTAB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME

         DO J = 1, I

            VAL  =  - MAXTAB * I - J

            CALL SYENQI ( NAME, VAL, SYNAMS, SYPTRS, SYVALS )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

      DO I = 1, MAXTAB

         NVALS =  SYDIMI( XNAMES(I), SYNAMS, SYPTRS, SYVALS  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'Dimension of symbol #'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL CHCKSI ( TITLE, NVALS, '=', I, 0, OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYPOPI test:  Pop values from symbol table ' // &
                   'with array-valued symbols.'                 )


!
!     Re-create the default array-valued symbol table.
!
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )


      DO I = 1, MAXTAB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

!
!        Set the values of the Ith symbol.
!
         DO J = 1, I

            IVALS(J)  =  - MAXTAB * I - J

         END DO

!
!        Insert the Ith symbol.
!
         CALL SYPUTI ( NAME, IVALS, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
!     Now test SYPOPI.
!
      TO = 1

      DO I = 1, MAXTAB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XPTRS(I)  = I

         DO J = 1, I
!
!           Set the expected value.
!
            XVALS( TO )  =  - MAXTAB * I - J

            CALL SYPOPI ( NAME, SYNAMS, SYPTRS, SYVALS, VAL, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TITLE = 'Was value # of symbol name # found?'
            CALL REPMI ( TITLE, '#', J,    TITLE )
            CALL REPMC ( TITLE, '#', NAME, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

            IF ( FOUND ) THEN

               TITLE = 'Symbol #: popped value at index #'
               CALL REPMC ( TITLE, '#', NAME, TITLE )
               CALL REPMI ( TITLE, '#', J,    TITLE )

               CALL CHCKSI ( TITLE, VAL, '=', XVALS(TO), 0, OK )

            END IF

            TO    =    TO + 1

         END DO

!
!        Try to pop a value from the now empty symbol.
!
         CALL SYPOPI ( NAME, SYNAMS, SYPTRS, SYVALS, VAL, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'Was element popped from symbol #?'
         CALL REPMC ( TITLE, '#', NAME, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYTRNI test:  Transpose values from symbol ' // &
                   'table with array-valued symbols.'            )

!
!     We'll create a small version of our array-valued symbol table.
!     Since we want to try all possible transpositions, there would
!     be an excessive number of calls to SYTRNI if we used the full
!     set of MAXTAB symbols.
!
!     Create the symbol table now.
!
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

      TABSIZ = 5

      TO     = 1


      DO I = 1, TABSIZ
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, TABSIZ, NAME )

!
!        Set the values of the Ith symbol.
!
         START = TO

         DO J = 1, I

            XVALS( TO )  =  - TABSIZ * I - J
            TO           =    TO + 1

         END DO

!
!        Insert the Ith symbol.
!
         CALL SYPUTI ( NAME,  XVALS(START), I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Now try the transpositions.
!
      START = 1

      DO I = 1, TABSIZ

         SIZE = I
!
!        Try every possible transposition of the elements of the Ith
!        symbol, including those where the indices of the transposed
!        elements are not distinct.
!
         CALL T_ITHSYM ( I, TABSIZ, NAME )

!
!        Transpose elements J and K of the Ith symbol, for all
!        applicable J, K.
!
         DO J = 1, SIZE

            DO K = 1, SIZE

               CALL SYTRNI ( NAME, J, K, SYNAMS, SYPTRS, SYVALS )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Fetch the values of the modified symbol.
!
               CALL SYGETI ( NAME,  SYNAMS, SYPTRS, SYVALS, &
                             NVALS, IVALS,  FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

               IF ( FOUND ) THEN
!
!                 If IVALS reflects the correct transposition,
!                 then we can swap elements J and K of IVALS
!                 to obtain the original symbol's values.
!
                  IF ( J .NE. K ) THEN
                     CALL SWAPI ( IVALS(J), IVALS(K) )
                  END IF

                  TITLE = 'Values of symbol #:  J = #; K = #'
                  CALL REPMC  ( TITLE, '#', NAME, TITLE )
                  CALL REPMI  ( TITLE, '#', J,    TITLE )
                  CALL REPMI  ( TITLE, '#', K,    TITLE )

                  CALL CHCKAI ( TITLE, IVALS, '=', XVALS(START), I, OK )

               END IF

!
!              Undo the transposition in the symbol table.
!
               CALL SYTRNI ( NAME, J, K, SYNAMS, SYPTRS, SYVALS )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END DO

         END DO

!
!        Set the start index for the next symbol.
!
         START = START + I

      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYSELI test:  Extract slice of values from ' // &
                   'symbol table with array-valued symbols.'     )

!
!     This approach used in this test case closely parallels that
!     of the SYTRNI test case above.
!
!     We'll create a small version of our array-valued symbol table.
!     Since we want to try all possible slices, there would
!     be an excessive number of calls to SYTRNI if we used the full
!     set of MAXTAB symbols.
!
!     Create the symbol table now.
!
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

      CALL CLEARI ( MAXVAL, XVALS )

      TABSIZ = 5

      TO     = 1


      DO I = 1, TABSIZ
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, TABSIZ, NAME )

!
!        Set the values of the Ith symbol.
!
         START = TO

         DO J = 1, I

            XVALS( TO )  =  - TABSIZ * I - J
            TO           =    TO + 1

         END DO

!
!        Insert the Ith symbol.
!
         CALL SYPUTI ( NAME,  XVALS(START), I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Now try the slice extractions.
!
      START = 1

      DO I = 1, TABSIZ

         SIZE = I
!
!        Try every possible slice of the elements of the Ith
!        symbol, including those where the endpoint indices of the
!        slice are not distinct.
!
         CALL T_ITHSYM ( I, TABSIZ, NAME )

!
!        Extrace the slice ranging from elements J to K of the Ith
!        symbol, for all applicable J, K.
!
         DO J = 1, SIZE

            DO K = 1, SIZE
!
!              Fetch the slice from J to K.  We expect to find a
!              result only if K >= J.
!
               CALL SYSELI ( NAME,   J,      K, &
                             SYNAMS, SYPTRS, SYVALS, IVALS, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( K .GE. J ) THEN
                   CALL CHCKSL ( TITLE, FOUND, .TRUE.,  OK )
               ELSE
                   CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )
               END IF

               IF ( FOUND ) THEN

                  TITLE = 'Slice #(#:#)'
                  CALL REPMC  ( TITLE, '#', NAME, TITLE )
                  CALL REPMI  ( TITLE, '#', J,    TITLE )
                  CALL REPMI  ( TITLE, '#', K,    TITLE )

                  CALL CHCKAI ( TITLE, &
                                IVALS, &
                                '=', &
                                XVALS(START+J-1), &
                                K-J+1, &
                                OK               )
               END IF

            END DO

         END DO

!
!        Set the start index for the next symbol.
!
         START = START + I

      END DO


!
!     Now for some error handling tests.
!
!
!     SYDELI:  No errors are detected by this routine.
!
!     SYDIMI:  No errors are detected by this routine.
!
!
!     SYDUPI:
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYDUPI:  symbol name not present' )

      CALL SYDUPI ( 'NOSYMBOL', 'NOSYMBOL2', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHSYMBOL)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYDUPI:  name table overflow' )

!
!     We'll create a small symbol table so we can test the handling of
!     overflow conditions.
!
      TABSIZ = 5

      CALL SSIZEC ( TABSIZ,   SYNAMS )
      CALL SSIZEI ( 2*TABSIZ, SYPTRS )
      CALL SSIZEI ( 2*TABSIZ, SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SYDUPI ( NAME, 'NEWNAME', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NAMETABLEFULL)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYDUPI:  pointer table overflow' )

      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ,    SYPTRS )
      CALL SSIZEI ( TABSIZ+2,  SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SYDUPI ( NAME, 'NEWNAME', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(POINTERTABLEFULL)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYDUPI:  value table overflow' )


      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ+1,  SYPTRS )
      CALL SSIZEI ( TABSIZ,    SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SYDUPI ( NAME, 'NEWNAME', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )

!
!
!     SYENQI:
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYENQI:  value table overflow' )


      TABSIZ = 5

      CALL SSIZEC ( TABSIZ,  SYNAMS )
      CALL SSIZEI ( TABSIZ,  SYPTRS )
      CALL SSIZEI ( TABSIZ,  SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Enqueue a value onto the last symbol.
!
      CALL SYENQI ( NAME, -1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )

!
!
!
!     SYFETI:  No errors are detected by this routine.
!
!     SYGETI:  No errors are detected by this routine.
!
!     SYNTHI:  No errors are detected by this routine.
!
!     SYORDI:  No errors are detected by this routine.
!
!     SYPOPI:  No errors are detected by this routine.
!
!
!
!     SYPSHI:
!
! --- Case: ------------------------------------------------------
!
!
      CALL TCASE  ( 'SYPSHI:  value table overflow' )


      TABSIZ = 5

      CALL SSIZEC ( TABSIZ,  SYNAMS )
      CALL SSIZEI ( TABSIZ,  SYPTRS )
      CALL SSIZEI ( TABSIZ,  SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Push a value onto the last symbol.
!
      CALL SYPSHI ( NAME, -1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )




!
!     SYPUTI:
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYPUTI:  name table overflow' )

!
!     We'll create a small symbol table so we can test the handling of
!     overflow conditions.
!
      TABSIZ = 5

      CALL SSIZEC ( TABSIZ,   SYNAMS )
      CALL SSIZEI ( 2*TABSIZ, SYPTRS )
      CALL SSIZEI ( 2*TABSIZ, SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      CALL SYPUTI ( 'NEWNAME', [I], 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NAMETABLEFULL)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYPUTI:  pointer table overflow' )

      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ,    SYPTRS )
      CALL SSIZEI ( TABSIZ+2,  SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SYPUTI ( 'NEWNAME', [I], 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(POINTERTABLEFULL)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYPUTI:  value table overflow' )


      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ+1,  SYPTRS )
      CALL SSIZEI ( TABSIZ,    SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SYPUTI ( 'NEWNAME', [I], 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYPUTI: invalid value count' )


      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ+1,  SYPTRS )
      CALL SSIZEI ( TABSIZ+1,  SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SYPUTI ( 'NEWNAME', [I], -1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARGUMENT)', OK )


!
!     SYRENI:
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYRENI:  rename non-existent symbol' )

      CALL SYRENI ( 'NONNAME', 'NEWNAME', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHSYMBOL)', OK )



!
!
!     SYSELI:  No errors are detected by this routine.
!
!
!     SYSETI:
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYSETI:  name table overflow' )

!
!     We'll create a small symbol table so we can test the handling of
!     overflow conditions.
!
      TABSIZ = 5

      CALL SSIZEC ( TABSIZ,   SYNAMS )
      CALL SSIZEI ( 2*TABSIZ, SYPTRS )
      CALL SSIZEI ( 2*TABSIZ, SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      CALL SYSETI ( 'NEWNAME', I, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NAMETABLEFULL)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYSETI:  pointer table overflow' )

      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ,    SYPTRS )
      CALL SSIZEI ( TABSIZ+2,  SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SYSETI ( 'NEWNAME', I, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(POINTERTABLEFULL)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYSETI:  value table overflow' )


      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ+1,  SYPTRS )
      CALL SSIZEI ( TABSIZ,    SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SYSETI ( 'NEWNAME', I, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )


!     SYTRNI:
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYTRNI:  first index < 1' )


      CALL SSIZEC ( TABSIZ,  SYNAMS )
      CALL SSIZEI ( TABSIZ,  SYPTRS )
      CALL SSIZEI ( TABSIZ,  SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XNAMES(I) = NAME
         XPTRS (I) = 1
         XVALS (I) = I

      END DO

      CALL SYTRNI ( NAME, -1, 2, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYTRNI:  first index > second index' )

      CALL SYTRNI ( NAME, 2, 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYTRNI:  second index > symbol size' )

      CALL SYTRNI ( NAME, 1, TABSIZ+1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYTRNI:  non-error case: attempt transposition '// &
                    'of values of non-existent symbol.'             )

      CALL SYTRNI ( 'NONAME', 1, 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure the symbol table wasn't touched.
!
!     Check the structure of the cells we've populated.
!
!     The symbol name cell first:
!
      CALL CHCKSI ( 'Card(SYNAMS)', CARDC(SYNAMS), '=', TABSIZ, 0, OK )

      DO I = 1, CARDC(SYNAMS)

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, SYNAMS(I), '=', XNAMES(I), OK )

      END DO

!
!     Then the "pointer" cell.  The pointers are actually element
!     counts.
!
      CALL CHCKSI ( 'Card(SYPTRS)', CARDI(SYPTRS), '=', TABSIZ, 0, OK )

      CALL CHCKAI ( 'SYPTRS', SYPTRS(1), '=', XPTRS, TABSIZ, OK )

!
!     Then the value cell.
!
      CALL CHCKSI ( 'Card(SYVALS)', CARDI(SYVALS), '=', TABSIZ, 0, OK )

      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, TABSIZ, OK )


!
!     That's all, folks!
!

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END



!
!     The utility routine T_ITHSYM creates the name of the ith
!     symbol.  The names created by this routine have the form
!
!        XXX...XXXnnn
!
!     where nnn is the input number N, printed with a sufficient
!     number of leading zeros so that all numbers in the range
!     1:NMAX have the same width.  The number is padded with
!     leading 'X' characters up to the length of the string NAME.
!
      SUBROUTINE T_ITHSYM ( N, NMAX, NAME )
      use pepper_legacy
      IMPLICIT NONE

      INTEGER               N
      INTEGER               NMAX
      CHARACTER*(*)         NAME

!
!     SPICELIB functions
!

!
!     Local parameters
!
      INTEGER               PICLEN
      PARAMETER           ( PICLEN = 11 )

!
!     Local variables
!
      CHARACTER*(PICLEN)    PICTUR

      INTEGER               I
      INTEGER               NPAD
      INTEGER               NPICT
      INTEGER               SLEN
      INTEGER               NDIGIT

!
!     Get the length of the output name.
!
      SLEN  =  LEN(NAME)

!
!     Determine the number of digits required.
!
      NDIGIT =  INT (  LOG10 ( DBLE(NMAX)+0.5D0 )  )  +  1

!
!     Create the format picture for the names.
!
      PICTUR = '0'
      NPICT  = MIN ( SLEN, NDIGIT )

      DO I = 2, NPICT
         PICTUR(I:I) = 'X'
      END DO

      CALL DPFMT ( DBLE(N), PICTUR, NAME )

!
!     Add non-blank leading padding to the name.
!
      NPAD = SLEN - NPICT

      IF ( NPAD .GT. 0 ) THEN

         CALL RJUST ( NAME, NAME )

         DO I = 1, NPAD
            NAME(I:I) = 'X'
         END DO

      END IF

      END


