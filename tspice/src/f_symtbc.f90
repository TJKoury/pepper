!$Procedure F_SYMTBC ( Test SPICELIB character symbol table routines )

      SUBROUTINE F_SYMTBC ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the SPICELIB symbol table routines of type
!     CHARACTER.
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
!        SYDELC
!        SYDIMC
!        SYDUPC
!        SYENQC
!        SYFETC
!        SYGETC
!        SYNTHC
!        SYORDC
!        SYPOPC
!        SYPSHC
!        SYPUTC
!        SYRENC
!        SYSELC
!        SYSETC
!        SYTRNC
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
!-    TSPICE Version 1.0.0, 01-NOV-2005 (NJB)
!
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

      INTEGER               VALLEN
      PARAMETER           ( VALLEN = 50 )

!
!     Local Variables
!
      CHARACTER*(VALLEN)    CVALS  (          MAXVAL )
      CHARACTER*(VALLEN)    IVS    (          MAXVAL )
      CHARACTER*(NAMLEN)    NAME
      CHARACTER*(VALLEN)    NIVS   (          MAXVAL )
      CHARACTER*(NAMLEN)    NEWNAM
      CHARACTER*(NAMLEN)    SYNAMS ( LBCELL : MAXTAB )
      CHARACTER*(NAMLEN)    XNAMES (          MAXTAB )
      CHARACTER*(VALLEN)    SYVALS ( LBCELL : MAXVAL )
      CHARACTER*(VALLEN)    VAL
      CHARACTER*(VALLEN)    XVAL
      CHARACTER*(VALLEN)    XVALS  (          MAXVAL )

      CHARACTER*(MSGLEN)    TITLE

      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               NVALS
      INTEGER               SIZE
      INTEGER               START
      INTEGER               SYPTRS ( LBCELL : MAXTAB )
      INTEGER               TABSIZ
      INTEGER               TO
      INTEGER               UB
      INTEGER               XCARD
      INTEGER               XPTRS  (          MAXTAB )

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
      CALL TOPEN ( 'F_SYMTBC' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup:  create arrays of values that take ' // &
                   'the place of the numeric values used in '   // &
                   'tests for the integer and d.p. symbol '     // &
                   'table routines.'                            )

!
!     We use the symbol name creation utility to create the values.
!
!
!     The array IVS contains values that replace integers.  Note
!     that these values are *not* integers in string form.
!
      DO I = 1, MAXVAL

         CALL T_ITHSYM ( I, MAXVAL, IVS(I) )

      END DO

!
!     The array NIVS contains values whose order is the reverse of
!     the order of the elements of IVS.
!
      DO I = 1, MAXVAL

         J = MAXVAL + 1 - I

         NIVS(I) = IVS(J)

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYSETC test:  Populate a simple symbol table ' // &
                   'with scalar-valued symbols.'                   )

!
!     Initialize the symbol table.
!
      CALL SSIZEC ( MAXTAB, SYNAMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( MAXTAB, SYPTRS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEC ( MAXVAL, SYVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, MAXTAB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME
         XPTRS(I)  = 1
         XVALS(I)  = NIVS(I)

!
!        Associate the value NIVS(I) with the Ith symbol.  Add this
!        symbol to the table.
!
         CALL SYSETC ( NAME, NIVS(I), SYNAMS, SYPTRS, SYVALS )
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
      CALL CHCKSI ( 'Card(SYVALS)', CARDC(SYVALS), '=', MAXTAB, 0, OK )

      CALL CHCKAC ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYSETC test:  Populate a simple symbol table ' // &
                   'with scalar-valued symbols.  Insert symbols '  // &
                   'in reverse order.'                             )

!
!     Clear out the symbol table.
!
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDC ( 0,  SYVALS )

!
!     Use the symbol names and values from the previous test.
!
      DO I = 1, MAXTAB
!
!        Let J be the index of the symbol to insert.
!
         J = MAXTAB + 1 - I
!
!        Associate the value NIVS(J) with the Jth symbol.  Add this
!        symbol to the table.
!
         CALL SYSETC ( XNAMES(J), NIVS(J), SYNAMS, SYPTRS, SYVALS )
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
      CALL CHCKSI ( 'Card(SYVALS)', CARDC(SYVALS), '=', MAXTAB, 0, OK )

      CALL CHCKAC ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYPUTC test:  Populate a simple symbol table ' // &
                   'with scalar-valued symbols.'                   )

!
!     This is essentially a reprise of the previous SYSETC test.
!
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDC ( 0,  SYVALS )


      DO I = 1, MAXTAB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME
         XPTRS(I)  = 1
         XVALS(I)  = NIVS(I)

!
!        Associate the value NIVS(I) with the Ith symbol.  Add this
!        symbol to the table.
!
         CALL SYPUTC ( NAME, XVALS(I), 1, SYNAMS, SYPTRS, SYVALS )
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
      CALL CHCKSI ( 'Card(SYVALS)', CARDC(SYVALS), '=', MAXTAB, 0, OK )

      CALL CHCKAC ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYFETC test:  Fetch names from symbol table ' // &
                   'with scalar-valued symbols.'                   )


      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYFETC( I, SYNAMS, SYPTRS, SYVALS, NAME, FOUND )
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

      CALL SYFETC( MAXTAB+1, SYNAMS, SYPTRS, SYVALS, NAME, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYGETC test:  Fetch values from symbol table ' // &
                   'with scalar-valued symbols.'                   )


      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYGETC( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, VAL, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         TITLE = 'Value of symbol #'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL CHCKSC ( TITLE, VAL, '=', XVALS(I), OK )

      END DO

!
!     Also look for a symbol we know isn't there.
!
      NAME = 'NOT_THERE'

      CALL SYGETC( NAME, &
                   SYNAMS, SYPTRS, SYVALS, NVALS, VAL, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TITLE = 'Was name # found?'
      CALL REPMC  ( TITLE, '#',    NAME,   TITLE )
      CALL CHCKSL ( TITLE, FOUND, .FALSE., OK    )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYRENC test:  Rename symbols in a simple ' // &
                   'symbol table with scalar-valued symbols.' )


      DO I = 1, MAXTAB
!
!        Get the name and value of the ith symbol.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         CALL SYGETC ( NAME,  SYNAMS, SYPTRS, SYVALS, &
                       NVALS, XVAL,   FOUND           )
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
         CALL SYRENC ( NAME, NEWNAM, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Make sure the old symbol is gone.
!
         CALL SYGETC ( NAME,  SYNAMS, SYPTRS, SYVALS, &
                       NVALS, VAL,    FOUND           )
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
         CALL SYGETC ( NEWNAM,  SYNAMS, SYPTRS, SYVALS, &
                       NVALS,   VAL,    FOUND           )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

!
!        Check the value.
!
         CALL CHCKSC ( 'VAL', VAL, '=', XVAL, OK )

!
!        Change the name back to its original value.
!
         CALL SYRENC ( NEWNAM, NAME, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYDELC test:  Delete symbols from symbol table ' // &
                   'with scalar-valued symbols.'                   )

      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYDELC( XNAMES(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Make sure the symbol is gone.
!
         TITLE = 'Was (deleted symbol) name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYGETC( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, VAL, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )

!
!        Validate the remaining symbol table.
!
         DO J = I+1, MAXTAB

            TITLE = 'Was (remaining symbol) name # found?'
            CALL REPMI ( TITLE, '#', I, TITLE )

            CALL SYGETC ( XNAMES(J), &
                          SYNAMS, SYPTRS, SYVALS, NVALS, VAL, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

            TITLE = 'Remaining symbol # value'
            CALL REPMI ( TITLE, '#', I, TITLE )

            CALL CHCKSC ( TITLE, VAL, '=', XVALS(J), OK )

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
      CALL TCASE ( 'SYPUTC test:  Create symbol table ' // &
                   'with array-valued symbols.'         )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDC ( 0,  SYVALS )


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

            XVALS( TO )  =  IVS( TO )
            TO           =  TO + 1

         END DO

!
!        Add the symbol to the table.
!
         CALL SYPUTC ( NAME, XVALS(START), I, SYNAMS, SYPTRS, SYVALS )
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

      CALL CHCKSI ( 'Card(SYVALS)', CARDC(SYVALS), '=', XCARD, 0, OK )

      CALL CHCKAC ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYPUTC test:  Populate a simple symbol table ' // &
                   'with scalar-valued symbols.  Insert symbols '  // &
                   'in reverse order.'                             )

!
!     Clear out the symbol table.
!
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDC ( 0,  SYVALS )

      CALL CLEARC ( MAXVAL, XVALS )

!
!     Use the symbol names and values from the previous test.
!
      SIZE  = MAXTAB * ( MAXTAB + 1 ) / 2

      START = SIZE - MAXTAB + 1

      DO I = MAXTAB, 1, -1
!
!        Add the symbol to the table.
!
         CALL SYPUTC ( XNAMES(I), XVALS(START), I, &
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

      CALL CHCKSI ( 'Card(SYVALS)', CARDC(SYVALS), '=', XCARD, 0, OK )

      CALL CHCKAC ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYGETC test:  validate array-valued symbol '// &
                   'table by fetching values associated with '  // &
                   'each symbol.'                               )


      START = 1

      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYGETC( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, CVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol #'
            CALL REPMI ( TITLE, '#', I, TITLE )

            CALL CHCKAC ( TITLE, CVALS, '=', XVALS(START), I, OK )

         END IF

         START = START + I

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYNTHC test:  validate array-valued symbol '// &
                   'table by fetching values associated with '  // &
                   'each symbol.'                               )


      START = 1

      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYGETC( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, CVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN
!
!           Get the size of the current symbol.
!
            SIZE = SYDIMC ( XNAMES(I), SYNAMS, SYPTRS, SYVALS )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO J = 1, SIZE
!
!              Fetch each value.
!
               TITLE = 'Value # of symbol #'
               CALL REPMI ( TITLE, '#', I,         TITLE )
               CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL SYNTHC( XNAMES(I), J,     SYNAMS, SYPTRS, SYVALS, &
                            VAL,       FOUND                          )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

               CALL CHCKSC ( TITLE, VAL, '=', &
                                    XVALS(START+J-1), OK )

            END DO

!
!           Try to fetch an element we know isn't there.
!
            J = SIZE + 1

            TITLE = 'Value # of symbol #'
            CALL REPMI ( TITLE, '#', J,         TITLE )
            CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL SYNTHC( XNAMES(I), J, SYNAMS, SYPTRS, SYVALS, &
                            VAL,       FOUND                   )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )

         END IF

         START = START + I

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYENQC test:  Create symbol table ' // &
                   'with array-valued symbols.'         )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDC ( 0,  SYVALS )


      TO = 1

      DO I = 1, MAXTAB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME
         XPTRS(I)  = I

         DO J = 1, I

            XVALS( TO )  =    IVS( TO )

            CALL SYENQC ( NAME, XVALS(TO), SYNAMS, SYPTRS, SYVALS )
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

      CALL CHCKSI ( 'Card(SYVALS)', CARDC(SYVALS), '=', XCARD, 0, OK )

      CALL CHCKAC ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYPSHC test:  Create symbol table ' // &
                   'with array-valued symbols.'         )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDC ( 0,  SYVALS )


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

            XVALS( TO )  =    IVS( TO )

            CALL SYPSHC ( NAME, XVALS(TO), SYNAMS, SYPTRS, SYVALS )
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

      CALL CHCKSI ( 'Card(SYVALS)', CARDC(SYVALS), '=', XCARD, 0, OK )

      CALL CHCKAC ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYDUPC test:  Create symbol table '         // &
                   'with array-valued symbols.  Then duplicate '// &
                   'each symbol.'                               )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDC ( 0,  SYVALS )


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

            XVALS( TO )  =    IVS( TO )

            CALL SYENQC ( NAME, XVALS(TO), SYNAMS, SYPTRS, SYVALS )
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

         CALL SYGETC( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, CVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (0)'
            CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )

            CALL CHCKAC ( TITLE, CVALS, '=', XVALS(START), I, OK )

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

         CALL SYDUPC ( NAME, NEWNAM, SYNAMS, SYPTRS, SYVALS )
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

         CALL SYGETC( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, CVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (1)'
            CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )

            CALL CHCKAC ( TITLE, CVALS, '=', XVALS(START), I, OK )

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

         CALL SYGETC( NEWNAM, &
                      SYNAMS, SYPTRS, SYVALS, NVALS, CVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (2)'
            CALL REPMC ( TITLE, '#', NEWNAM, TITLE )

            CALL CHCKAC ( TITLE, CVALS, '=', XVALS(START), I, OK )

         END IF

         START = START + I

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYDUPC test:  Create symbol table '             // &
                   'with array-valued symbols.  Duplicate these '   // &
                   'symbols.  Negate the values of the original '   // &
                   'symbols.  Then duplicate the duplicate '        // &
                   'the symbols, overwriting the original symbols.' )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDC ( 0,  SYVALS )

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

            XVALS( TO )  =    IVS ( TO )

            CALL SYENQC ( NAME, XVALS(TO), SYNAMS, SYPTRS, SYVALS )
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

         CALL SYDUPC ( NAME, NEWNAM, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     "Negate" the values of original symbols.
!
      DO I = 1, UB

         CALL SYGETC( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, CVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, NVALS
            CALL PREFIX ( '-', 0, CVALS(J) )
         END DO

         CALL SYPUTC( XNAMES(I),  CVALS,   NVALS, &
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

         CALL SYDUPC ( NEWNAM, NAME, SYNAMS, SYPTRS, SYVALS )
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

         CALL SYGETC( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, CVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (1)'
            CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )

            CALL CHCKAC ( TITLE, CVALS, '=', XVALS(START), I, OK )

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

         CALL SYGETC( XNAMES(I), &
                      SYNAMS, SYPTRS, SYVALS, NVALS, CVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (2)'
            CALL REPMC ( TITLE, '#', NEWNAM, TITLE )

            CALL CHCKAC ( TITLE, CVALS, '=', XVALS(START), I, OK )

         END IF

         START = START + I

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYORDC test:  Create symbol table '      // &
                   'with array-valued symbols; sort values ' // &
                   'associated with each symbol.'            )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDC ( 0,  SYVALS )

      CALL CLEARC ( MAXVAL, XVALS )

!
!     Create the symbol table to be sorted.
!
      TO = 1

      DO I = 1, MAXTAB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME

         DO J = 1, I

            VAL  =  IVS( TO )

            CALL SYENQC ( NAME, VAL, SYNAMS, SYPTRS, SYVALS )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TO   =  TO + 1

         END DO

      END DO

!
!     Sort the symbol values.
!
      DO I = 1, MAXTAB

         CALL SYORDC  ( XNAMES(I), SYNAMS, SYPTRS, SYVALS )

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

            XVALS(TO)  =    IVS ( TO )

         END DO

         START = START + I

      END DO

!
!     Check the symbol table values against the value array.
!
      CALL CHCKAC ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYDIMC test:  Find dimensions of symbols in ' // &
                   'symbol table with array-valued symbols.'      )


      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDC ( 0,  SYVALS )

      CALL CLEARC ( MAXVAL, XVALS )


      DO I = 1, MAXTAB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME

         DO J = 1, I

            VAL  =  IVS ( TO )

            CALL SYENQC ( NAME, VAL, SYNAMS, SYPTRS, SYVALS )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

      DO I = 1, MAXTAB

         NVALS =  SYDIMC( XNAMES(I), SYNAMS, SYPTRS, SYVALS  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'Dimension of symbol #'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL CHCKSI ( TITLE, NVALS, '=', I, 0, OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYPOPC test:  Pop values from symbol table ' // &
                   'with array-valued symbols.'                 )


!
!     Re-create the default array-valued symbol table.
!
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDC ( 0,  SYVALS )

      TO = 1

      DO I = 1, MAXTAB
!
!        Create the symbol name.
!
         CALL T_ITHSYM ( I, MAXTAB, NAME )

!
!        Set the values of the Ith symbol.
!
         DO J = 1, I

            CVALS(J)  =  IVS ( TO )
            TO        =  TO + 1

         END DO

!
!        Insert the Ith symbol.
!
         CALL SYPUTC ( NAME, CVALS, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
!     Now test SYPOPC.
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
            XVALS( TO )  =  IVS ( TO )

            CALL SYPOPC ( NAME, SYNAMS, SYPTRS, SYVALS, VAL, FOUND )
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

               CALL CHCKSC ( TITLE, VAL, '=', XVALS(TO), OK )

            END IF

            TO    =    TO + 1

         END DO

!
!        Try to pop a value from the now empty symbol.
!
         CALL SYPOPC ( NAME, SYNAMS, SYPTRS, SYVALS, VAL, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'Was element popped from symbol #?'
         CALL REPMC ( TITLE, '#', NAME, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SYTRNC test:  Transpose values from symbol ' // &
                   'table with array-valued symbols.'            )

!
!     We'll create a small version of our array-valued symbol table.
!     Since we want to try all possible transpositions, there would
!     be an excessive number of calls to SYTRNC if we used the full
!     set of MAXTAB symbols.
!
!     Create the symbol table now.
!
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDC ( 0,  SYVALS )

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

            XVALS( TO )  =    IVS ( TO )
            TO           =    TO + 1

         END DO

!
!        Insert the Ith symbol.
!
         CALL SYPUTC ( NAME,  XVALS(START), I, SYNAMS, SYPTRS, SYVALS )
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

               CALL SYTRNC ( NAME, J, K, SYNAMS, SYPTRS, SYVALS )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Fetch the values of the modified symbol.
!
               CALL SYGETC ( NAME,  SYNAMS, SYPTRS, SYVALS, &
                             NVALS, CVALS,  FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

               IF ( FOUND ) THEN
!
!                 If CVALS reflects the correct transposition,
!                 then we can swap elements J and K of CVALS
!                 to obtain the original symbol's values.
!
                  IF ( J .NE. K ) THEN
                     CALL SWAPC ( CVALS(J), CVALS(K) )
                  END IF

                  TITLE = 'Values of symbol #:  J = #; K = #'
                  CALL REPMC  ( TITLE, '#', NAME, TITLE )
                  CALL REPMI  ( TITLE, '#', J,    TITLE )
                  CALL REPMI  ( TITLE, '#', K,    TITLE )

                  CALL CHCKAC ( TITLE, CVALS, '=', &
                                       XVALS(START), I, OK )

               END IF

!
!              Undo the transposition in the symbol table.
!
               CALL SYTRNC ( NAME, J, K, SYNAMS, SYPTRS, SYVALS )
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
      CALL TCASE ( 'SYSELC test:  Extract slice of values from ' // &
                   'symbol table with array-valued symbols.'     )

!
!     This approach used in this test case closely parallels that
!     of the SYTRNC test case above.
!
!     We'll create a small version of our array-valued symbol table.
!     Since we want to try all possible slices, there would
!     be an excessive number of calls to SYTRNC if we used the full
!     set of MAXTAB symbols.
!
!     Create the symbol table now.
!
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDC ( 0,  SYVALS )

      CALL CLEARC ( MAXVAL, XVALS )

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

            XVALS( TO )  =    IVS ( TO )
            TO           =    TO + 1

         END DO

!
!        Insert the Ith symbol.
!
         CALL SYPUTC ( NAME,  XVALS(START), I, SYNAMS, SYPTRS, SYVALS )
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
               CALL SYSELC ( NAME,   J,      K, &
                             SYNAMS, SYPTRS, SYVALS, CVALS, FOUND )
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

                  CALL CHCKAC ( TITLE, &
                                CVALS, &
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
!     SYDELC:  No errors are detected by this routine.
!
!     SYDIMC:  No errors are detected by this routine.
!
!
!     SYDUPC:
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYDUPC:  symbol name not present' )

      CALL SYDUPC ( 'NOSYMBOL', 'NOSYMBOL2', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHSYMBOL)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYDUPC:  name table overflow' )

!
!     We'll create a small symbol table so we can test the handling of
!     overflow conditions.
!
      TABSIZ = 5

      CALL SSIZEC ( TABSIZ,   SYNAMS )
      CALL SSIZEI ( 2*TABSIZ, SYPTRS )
      CALL SSIZEC ( 2*TABSIZ, SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETC ( NAME, IVS(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SYDUPC ( NAME, 'NEWNAME', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NAMETABLEFULL)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYDUPC:  pointer table overflow' )

      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ,    SYPTRS )
      CALL SSIZEC ( TABSIZ+2,  SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETC ( NAME, IVS(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SYDUPC ( NAME, 'NEWNAME', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(POINTERTABLEFULL)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYDUPC:  value table overflow' )


      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ+1,  SYPTRS )
      CALL SSIZEC ( TABSIZ,    SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETC ( NAME, IVS(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SYDUPC ( NAME, 'NEWNAME', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )

!
!
!     SYENQC:
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYENQC:  value table overflow' )


      TABSIZ = 5

      CALL SSIZEC ( TABSIZ,  SYNAMS )
      CALL SSIZEI ( TABSIZ,  SYPTRS )
      CALL SSIZEC ( TABSIZ,  SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETC ( NAME, IVS(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Enqueue a value onto the last symbol.
!
      CALL SYENQC ( NAME, NIVS(1), SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )

!
!
!
!     SYFETC:  No errors are detected by this routine.
!
!     SYGETC:  No errors are detected by this routine.
!
!     SYNTHC:  No errors are detected by this routine.
!
!     SYORDC:  No errors are detected by this routine.
!
!     SYPOPC:  No errors are detected by this routine.
!
!
!
!     SYPSHC:
!
! --- Case: ------------------------------------------------------
!
!
      CALL TCASE  ( 'SYPSHC:  value table overflow' )


      TABSIZ = 5

      CALL SSIZEC ( TABSIZ,  SYNAMS )
      CALL SSIZEI ( TABSIZ,  SYPTRS )
      CALL SSIZEC ( TABSIZ,  SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETC ( NAME, IVS(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Push a value onto the last symbol.
!
      CALL SYPSHC ( NAME, NIVS(I), SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )




!
!     SYPUTC:
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYPUTC:  name table overflow' )

!
!     We'll create a small symbol table so we can test the handling of
!     overflow conditions.
!
      TABSIZ = 5

      CALL SSIZEC ( TABSIZ,   SYNAMS )
      CALL SSIZEI ( 2*TABSIZ, SYPTRS )
      CALL SSIZEC ( 2*TABSIZ, SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETC ( NAME, IVS(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      CALL SYPUTC ( 'NEWNAME', IVS(I), 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NAMETABLEFULL)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYPUTC:  pointer table overflow' )

      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ,    SYPTRS )
      CALL SSIZEC ( TABSIZ+2,  SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETC ( NAME, IVS(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SYPUTC ( 'NEWNAME', IVS(I), 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(POINTERTABLEFULL)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYPUTC:  value table overflow' )


      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ+1,  SYPTRS )
      CALL SSIZEC ( TABSIZ,    SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETC ( NAME, IVS(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SYPUTC ( 'NEWNAME', IVS(I), 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYPUTC: invalid value count' )


      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ+1,  SYPTRS )
      CALL SSIZEC ( TABSIZ+1,  SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETC ( NAME, IVS(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SYPUTC ( 'NEWNAME', IVS(I), -1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARGUMENT)', OK )


!
!     SYRENC:
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYRENC:  rename non-existent symbol' )

      CALL SYRENC ( 'NONNAME', 'NEWNAME', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHSYMBOL)', OK )



!
!
!     SYSELC:  No errors are detected by this routine.
!
!
!     SYSETC:
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYSETC:  name table overflow' )

!
!     We'll create a small symbol table so we can test the handling of
!     overflow conditions.
!
      TABSIZ = 5

      CALL SSIZEC ( TABSIZ,   SYNAMS )
      CALL SSIZEI ( 2*TABSIZ, SYPTRS )
      CALL SSIZEC ( 2*TABSIZ, SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETC ( NAME, IVS(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      CALL SYSETC ( 'NEWNAME', IVS(I), SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NAMETABLEFULL)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYSETC:  pointer table overflow' )

      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ,    SYPTRS )
      CALL SSIZEC ( TABSIZ+2,  SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETC ( NAME, IVS(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SYSETC ( 'NEWNAME', IVS(I), SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(POINTERTABLEFULL)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYSETC:  value table overflow' )


      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ+1,  SYPTRS )
      CALL SSIZEC ( TABSIZ,    SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETC ( NAME, IVS(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SYSETC ( 'NEWNAME', IVS(I), SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )


!     SYTRNC:
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYTRNC:  first index < 1' )


      CALL SSIZEC ( TABSIZ,  SYNAMS )
      CALL SSIZEI ( TABSIZ,  SYPTRS )
      CALL SSIZEC ( TABSIZ,  SYVALS )


      DO I = 1, TABSIZ

         CALL T_ITHSYM ( I, TABSIZ, NAME )

         CALL SYSETC ( NAME, IVS(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XNAMES(I) = NAME
         XPTRS (I) = 1
         XVALS (I) = IVS(I)

      END DO

      CALL SYTRNC ( NAME, -1, 2, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYTRNC:  first index > second index' )

      CALL SYTRNC ( NAME, 2, 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYTRNC:  second index > symbol size' )

      CALL SYTRNC ( NAME, 1, TABSIZ+1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SYTRNC:  non-error case: attempt transposition '// &
                    'of values of non-existent symbol.'             )

      CALL SYTRNC ( 'NONAME', 1, 1, SYNAMS, SYPTRS, SYVALS )

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
      CALL CHCKSI ( 'Card(SYVALS)', CARDC(SYVALS), '=', TABSIZ, 0, OK )

      CALL CHCKAC ( 'SYVALS', SYVALS(1), '=', XVALS, TABSIZ, OK )


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
!     The calling sequence of the utility is:
!
!        SUBROUTINE T_ITHSYM ( N, NMAX, NAME )
!
!     This utility is defined in the file
!
!        f_symtbi.for
!
