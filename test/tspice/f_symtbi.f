C$Procedure F_SYMTBI ( Test SPICELIB integer symbol table routines )
 
      SUBROUTINE F_SYMTBI ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB symbol table routines of type INTEGER.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SYMBOL
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE

      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Particulars
C
C     This routine tests the SPICELIB symbol table routines
C
C        SYDELI
C        SYDIMI
C        SYDUPI
C        SYENQI
C        SYFETI
C        SYGETI
C        SYNTHI
C        SYORDI
C        SYPOPI
C        SYPSHI
C        SYPUTI
C        SYRENI
C        SYSELI
C        SYSETI
C        SYTRNI
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    TSPICE Version 1.1.0, 18-JUL-2007 (NJB)
C
C        Bug fix: modified digit computation in utility
C        routine T_ITHSYM to guard against round-off 
C        when taking a base 10 log of an integral power
C        of 10. 
C
C-    TSPICE Version 1.0.0, 01-NOV-2005 (NJB)
C
C
C-&

C$ Revisions
C
C-    TSPICE Version 1.1.0, 18-JUL-2007 (NJB)
C
C        Bug fix: modified digit computation in utility
C        routine T_ITHSYM to guard against round-off 
C        when taking a base 10 log of an integral power
C        of 10. 
C
C        The new computation is
C
C           NDIGIT =  INT (  LOG10 ( DBLE(NMAX)+0.5D0 )  )  +  1
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               CARDC
      INTEGER               CARDI
      INTEGER               SYDIMI

C
C     Local Parameters
C
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

C
C     Local Variables
C
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
      INTEGER               XCARD
      INTEGER               XPTRS  (          MAXTAB )
      INTEGER               XVAL
      INTEGER               XVALS  (          MAXVAL )

      LOGICAL               FOUND

C
C     Saved values
C
C     To avoid memory problems under cygwin, save everything.
C
      SAVE

C
C     Initial values
C

C
C     *********************************************************
C     Note:  the order of the test cases is significant in that
C     some cases depend on results from other cases.  Take care
C     to preserve side effects when modifying this code!
C     *********************************************************
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_SYMTBI' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYSETI test:  Populate a simple symbol table ' //
     .             'with scalar-valued symbols.'                   )

C
C     Initialize the symbol table.
C
      CALL SSIZEC ( MAXTAB, SYNAMS )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL SSIZEI ( MAXTAB, SYPTRS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( MAXVAL, SYVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )
      

      DO I = 1, MAXTAB
C
C        Create the symbol name.
C
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME
         XPTRS(I)  =  1
         XVALS(I)  = -I

C
C        Associate the value -I with the Ith symbol.  Add this
C        symbol to the table.
C
         CALL SYSETI ( NAME, -I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check the structure of the cells we've populated.
C
C     The symbol name cell first:
C
      CALL CHCKSI ( 'Card(SYNAMS)', CARDC(SYNAMS), '=', MAXTAB, 0, OK )
      
      DO I = 1, CARDC(SYNAMS)

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, SYNAMS(I), '=', XNAMES(I), OK )

      END DO 

C
C     Then the "pointer" cell.  The pointers are actually element
C     counts.
C
      CALL CHCKSI ( 'Card(SYPTRS)', CARDI(SYPTRS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYPTRS', SYPTRS(1), '=', XPTRS, MAXTAB, OK )
      
C
C     Then the value cell.
C
      CALL CHCKSI ( 'Card(SYVALS)', CARDI(SYVALS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYSETI test:  Populate a simple symbol table ' //
     .             'with scalar-valued symbols.  Insert symbols '  //
     .             'in reverse order.'                             )

C
C     Clear out the symbol table.
C
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

C
C     Use the symbol names and values from the previous test.
C
      DO I = 1, MAXTAB
C
C        Let J be the index of the symbol to insert.
C
         J = MAXTAB + 1 - I
C
C        Associate the value -J with the Jth symbol.  Add this
C        symbol to the table.
C
         CALL SYSETI ( XNAMES(J), -J, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     We should end up with the same symbol table as in the
C     previous test case.
C
C
C     Check the structure of the cells we've populated.
C
C     The symbol name cell first:
C
      CALL CHCKSI ( 'Card(SYNAMS)', CARDC(SYNAMS), '=', MAXTAB, 0, OK )
      
      DO I = 1, CARDC(SYNAMS)

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, SYNAMS(I), '=', XNAMES(I), OK )

      END DO 

C
C     Then the "pointer" cell.  The pointers are actually element
C     counts.
C
      CALL CHCKSI ( 'Card(SYPTRS)', CARDI(SYPTRS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYPTRS', SYPTRS(1), '=', XPTRS, MAXTAB, OK )
      
C
C     Then the value cell.
C
      CALL CHCKSI ( 'Card(SYVALS)', CARDI(SYVALS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYPUTI test:  Populate a simple symbol table ' //
     .             'with scalar-valued symbols.'                   )

C
C     This is essentially a reprise of the previous SYSETI test.
C
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )


      DO I = 1, MAXTAB
C
C        Create the symbol name.
C
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME
         XPTRS(I)  =  1
         XVALS(I)  = -I

C
C        Associate the value -I with the Ith symbol.  Add this
C        symbol to the table.
C
         CALL SYPUTI ( NAME, XVALS(I), 1, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check the structure of the cells we've populated.
C
C     The symbol name cell first:
C
      CALL CHCKSI ( 'Card(SYNAMS)', CARDC(SYNAMS), '=', MAXTAB, 0, OK )
      
      DO I = 1, CARDC(SYNAMS)

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, SYNAMS(I), '=', XNAMES(I), OK )

      END DO 

C
C     Then the "pointer" cell.  The pointers are actually element
C     counts.
C
      CALL CHCKSI ( 'Card(SYPTRS)', CARDI(SYPTRS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYPTRS', SYPTRS(1), '=', XPTRS, MAXTAB, OK )
      
C
C     Then the value cell.
C
      CALL CHCKSI ( 'Card(SYVALS)', CARDI(SYVALS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYFETI test:  Fetch names from symbol table ' //
     .             'with scalar-valued symbols.'                   )


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


C
C     Also look for a symbol we know isn't there.
C
      I     =  MAXTAB+1
      TITLE = 'Was name # found?'
      CALL REPMI  ( TITLE, '#',    I,      TITLE )

      CALL SYFETI( MAXTAB+1, SYNAMS, SYPTRS, SYVALS, NAME, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYGETI test:  Fetch values from symbol table ' //
     .             'with scalar-valued symbols.'                   )


      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYGETI( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, VAL, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         TITLE = 'Value of symbol #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         
         CALL CHCKSI ( TITLE, VAL, '=', XVALS(I), 0, OK )
         
      END DO 

C
C     Also look for a symbol we know isn't there.
C
      NAME = 'NOT_THERE'

      CALL SYGETI( NAME,
     .             SYNAMS, SYPTRS, SYVALS, NVALS, VAL, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TITLE = 'Was name # found?'
      CALL REPMC  ( TITLE, '#',    NAME,   TITLE )
      CALL CHCKSL ( TITLE, FOUND, .FALSE., OK    )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYRENI test:  Rename symbols in a simple ' //
     .             'symbol table with scalar-valued symbols.' )


      DO I = 1, MAXTAB
C
C        Get the name and value of the ith symbol.
C
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         CALL SYGETI ( NAME,  SYNAMS, SYPTRS, SYVALS, 
     .                 NVALS, XVAL,   FOUND           )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

C
C        Create the replacement name:  
C
         CALL REPLCH ( NAME, 'X', 'Y', NEWNAM )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Change the name of the symbol.
C
         CALL SYRENI ( NAME, NEWNAM, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Make sure the old symbol is gone.
C
         CALL SYGETI ( NAME,  SYNAMS, SYPTRS, SYVALS, 
     .                 NVALS, VAL,    FOUND           )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'Was name # found?'
         CALL REPMC ( TITLE, '#', NAME, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )

C
C        Get the value associated with the new name.
C
C
C        Get the name and value of the ith symbol.
C
         CALL SYGETI ( NEWNAM,  SYNAMS, SYPTRS, SYVALS, 
     .                 NVALS,   VAL,    FOUND           )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

C
C        Check the value.
C
         CALL CHCKSI ( 'VAL', VAL, '=', XVAL, 0, OK )

C
C        Change the name back to its original value.
C
         CALL SYRENI ( NEWNAM, NAME, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO 



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYDELI test:  Delete symbols from symbol table ' //
     .             'with scalar-valued symbols.'                   )

      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYDELI( XNAMES(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Make sure the symbol is gone.
C
         TITLE = 'Was (deleted symbol) name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYGETI( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, VAL, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )

C
C        Validate the remaining symbol table.
C
         DO J = I+1, MAXTAB

            TITLE = 'Was (remaining symbol) name # found?'
            CALL REPMI ( TITLE, '#', I, TITLE )

            CALL SYGETI ( XNAMES(J),
     .                    SYNAMS, SYPTRS, SYVALS, NVALS, VAL, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

            TITLE = 'Remaining symbol # value'
            CALL REPMI ( TITLE, '#', I, TITLE )

            CALL CHCKSI ( TITLE, VAL, '=', XVALS(J), 0, OK )

         END DO

      END DO 


C
C     At this point, we need to work with symbol tables containing
C     symbols having multiple values associated with them.  We'll
C     build a symbol table whose nth symbol has n associated values.
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYPUTI test:  Create symbol table ' //
     .             'with array-valued symbols.'         )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

      
      TO = 1

      DO I = 1, MAXTAB
C
C        Create the symbol name.
C
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME
         XPTRS(I)  = I
         START     = TO

         DO J = 1, I

            XVALS( TO )  =  - MAXTAB * I - J
            TO           =    TO + 1

         END DO

C
C        Add the symbol to the table.
C
         CALL SYPUTI ( NAME, XVALS(START), I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


C
C     Check the structure of the cells we've populated.
C
C     The symbol name cell first:
C
      CALL CHCKSI ( 'Card(SYNAMS)', CARDC(SYNAMS), '=', MAXTAB, 0, OK )
      
      DO I = 1, CARDC(SYNAMS)

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, SYNAMS(I), '=', XNAMES(I), OK )

      END DO 

C
C     Then the "pointer" cell.  The pointers are actually element
C     counts.
C
      CALL CHCKSI ( 'Card(SYPTRS)', CARDI(SYPTRS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYPTRS', SYPTRS(1), '=', XPTRS, MAXTAB, OK )
      
C
C     Then the value cell.
C
      XCARD  =  MAXTAB * ( MAXTAB + 1 ) / 2

      CALL CHCKSI ( 'Card(SYVALS)', CARDI(SYVALS), '=', XCARD, 0, OK )

      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYPUTI test:  Populate a simple symbol table ' //
     .             'with scalar-valued symbols.  Insert symbols '  //
     .             'in reverse order.'                             )

C
C     Clear out the symbol table.
C
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

      CALL CLEARI ( MAXVAL, XVALS )

C
C     Use the symbol names and values from the previous test.
C
      SIZE  = MAXTAB * ( MAXTAB + 1 ) / 2

      START = SIZE - MAXTAB + 1

      DO I = MAXTAB, 1, -1
C
C        Add the symbol to the table.
C
         CALL SYPUTI ( XNAMES(I), XVALS(START), I, 
     .                 SYNAMS,    SYPTRS,       SYVALS )

         CALL CHCKXC ( .FALSE., ' ', OK )

         START = START - ( I - 1 )

      END DO

C
C     We should end up with the same symbol table as in the
C     previous test case. 
C
C
C     Check the structure of the cells we've populated.
C
C     The symbol name cell first:
C
      CALL CHCKSI ( 'Card(SYNAMS)', CARDC(SYNAMS), '=', MAXTAB, 0, OK )
      
      DO I = 1, CARDC(SYNAMS)

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, SYNAMS(I), '=', XNAMES(I), OK )

      END DO 

C
C     Then the "pointer" cell.  The pointers are actually element
C     counts.
C
      CALL CHCKSI ( 'Card(SYPTRS)', CARDI(SYPTRS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYPTRS', SYPTRS(1), '=', XPTRS, MAXTAB, OK )
      
C
C     Then the value cell.
C
      XCARD  =  MAXTAB * ( MAXTAB + 1 ) / 2

      CALL CHCKSI ( 'Card(SYVALS)', CARDI(SYVALS), '=', XCARD, 0, OK )

      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYGETI test:  validate array-valued symbol '//
     .             'table by fetching values associated with '  //
     .             'each symbol.'                               )


      START = 1

      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYGETI( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, IVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol #'
            CALL REPMI ( TITLE, '#', I, TITLE )
         
            CALL CHCKAI ( TITLE, IVALS, '=', XVALS(START), I, OK )

         END IF         

         START = START + I

      END DO 


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYNTHI test:  validate array-valued symbol '//
     .             'table by fetching values associated with '  //
     .             'each symbol.'                               )


      START = 1

      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYGETI( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, IVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN
C
C           Get the size of the current symbol.
C
            SIZE = SYDIMI ( XNAMES(I), SYNAMS, SYPTRS, SYVALS )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO J = 1, SIZE
C
C              Fetch each value.
C
               TITLE = 'Value # of symbol #'
               CALL REPMI ( TITLE, '#', I,         TITLE )
               CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )
   
               CALL SYNTHI( XNAMES(I), J,     SYNAMS, SYPTRS, SYVALS, 
     .                      VAL,       FOUND                          )
               CALL CHCKXC ( .FALSE., ' ', OK )
           
               CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

               CALL CHCKSI ( TITLE, VAL, '=', XVALS(START+J-1), 0, OK )
         
            END DO

C
C           Try to fetch an element we know isn't there.
C
            J = SIZE + 1

            TITLE = 'Value # of symbol #'
            CALL REPMI ( TITLE, '#', J,         TITLE )
            CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )
   
            CALL SYNTHI( XNAMES(I), J, SYNAMS, SYPTRS, SYVALS, 
     .                      VAL,       FOUND                   )
            CALL CHCKXC ( .FALSE., ' ', OK )
           
            CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )
         
         END IF

         START = START + I

      END DO 


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYENQI test:  Create symbol table ' //
     .             'with array-valued symbols.'         )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

      
      TO = 1

      DO I = 1, MAXTAB
C
C        Create the symbol name.
C
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


C
C     Check the structure of the cells we've populated.
C
C     The symbol name cell first:
C
      CALL CHCKSI ( 'Card(SYNAMS)', CARDC(SYNAMS), '=', MAXTAB, 0, OK )
      
      DO I = 1, CARDC(SYNAMS)

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, SYNAMS(I), '=', XNAMES(I), OK )

      END DO 

C
C     Then the "pointer" cell.  The pointers are actually element
C     counts.
C
      CALL CHCKSI ( 'Card(SYPTRS)', CARDI(SYPTRS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYPTRS', SYPTRS(1), '=', XPTRS, MAXTAB, OK )
      
C
C     Then the value cell.
C
      XCARD  =  MAXTAB * ( MAXTAB + 1 ) / 2

      CALL CHCKSI ( 'Card(SYVALS)', CARDI(SYVALS), '=', XCARD, 0, OK )

      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYPSHI test:  Create symbol table ' //
     .             'with array-valued symbols.'         )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

      
      TO = 0

      DO I = 1, MAXTAB
C
C        Create the symbol name.
C
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME
         XPTRS(I)  = I
         TO        = TO + I

C
C        At the start of the loop, TO points to the element of XVALS
C        that will hold the last value associated with the Ith symbol.
C
         DO J = I, 1, -1

            XVALS( TO )  =  - MAXTAB * I - J

            CALL SYPSHI ( NAME, XVALS(TO), SYNAMS, SYPTRS, SYVALS )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            TO           =    TO - 1

         END DO

         TO = TO + I

      END DO


C
C     Check the structure of the cells we've populated.
C
C     The symbol name cell first:
C
      CALL CHCKSI ( 'Card(SYNAMS)', CARDC(SYNAMS), '=', MAXTAB, 0, OK )
      
      DO I = 1, CARDC(SYNAMS)

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, SYNAMS(I), '=', XNAMES(I), OK )

      END DO 

C
C     Then the "pointer" cell.  The pointers are actually element
C     counts.
C
      CALL CHCKSI ( 'Card(SYPTRS)', CARDI(SYPTRS), '=', MAXTAB, 0, OK )

      CALL CHCKAI ( 'SYPTRS', SYPTRS(1), '=', XPTRS, MAXTAB, OK )
      
C
C     Then the value cell.
C
      XCARD  =  MAXTAB * ( MAXTAB + 1 ) / 2

      CALL CHCKSI ( 'Card(SYVALS)', CARDI(SYVALS), '=', XCARD, 0, OK )

      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYDUPI test:  Create symbol table '         //
     .             'with array-valued symbols.  Then duplicate '//
     .             'each symbol.'                               )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )


C
C     We'll set the cardinality upper bound UB to MAXTAB/2, so
C     we'll have room for the duplicate symbols.
C     
      TO = 1
      UB = MAXTAB/2

      DO I = 1, UB
C
C        Create the symbol name.
C
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

C
C     Validate the symbol table at this stage.
C
      START = 1

      DO I = 1, UB

         TITLE = 'Was name # found? (0)'
         CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )

         CALL SYGETI( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, IVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (0)'
            CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )
         
            CALL CHCKAI ( TITLE, IVALS, '=', XVALS(START), I, OK )

         END IF         

         START = START + I

      END DO

C
C     Look up each symbol; add to the symbol table a duplicate symbol 
C     with a new name.
C
      DO I = 1, UB
C
C        Create the symbol name.
C
         CALL T_ITHSYM ( I, UB, NAME )

         NEWNAM = NAME
         NEWNAM(1:2) = '2_'
         
         CALL SYDUPI ( NAME, NEWNAM, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

C
C     Now validate the symbol table.  First make sure the original
C     symbols are intact.
C
      START = 1

      DO I = 1, UB

         TITLE = 'Was name # found? (1)'
         CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )

         CALL SYGETI( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, IVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (1)'
            CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )
         
            CALL CHCKAI ( TITLE, IVALS, '=', XVALS(START), I, OK )

         END IF         

         START = START + I

      END DO

C
C     Now check the duplicate symbols.
C
      START = 1

      DO I = 1, UB

         NEWNAM = XNAMES(I)
         NEWNAM(1:2) = '2_'
         
         TITLE = 'Was name # found? (2)'
         CALL REPMC ( TITLE, '#', NEWNAM, TITLE )

         CALL SYGETI( NEWNAM,
     .                SYNAMS, SYPTRS, SYVALS, NVALS, IVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (2)'
            CALL REPMC ( TITLE, '#', NEWNAM, TITLE )
         
            CALL CHCKAI ( TITLE, IVALS, '=', XVALS(START), I, OK )

         END IF         

         START = START + I

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYDUPI test:  Create symbol table '             //
     .             'with array-valued symbols.  Duplicate these '   //
     .             'symbols.  Negate the values of the original '   //
     .             'symbols.  Then duplicate the duplicate '        //
     .             'the symbols, overwriting the original symbols.' )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

C
C     We'll set the cardinality upper bound UB to MAXTAB/2, so
C     we'll have room for the duplicate symbols.
C      
      TO = 1
      UB = MAXTAB/2

      DO I = 1, UB
C
C        Create the symbol name.
C
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

C
C     Look up each symbol; add to the symbol table a duplicate symbol 
C     with a new name.
C
      DO I = 1, UB
C
C        Create the symbol name.
C
         CALL T_ITHSYM ( I, UB, NAME )

         NEWNAM = NAME
         NEWNAM(1:2) = '2_'
         
         CALL SYDUPI ( NAME, NEWNAM, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

C
C     Negate the values of the symbols of the original symbols.
C
      DO I = 1, UB

         CALL SYGETI( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, IVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         DO J = 1, NVALS
            IVALS(J) = -IVALS(J)
         END DO

         CALL SYPUTI( XNAMES(I),  IVALS,   NVALS,   
     .                SYNAMS,     SYPTRS,  SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Duplicate the duplicate symbols, overwriting the originals.
C
      DO I = 1, UB
C
C        Create the symbol name.
C
         CALL T_ITHSYM ( I, UB, NAME )

         NEWNAM = NAME
         NEWNAM(1:2) = '2_'
         
         CALL SYDUPI ( NEWNAM, NAME, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

C
C     Now validate the symbol table.  First make sure the
C     first set of duplicate symbols is intact.
C
      START = 1

      DO I = 1, UB

         CALL T_ITHSYM ( I, UB, NAME )

         NEWNAM = NAME
         NEWNAM(1:2) = '2_'
 
         TITLE = 'Was name # found? (1)'
         CALL REPMC ( TITLE, '#', NEWNAM, TITLE )

         CALL SYGETI( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, IVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (1)'
            CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )
         
            CALL CHCKAI ( TITLE, IVALS, '=', XVALS(START), I, OK )

         END IF         

         START = START + I

      END DO

C
C     Now check the duplicated duplicate symbols.
C
      START = 1

      DO I = 1, UB
         
         TITLE = 'Was name # found? (2)'
         CALL REPMC ( TITLE, '#', NAME, TITLE )

         CALL SYGETI( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, IVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (2)'
            CALL REPMC ( TITLE, '#', NEWNAM, TITLE )
         
            CALL CHCKAI ( TITLE, IVALS, '=', XVALS(START), I, OK )

         END IF         

         START = START + I

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYORDI test:  Create symbol table '      //
     .             'with array-valued symbols; sort values ' //
     .             'associated with each symbol.'            )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

      CALL CLEARI ( MAXVAL, XVALS )

C
C     Create the symbol table to be sorted.
C      
      DO I = 1, MAXTAB
C
C        Create the symbol name.
C
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME

         DO J = 1, I

            VAL  =  - MAXTAB * I - J

            CALL SYENQI ( NAME, VAL, SYNAMS, SYPTRS, SYVALS )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

C
C     Sort the symbol values.
C     
      DO I = 1, MAXTAB

         CALL SYORDI  ( XNAMES(I), SYNAMS, SYPTRS, SYVALS ) 

      END DO

C
C     Now we must validate the ordered symbol table.  Create
C     an array containing the expected values in the table.
C
      START = 1

      DO I = 1, MAXTAB

         DO J = 1, I
C
C           Fill in the values for the Ith symbol in the
C           opposite order from that in which they were inserted
C           into the symbol table.
C
            TO         =    START  + I - J

            XVALS(TO)  =  - MAXTAB * I - J

         END DO
         
         START = START + I

      END DO

C
C     Check the symbol table values against the value array.
C
      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYDIMI test:  Find dimensions of symbols in ' //
     .             'symbol table with array-valued symbols.'      )


      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

      CALL CLEARI ( MAXVAL, XVALS )

     
      DO I = 1, MAXTAB
C
C        Create the symbol name.
C
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

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYPOPI test:  Pop values from symbol table ' //
     .             'with array-valued symbols.'                 )
      

C
C     Re-create the default array-valued symbol table.
C
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

      
      DO I = 1, MAXTAB
C
C        Create the symbol name.
C
         CALL T_ITHSYM ( I, MAXTAB, NAME )

C
C        Set the values of the Ith symbol.
C        
         DO J = 1, I

            IVALS(J)  =  - MAXTAB * I - J
            
         END DO

C
C        Insert the Ith symbol.
C
         CALL SYPUTI ( NAME, IVALS, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


C
C     Now test SYPOPI.
C
      TO = 1

      DO I = 1, MAXTAB
C
C        Create the symbol name.
C
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XPTRS(I)  = I

         DO J = 1, I
C
C           Set the expected value.
C
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

C
C        Try to pop a value from the now empty symbol.
C
         CALL SYPOPI ( NAME, SYNAMS, SYPTRS, SYVALS, VAL, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'Was element popped from symbol #?'
         CALL REPMC ( TITLE, '#', NAME, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )
 
         CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYTRNI test:  Transpose values from symbol ' //
     .             'table with array-valued symbols.'            )

C
C     We'll create a small version of our array-valued symbol table.
C     Since we want to try all possible transpositions, there would
C     be an excessive number of calls to SYTRNI if we used the full
C     set of MAXTAB symbols.
C
C     Create the symbol table now.
C
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

      TABSIZ = 5      

      TO     = 1


      DO I = 1, TABSIZ
C
C        Create the symbol name.
C
         CALL T_ITHSYM ( I, TABSIZ, NAME )

C
C        Set the values of the Ith symbol.
C        
         START = TO

         DO J = 1, I

            XVALS( TO )  =  - TABSIZ * I - J
            TO           =    TO + 1
            
         END DO

C
C        Insert the Ith symbol.
C
         CALL SYPUTI ( NAME,  XVALS(START), I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO  

C
C     Now try the transpositions.
C
      START = 1

      DO I = 1, TABSIZ

         SIZE = I
C
C        Try every possible transposition of the elements of the Ith
C        symbol, including those where the indices of the transposed
C        elements are not distinct.
C
         CALL T_ITHSYM ( I, TABSIZ, NAME )

C
C        Transpose elements J and K of the Ith symbol, for all
C        applicable J, K.
C
         DO J = 1, SIZE

            DO K = 1, SIZE

               CALL SYTRNI ( NAME, J, K, SYNAMS, SYPTRS, SYVALS )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Fetch the values of the modified symbol.
C               
               CALL SYGETI ( NAME,  SYNAMS, SYPTRS, SYVALS,
     .                       NVALS, IVALS,  FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

               IF ( FOUND ) THEN
C
C                 If IVALS reflects the correct transposition,
C                 then we can swap elements J and K of IVALS
C                 to obtain the original symbol's values.
C
                  IF ( J .NE. K ) THEN
                     CALL SWAPI ( IVALS(J), IVALS(K) )
                  END IF

                  TITLE = 'Values of symbol #:  J = #; K = #'
                  CALL REPMC  ( TITLE, '#', NAME, TITLE )
                  CALL REPMI  ( TITLE, '#', J,    TITLE )
                  CALL REPMI  ( TITLE, '#', K,    TITLE )
         
                  CALL CHCKAI ( TITLE, IVALS, '=', XVALS(START), I, OK )
                  
               END IF

C
C              Undo the transposition in the symbol table.
C              
               CALL SYTRNI ( NAME, J, K, SYNAMS, SYPTRS, SYVALS )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END DO

         END DO

C
C        Set the start index for the next symbol.
C
         START = START + I

      END DO



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYSELI test:  Extract slice of values from ' //
     .             'symbol table with array-valued symbols.'     )

C
C     This approach used in this test case closely parallels that
C     of the SYTRNI test case above.
C
C     We'll create a small version of our array-valued symbol table.
C     Since we want to try all possible slices, there would
C     be an excessive number of calls to SYTRNI if we used the full
C     set of MAXTAB symbols.
C
C     Create the symbol table now.
C
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDI ( 0,  SYVALS )

      CALL CLEARI ( MAXVAL, XVALS )

      TABSIZ = 5      

      TO     = 1


      DO I = 1, TABSIZ
C
C        Create the symbol name.
C
         CALL T_ITHSYM ( I, TABSIZ, NAME )

C
C        Set the values of the Ith symbol.
C        
         START = TO

         DO J = 1, I

            XVALS( TO )  =  - TABSIZ * I - J
            TO           =    TO + 1
            
         END DO

C
C        Insert the Ith symbol.
C
         CALL SYPUTI ( NAME,  XVALS(START), I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO  

C
C     Now try the slice extractions.
C
      START = 1

      DO I = 1, TABSIZ

         SIZE = I
C
C        Try every possible slice of the elements of the Ith
C        symbol, including those where the endpoint indices of the
C        slice are not distinct.
C
         CALL T_ITHSYM ( I, TABSIZ, NAME )

C
C        Extrace the slice ranging from elements J to K of the Ith 
C        symbol, for all applicable J, K.
C
         DO J = 1, SIZE

            DO K = 1, SIZE
C
C              Fetch the slice from J to K.  We expect to find a
C              result only if K >= J.
C
               CALL SYSELI ( NAME,   J,      K, 
     .                       SYNAMS, SYPTRS, SYVALS, IVALS, FOUND )
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
         
                  CALL CHCKAI ( TITLE, 
     .                          IVALS, 
     .                          '=', 
     .                          XVALS(START+J-1), 
     .                          K-J+1, 
     .                          OK               )                  
               END IF

            END DO

         END DO

C
C        Set the start index for the next symbol.
C
         START = START + I

      END DO


C
C     Now for some error handling tests.  
C      
C     
C     SYDELI:  No errors are detected by this routine.
C 
C     SYDIMI:  No errors are detected by this routine.
C     
C
C     SYDUPI:
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYDUPI:  symbol name not present' )
 
      CALL SYDUPI ( 'NOSYMBOL', 'NOSYMBOL2', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHSYMBOL)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYDUPI:  name table overflow' )

C
C     We'll create a small symbol table so we can test the handling of
C     overflow conditions.
C
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

C
C --- Case: ------------------------------------------------------
C
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

C
C --- Case: ------------------------------------------------------
C
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

C
C
C     SYENQI:
C
C --- Case: ------------------------------------------------------
C
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

C
C     Enqueue a value onto the last symbol.
C
      CALL SYENQI ( NAME, -1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )

C
C
C
C     SYFETI:  No errors are detected by this routine.
C
C     SYGETI:  No errors are detected by this routine.
C
C     SYNTHI:  No errors are detected by this routine.
C 
C     SYORDI:  No errors are detected by this routine.
C 
C     SYPOPI:  No errors are detected by this routine.
C
C
C
C     SYPSHI:
C
C --- Case: ------------------------------------------------------
C
C
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

C
C     Push a value onto the last symbol.
C
      CALL SYPSHI ( NAME, -1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )




C
C     SYPUTI:
C     
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYPUTI:  name table overflow' )

C
C     We'll create a small symbol table so we can test the handling of
C     overflow conditions.
C
      TABSIZ = 5

      CALL SSIZEC ( TABSIZ,   SYNAMS )
      CALL SSIZEI ( 2*TABSIZ, SYPTRS )
      CALL SSIZEI ( 2*TABSIZ, SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

     
      CALL SYPUTI ( 'NEWNAME', I, 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NAMETABLEFULL)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYPUTI:  pointer table overflow' )

      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ,    SYPTRS )
      CALL SSIZEI ( TABSIZ+2,  SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

      CALL SYPUTI ( 'NEWNAME', I, 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(POINTERTABLEFULL)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYPUTI:  value table overflow' )


      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ+1,  SYPTRS )
      CALL SSIZEI ( TABSIZ,    SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

      CALL SYPUTI ( 'NEWNAME', I, 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYPUTI: invalid value count' )


      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ+1,  SYPTRS )
      CALL SSIZEI ( TABSIZ+1,  SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETI ( NAME, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

      CALL SYPUTI ( 'NEWNAME', I, -1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARGUMENT)', OK )


C
C     SYRENI:
C     
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYRENI:  rename non-existent symbol' )

      CALL SYRENI ( 'NONNAME', 'NEWNAME', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHSYMBOL)', OK )



C
C
C     SYSELI:  No errors are detected by this routine.
C
C
C     SYSETI:     
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYSETI:  name table overflow' )

C
C     We'll create a small symbol table so we can test the handling of
C     overflow conditions.
C
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

C
C --- Case: ------------------------------------------------------
C
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

C
C --- Case: ------------------------------------------------------
C
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


C     SYTRNI:     
C
C --- Case: ------------------------------------------------------
C
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

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYTRNI:  first index > second index' )

      CALL SYTRNI ( NAME, 2, 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYTRNI:  second index > symbol size' )

      CALL SYTRNI ( NAME, 1, TABSIZ+1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYTRNI:  non-error case: attempt transposition '//
     .              'of values of non-existent symbol.'             )

      CALL SYTRNI ( 'NONAME', 1, 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure the symbol table wasn't touched.
C
C     Check the structure of the cells we've populated.
C
C     The symbol name cell first:
C
      CALL CHCKSI ( 'Card(SYNAMS)', CARDC(SYNAMS), '=', TABSIZ, 0, OK )
      
      DO I = 1, CARDC(SYNAMS)

         TITLE = 'Name #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, SYNAMS(I), '=', XNAMES(I), OK )

      END DO 

C
C     Then the "pointer" cell.  The pointers are actually element
C     counts.
C
      CALL CHCKSI ( 'Card(SYPTRS)', CARDI(SYPTRS), '=', TABSIZ, 0, OK )

      CALL CHCKAI ( 'SYPTRS', SYPTRS(1), '=', XPTRS, TABSIZ, OK )
      
C
C     Then the value cell.
C
      CALL CHCKSI ( 'Card(SYVALS)', CARDI(SYVALS), '=', TABSIZ, 0, OK )

      CALL CHCKAI ( 'SYVALS', SYVALS(1), '=', XVALS, TABSIZ, OK )


C
C     That's all, folks!
C

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END



C
C     The utility routine T_ITHSYM creates the name of the ith
C     symbol.  The names created by this routine have the form
C  
C        XXX...XXXnnn
C
C     where nnn is the input number N, printed with a sufficient
C     number of leading zeros so that all numbers in the range
C     1:NMAX have the same width.  The number is padded with
C     leading 'X' characters up to the length of the string NAME.
C
      SUBROUTINE T_ITHSYM ( N, NMAX, NAME )
      IMPLICIT NONE

      INTEGER               N
      INTEGER               NMAX
      CHARACTER*(*)         NAME

C
C     SPICELIB functions
C

C
C     Local parameters
C
      INTEGER               PICLEN
      PARAMETER           ( PICLEN = 11 )

C
C     Local variables
C
      CHARACTER*(PICLEN)    PICTUR

      INTEGER               I
      INTEGER               NPAD
      INTEGER               NPICT
      INTEGER               SLEN
      INTEGER               NDIGIT
      
C
C     Get the length of the output name.
C
      SLEN  =  LEN(NAME)

C
C     Determine the number of digits required.
C
      NDIGIT =  INT (  LOG10 ( DBLE(NMAX)+0.5D0 )  )  +  1

C
C     Create the format picture for the names.
C
      PICTUR = '0'
      NPICT  = MIN ( SLEN, NDIGIT )

      DO I = 2, NPICT
         PICTUR(I:I) = 'X'
      END DO

      CALL DPFMT ( DBLE(N), PICTUR, NAME )

C
C     Add non-blank leading padding to the name.
C
      NPAD = SLEN - NPICT

      IF ( NPAD .GT. 0 ) THEN

         CALL RJUST ( NAME, NAME )
                  
         DO I = 1, NPAD
            NAME(I:I) = 'X'
         END DO

      END IF

      END


