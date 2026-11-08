C$Procedure F_SYMTBD ( Test SPICELIB d.p. symbol table routines )
 
      SUBROUTINE F_SYMTBD ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB symbol table routines of type DOUBLE
C     PRECISION.
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
C        SYDELD
C        SYDIMD
C        SYDUPD
C        SYENQD
C        SYFETD
C        SYGETD
C        SYNTHD
C        SYORDD
C        SYPOPD
C        SYPSHD
C        SYPUTD
C        SYREND
C        SYSELD
C        SYSETD
C        SYTRND
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
C-    TSPICE Version 1.0.0, 01-NOV-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               CARDC
      INTEGER               CARDD
      INTEGER               CARDI
      INTEGER               SYDIMD

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

      DOUBLE PRECISION      DVALS  (          MAXVAL )
      DOUBLE PRECISION      SYVALS ( LBCELL : MAXVAL )
      DOUBLE PRECISION      VAL
      DOUBLE PRECISION      XVAL
      DOUBLE PRECISION      XVALS  (          MAXVAL )

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

C
C     Saved values
C
C     To avoid memory problems under cygwin, save everything.
C
      SAVE
C

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
      CALL TOPEN ( 'F_SYMTBD' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYSETD test:  Populate a simple symbol table ' //
     .             'with scalar-valued symbols.'                   )

C
C     Initialize the symbol table.
C
      CALL SSIZEC ( MAXTAB, SYNAMS )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL SSIZEI ( MAXTAB, SYPTRS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXVAL, SYVALS )
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
         CALL SYSETD ( NAME, DBLE(-I), SYNAMS, SYPTRS, SYVALS )
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
      CALL CHCKSI ( 'Card(SYVALS)', CARDD(SYVALS), '=', MAXTAB, 0, OK )

      CALL CHCKAD ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYSETD test:  Populate a simple symbol table ' //
     .             'with scalar-valued symbols.  Insert symbols '  //
     .             'in reverse order.'                             )

C
C     Clear out the symbol table.
C
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDD ( 0,  SYVALS )

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
         CALL SYSETD ( XNAMES(J), DBLE(-J), SYNAMS, SYPTRS, SYVALS )
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
      CALL CHCKSI ( 'Card(SYVALS)', CARDD(SYVALS), '=', MAXTAB, 0, OK )

      CALL CHCKAD ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, 0.D0, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYPUTD test:  Populate a simple symbol table ' //
     .             'with scalar-valued symbols.'                   )

C
C     This is essentially a reprise of the previous SYSETD test.
C
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDD ( 0,  SYVALS )


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
         CALL SYPUTD ( NAME, XVALS(I), 1, SYNAMS, SYPTRS, SYVALS )
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
      CALL CHCKSI ( 'Card(SYVALS)', CARDD(SYVALS), '=', MAXTAB, 0, OK )

      CALL CHCKAD ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYFETD test:  Fetch names from symbol table ' //
     .             'with scalar-valued symbols.'                   )


      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYFETD( I, SYNAMS, SYPTRS, SYVALS, NAME, FOUND )
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

      CALL SYFETD( MAXTAB+1, SYNAMS, SYPTRS, SYVALS, NAME, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYGETD test:  Fetch values from symbol table ' //
     .             'with scalar-valued symbols.'                   )


      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYGETD( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, VAL, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         TITLE = 'Value of symbol #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         
         CALL CHCKSD ( TITLE, VAL, '=', XVALS(I), 0.D0, OK )
         
      END DO 

C
C     Also look for a symbol we know isn't there.
C
      NAME = 'NOT_THERE'

      CALL SYGETD( NAME,
     .             SYNAMS, SYPTRS, SYVALS, NVALS, VAL, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TITLE = 'Was name # found?'
      CALL REPMC  ( TITLE, '#',    NAME,   TITLE )
      CALL CHCKSL ( TITLE, FOUND, .FALSE., OK    )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYREND test:  Rename symbols in a simple ' //
     .             'symbol table with scalar-valued symbols.' )


      DO I = 1, MAXTAB
C
C        Get the name and value of the ith symbol.
C
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         CALL SYGETD ( NAME,  SYNAMS, SYPTRS, SYVALS, 
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
         CALL SYREND ( NAME, NEWNAM, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Make sure the old symbol is gone.
C
         CALL SYGETD ( NAME,  SYNAMS, SYPTRS, SYVALS, 
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
         CALL SYGETD ( NEWNAM,  SYNAMS, SYPTRS, SYVALS, 
     .                 NVALS,   VAL,    FOUND           )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

C
C        Check the value.
C
         CALL CHCKSD ( 'VAL', VAL, '=', XVAL, 0.D0, OK )

C
C        Change the name back to its original value.
C
         CALL SYREND ( NEWNAM, NAME, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO 



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYDELD test:  Delete symbols from symbol table ' //
     .             'with scalar-valued symbols.'                   )

      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYDELD( XNAMES(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Make sure the symbol is gone.
C
         TITLE = 'Was (deleted symbol) name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYGETD( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, VAL, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )

C
C        Validate the remaining symbol table.
C
         DO J = I+1, MAXTAB

            TITLE = 'Was (remaining symbol) name # found?'
            CALL REPMI ( TITLE, '#', I, TITLE )

            CALL SYGETD ( XNAMES(J),
     .                    SYNAMS, SYPTRS, SYVALS, NVALS, VAL, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

            TITLE = 'Remaining symbol # value'
            CALL REPMI ( TITLE, '#', I, TITLE )

            CALL CHCKSD ( TITLE, VAL, '=', XVALS(J), 0.D0, OK )

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
      CALL TCASE ( 'SYPUTD test:  Create symbol table ' //
     .             'with array-valued symbols.'         )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDD ( 0,  SYVALS )

      
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
         CALL SYPUTD ( NAME, XVALS(START), I, SYNAMS, SYPTRS, SYVALS )
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

      CALL CHCKSI ( 'Card(SYVALS)', CARDD(SYVALS), '=', XCARD, 0, OK )

      CALL CHCKAD ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYPUTD test:  Populate a simple symbol table ' //
     .             'with scalar-valued symbols.  Insert symbols '  //
     .             'in reverse order.'                             )

C
C     Clear out the symbol table.
C
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDD ( 0,  SYVALS )

      CALL CLEARD ( MAXVAL, XVALS )

C
C     Use the symbol names and values from the previous test.
C
      SIZE  = MAXTAB * ( MAXTAB + 1 ) / 2

      START = SIZE - MAXTAB + 1

      DO I = MAXTAB, 1, -1
C
C        Add the symbol to the table.
C
         CALL SYPUTD ( XNAMES(I), XVALS(START), I, 
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

      CALL CHCKSI ( 'Card(SYVALS)', CARDD(SYVALS), '=', XCARD, 0, OK )

      CALL CHCKAD ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, 0.D0, OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYGETD test:  validate array-valued symbol '//
     .             'table by fetching values associated with '  //
     .             'each symbol.'                               )


      START = 1

      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYGETD( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, DVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol #'
            CALL REPMI ( TITLE, '#', I, TITLE )
         
            CALL CHCKAD ( TITLE, DVALS, '=', XVALS(START), I, 0.D0, OK )

         END IF         

         START = START + I

      END DO 


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYNTHD test:  validate array-valued symbol '//
     .             'table by fetching values associated with '  //
     .             'each symbol.'                               )


      START = 1

      DO I = 1, MAXTAB

         TITLE = 'Was name # found?'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL SYGETD( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, DVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN
C
C           Get the size of the current symbol.
C
            SIZE = SYDIMD ( XNAMES(I), SYNAMS, SYPTRS, SYVALS )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO J = 1, SIZE
C
C              Fetch each value.
C
               TITLE = 'Value # of symbol #'
               CALL REPMI ( TITLE, '#', I,         TITLE )
               CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )
   
               CALL SYNTHD( XNAMES(I), J,     SYNAMS, SYPTRS, SYVALS, 
     .                      VAL,       FOUND                          )
               CALL CHCKXC ( .FALSE., ' ', OK )
           
               CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

               CALL CHCKSD ( TITLE, VAL, '=', 
     .                              XVALS(START+J-1), 0.D0, OK )
         
            END DO

C
C           Try to fetch an element we know isn't there.
C
            J = SIZE + 1

            TITLE = 'Value # of symbol #'
            CALL REPMI ( TITLE, '#', J,         TITLE )
            CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )
   
            CALL SYNTHD( XNAMES(I), J, SYNAMS, SYPTRS, SYVALS, 
     .                      VAL,       FOUND                   )
            CALL CHCKXC ( .FALSE., ' ', OK )
           
            CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )
         
         END IF

         START = START + I

      END DO 


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYENQD test:  Create symbol table ' //
     .             'with array-valued symbols.'         )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDD ( 0,  SYVALS )

      
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

            CALL SYENQD ( NAME, XVALS(TO), SYNAMS, SYPTRS, SYVALS )
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

      CALL CHCKSI ( 'Card(SYVALS)', CARDD(SYVALS), '=', XCARD, 0, OK )

      CALL CHCKAD ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYPSHD test:  Create symbol table ' //
     .             'with array-valued symbols.'         )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDD ( 0,  SYVALS )

      
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

            CALL SYPSHD ( NAME, XVALS(TO), SYNAMS, SYPTRS, SYVALS )
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

      CALL CHCKSI ( 'Card(SYVALS)', CARDD(SYVALS), '=', XCARD, 0, OK )

      CALL CHCKAD ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, 0.D0, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYDUPD test:  Create symbol table '         //
     .             'with array-valued symbols.  Then duplicate '//
     .             'each symbol.'                               )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDD ( 0,  SYVALS )


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

            CALL SYENQD ( NAME, XVALS(TO), SYNAMS, SYPTRS, SYVALS )
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

         CALL SYGETD( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, DVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (0)'
            CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )
         
            CALL CHCKAD ( TITLE, DVALS, '=', XVALS(START), I, 0.D0, OK )

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
         
         CALL SYDUPD ( NAME, NEWNAM, SYNAMS, SYPTRS, SYVALS )
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

         CALL SYGETD( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, DVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (1)'
            CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )
         
            CALL CHCKAD ( TITLE, DVALS, '=', XVALS(START), I, 0.D0, OK )

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

         CALL SYGETD( NEWNAM,
     .                SYNAMS, SYPTRS, SYVALS, NVALS, DVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (2)'
            CALL REPMC ( TITLE, '#', NEWNAM, TITLE )
         
            CALL CHCKAD ( TITLE, DVALS, '=', XVALS(START), I, 0.D0, OK )

         END IF         

         START = START + I

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYDUPD test:  Create symbol table '             //
     .             'with array-valued symbols.  Duplicate these '   //
     .             'symbols.  Negate the values of the original '   //
     .             'symbols.  Then duplicate the duplicate '        //
     .             'the symbols, overwriting the original symbols.' )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDD ( 0,  SYVALS )

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

            CALL SYENQD ( NAME, XVALS(TO), SYNAMS, SYPTRS, SYVALS )
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
         
         CALL SYDUPD ( NAME, NEWNAM, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

C
C     Negate the values of the symbols of the original symbols.
C
      DO I = 1, UB

         CALL SYGETD( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, DVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         DO J = 1, NVALS
            DVALS(J) = -DVALS(J)
         END DO

         CALL SYPUTD( XNAMES(I),  DVALS,   NVALS,   
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
         
         CALL SYDUPD ( NEWNAM, NAME, SYNAMS, SYPTRS, SYVALS )
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

         CALL SYGETD( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, DVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (1)'
            CALL REPMC ( TITLE, '#', XNAMES(I), TITLE )
         
            CALL CHCKAD ( TITLE, DVALS, '=', XVALS(START), I, 0.D0, OK )

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

         CALL SYGETD( XNAMES(I),
     .                SYNAMS, SYPTRS, SYVALS, NVALS, DVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            TITLE = 'Values of symbol # (2)'
            CALL REPMC ( TITLE, '#', NEWNAM, TITLE )
         
            CALL CHCKAD ( TITLE, DVALS, '=', XVALS(START), I, 0.D0, OK )

         END IF         

         START = START + I

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYORDD test:  Create symbol table '      //
     .             'with array-valued symbols; sort values ' //
     .             'associated with each symbol.'            )

      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDD ( 0,  SYVALS )

      CALL CLEARD ( MAXVAL, XVALS )

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

            CALL SYENQD ( NAME, VAL, SYNAMS, SYPTRS, SYVALS )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

C
C     Sort the symbol values.
C     
      DO I = 1, MAXTAB

         CALL SYORDD  ( XNAMES(I), SYNAMS, SYPTRS, SYVALS ) 

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
      CALL CHCKAD ( 'SYVALS', SYVALS(1), '=', XVALS, MAXTAB, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYDIMD test:  Find dimensions of symbols in ' //
     .             'symbol table with array-valued symbols.'      )


      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDD ( 0,  SYVALS )

      CALL CLEARD ( MAXVAL, XVALS )

     
      DO I = 1, MAXTAB
C
C        Create the symbol name.
C
         CALL T_ITHSYM ( I, MAXTAB, NAME )

         XNAMES(I) = NAME

         DO J = 1, I

            VAL  =  - MAXTAB * I - J

            CALL SYENQD ( NAME, VAL, SYNAMS, SYPTRS, SYVALS )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
         END DO

      END DO

      DO I = 1, MAXTAB

         NVALS =  SYDIMD( XNAMES(I), SYNAMS, SYPTRS, SYVALS  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'Dimension of symbol #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         
         CALL CHCKSI ( TITLE, NVALS, '=', I, 0, OK )

      END DO 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYPOPD test:  Pop values from symbol table ' //
     .             'with array-valued symbols.'                 )
      

C
C     Re-create the default array-valued symbol table.
C
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDD ( 0,  SYVALS )

      
      DO I = 1, MAXTAB
C
C        Create the symbol name.
C
         CALL T_ITHSYM ( I, MAXTAB, NAME )

C
C        Set the values of the Ith symbol.
C        
         DO J = 1, I

            DVALS(J)  =  - MAXTAB * I - J
            
         END DO

C
C        Insert the Ith symbol.
C
         CALL SYPUTD ( NAME, DVALS, I, SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


C
C     Now test SYPOPD.
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

            CALL SYPOPD ( NAME, SYNAMS, SYPTRS, SYVALS, VAL, FOUND )
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

               CALL CHCKSD ( TITLE, VAL, '=', XVALS(TO), 0.D0, OK )

            END IF

            TO    =    TO + 1

         END DO

C
C        Try to pop a value from the now empty symbol.
C
         CALL SYPOPD ( NAME, SYNAMS, SYPTRS, SYVALS, VAL, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'Was element popped from symbol #?'
         CALL REPMC ( TITLE, '#', NAME, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )
 
         CALL CHCKSL ( TITLE, FOUND, .FALSE., OK )

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SYTRND test:  Transpose values from symbol ' //
     .             'table with array-valued symbols.'            )

C
C     We'll create a small version of our array-valued symbol table.
C     Since we want to try all possible transpositions, there would
C     be an excessive number of calls to SYTRND if we used the full
C     set of MAXTAB symbols.
C
C     Create the symbol table now.
C
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDD ( 0,  SYVALS )

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
         CALL SYPUTD ( NAME,  XVALS(START), I, SYNAMS, SYPTRS, SYVALS )
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

               CALL SYTRND ( NAME, J, K, SYNAMS, SYPTRS, SYVALS )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Fetch the values of the modified symbol.
C               
               CALL SYGETD ( NAME,  SYNAMS, SYPTRS, SYVALS,
     .                       NVALS, DVALS,  FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

               IF ( FOUND ) THEN
C
C                 If DVALS reflects the correct transposition,
C                 then we can swap elements J and K of DVALS
C                 to obtain the original symbol's values.
C
                  IF ( J .NE. K ) THEN
                     CALL SWAPD ( DVALS(J), DVALS(K) )
                  END IF

                  TITLE = 'Values of symbol #:  J = #; K = #'
                  CALL REPMC  ( TITLE, '#', NAME, TITLE )
                  CALL REPMI  ( TITLE, '#', J,    TITLE )
                  CALL REPMI  ( TITLE, '#', K,    TITLE )
         
                  CALL CHCKAD ( TITLE, DVALS, '=', 
     .                                 XVALS(START), I, 0.D0, OK )
                  
               END IF

C
C              Undo the transposition in the symbol table.
C              
               CALL SYTRND ( NAME, J, K, SYNAMS, SYPTRS, SYVALS )
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
      CALL TCASE ( 'SYSELD test:  Extract slice of values from ' //
     .             'symbol table with array-valued symbols.'     )

C
C     This approach used in this test case closely parallels that
C     of the SYTRND test case above.
C
C     We'll create a small version of our array-valued symbol table.
C     Since we want to try all possible slices, there would
C     be an excessive number of calls to SYTRND if we used the full
C     set of MAXTAB symbols.
C
C     Create the symbol table now.
C
      CALL SCARDC ( 0,  SYNAMS )
      CALL SCARDI ( 0,  SYPTRS )
      CALL SCARDD ( 0,  SYVALS )

      CALL CLEARD ( MAXVAL, XVALS )

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
         CALL SYPUTD ( NAME,  XVALS(START), I, SYNAMS, SYPTRS, SYVALS )
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
               CALL SYSELD ( NAME,   J,      K, 
     .                       SYNAMS, SYPTRS, SYVALS, DVALS, FOUND )
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
         
                  CALL CHCKAD ( TITLE, 
     .                          DVALS, 
     .                          '=', 
     .                          XVALS(START+J-1), 
     .                          K-J+1, 
     .                          0.D0, 
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
C     SYDELD:  No errors are detected by this routine.
C 
C     SYDIMD:  No errors are detected by this routine.
C     
C
C     SYDUPD:
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYDUPD:  symbol name not present' )
 
      CALL SYDUPD ( 'NOSYMBOL', 'NOSYMBOL2', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHSYMBOL)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYDUPD:  name table overflow' )

C
C     We'll create a small symbol table so we can test the handling of
C     overflow conditions.
C
      TABSIZ = 5

      CALL SSIZEC ( TABSIZ,   SYNAMS )
      CALL SSIZEI ( 2*TABSIZ, SYPTRS )
      CALL SSIZED ( 2*TABSIZ, SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETD ( NAME, DBLE(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

      CALL SYDUPD ( NAME, 'NEWNAME', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NAMETABLEFULL)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYDUPD:  pointer table overflow' )

      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ,    SYPTRS )
      CALL SSIZED ( TABSIZ+2,  SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETD ( NAME, DBLE(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

      CALL SYDUPD ( NAME, 'NEWNAME', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(POINTERTABLEFULL)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYDUPD:  value table overflow' )


      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ+1,  SYPTRS )
      CALL SSIZED ( TABSIZ,    SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETD ( NAME, DBLE(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

      CALL SYDUPD ( NAME, 'NEWNAME', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )

C
C
C     SYENQD:
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYENQD:  value table overflow' )


      TABSIZ = 5

      CALL SSIZEC ( TABSIZ,  SYNAMS )
      CALL SSIZEI ( TABSIZ,  SYPTRS )
      CALL SSIZED ( TABSIZ,  SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETD ( NAME, DBLE(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

C
C     Enqueue a value onto the last symbol.
C
      CALL SYENQD ( NAME, DBLE(-1), SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )

C
C
C
C     SYFETD:  No errors are detected by this routine.
C
C     SYGETD:  No errors are detected by this routine.
C
C     SYNTHD:  No errors are detected by this routine.
C 
C     SYORDD:  No errors are detected by this routine.
C 
C     SYPOPD:  No errors are detected by this routine.
C
C
C
C     SYPSHD:
C
C --- Case: ------------------------------------------------------
C
C
      CALL TCASE  ( 'SYPSHD:  value table overflow' )


      TABSIZ = 5

      CALL SSIZEC ( TABSIZ,  SYNAMS )
      CALL SSIZEI ( TABSIZ,  SYPTRS )
      CALL SSIZED ( TABSIZ,  SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETD ( NAME, DBLE(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

C
C     Push a value onto the last symbol.
C
      CALL SYPSHD ( NAME, DBLE(-1), SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )




C
C     SYPUTD:
C     
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYPUTD:  name table overflow' )

C
C     We'll create a small symbol table so we can test the handling of
C     overflow conditions.
C
      TABSIZ = 5

      CALL SSIZEC ( TABSIZ,   SYNAMS )
      CALL SSIZEI ( 2*TABSIZ, SYPTRS )
      CALL SSIZED ( 2*TABSIZ, SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETD ( NAME, DBLE(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

     
      CALL SYPUTD ( 'NEWNAME', DBLE(I), 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NAMETABLEFULL)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYPUTD:  pointer table overflow' )

      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ,    SYPTRS )
      CALL SSIZED ( TABSIZ+2,  SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETD ( NAME, DBLE(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

      CALL SYPUTD ( 'NEWNAME', DBLE(I), 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(POINTERTABLEFULL)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYPUTD:  value table overflow' )


      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ+1,  SYPTRS )
      CALL SSIZED ( TABSIZ,    SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETD ( NAME, DBLE(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

      CALL SYPUTD ( 'NEWNAME', DBLE(I), 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYPUTD: invalid value count' )


      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ+1,  SYPTRS )
      CALL SSIZED ( TABSIZ+1,  SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETD ( NAME, DBLE(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

      CALL SYPUTD ( 'NEWNAME', DBLE(I), -1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARGUMENT)', OK )


C
C     SYREND:
C     
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYREND:  rename non-existent symbol' )

      CALL SYREND ( 'NONNAME', 'NEWNAME', SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHSYMBOL)', OK )



C
C
C     SYSELD:  No errors are detected by this routine.
C
C
C     SYSETD:     
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYSETD:  name table overflow' )

C
C     We'll create a small symbol table so we can test the handling of
C     overflow conditions.
C
      TABSIZ = 5

      CALL SSIZEC ( TABSIZ,   SYNAMS )
      CALL SSIZEI ( 2*TABSIZ, SYPTRS )
      CALL SSIZED ( 2*TABSIZ, SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETD ( NAME, DBLE(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

     
      CALL SYSETD ( 'NEWNAME', DBLE(I), SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NAMETABLEFULL)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYSETD:  pointer table overflow' )

      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ,    SYPTRS )
      CALL SSIZED ( TABSIZ+2,  SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETD ( NAME, DBLE(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

      CALL SYSETD ( 'NEWNAME', DBLE(I), SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(POINTERTABLEFULL)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYSETD:  value table overflow' )


      CALL SSIZEC ( TABSIZ+1,  SYNAMS )
      CALL SSIZEI ( TABSIZ+1,  SYPTRS )
      CALL SSIZED ( TABSIZ,    SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETD ( NAME, DBLE(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
      END DO

      CALL SYSETD ( 'NEWNAME', DBLE(I), SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUETABLEFULL)', OK )


C     SYTRND:     
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYTRND:  first index < 1' )


      CALL SSIZEC ( TABSIZ,  SYNAMS )
      CALL SSIZEI ( TABSIZ,  SYPTRS )
      CALL SSIZED ( TABSIZ,  SYVALS )


      DO I = 1, TABSIZ
         
         CALL T_ITHSYM ( I, TABSIZ, NAME )
         
         CALL SYSETD ( NAME, DBLE(I), SYNAMS, SYPTRS, SYVALS )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         XNAMES(I) = NAME
         XPTRS (I) = 1
         XVALS (I) = I

      END DO

      CALL SYTRND ( NAME, -1, 2, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYTRND:  first index > second index' )

      CALL SYTRND ( NAME, 2, 1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYTRND:  second index > symbol size' )

      CALL SYTRND ( NAME, 1, TABSIZ+1, SYNAMS, SYPTRS, SYVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SYTRND:  non-error case: attempt transposition '//
     .              'of values of non-existent symbol.'             )

      CALL SYTRND ( 'NONAME', 1, 1, SYNAMS, SYPTRS, SYVALS )

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
      CALL CHCKSI ( 'Card(SYVALS)', CARDD(SYVALS), '=', TABSIZ, 0, OK )

      CALL CHCKAD ( 'SYVALS', SYVALS(1), '=', XVALS, TABSIZ, 0.D0, OK )


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
C     The calling sequence of the utility is:
C
C        SUBROUTINE T_ITHSYM ( N, NMAX, NAME )
C
C     This utility is defined in the file
C
C        f_symtbi.for
C
