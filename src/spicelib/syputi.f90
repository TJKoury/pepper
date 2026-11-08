!$Procedure SYPUTI ( Set the values associated with a symbol )

      SUBROUTINE SYPUTI ( NAME, VALUES, N, TABSYM, TABPTR, TABVAL )

! $ Abstract
!
!     Set the values of a particular symbol in an integer symbol table.
!     If the symbol already exists, the previous values associated with
!     it are removed, otherwise a new symbol is created.
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
      INTEGER               VALUES     (        * )
      INTEGER               N
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      INTEGER               TABVAL     ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol whose associated values are to
!                    be put into the symbol table.
!     VALUES     I   Values to be associated with the symbol NAME.
!     N          I   Number of values in VALUES.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose associated values are to
!              be set.
!
!              If NAME has values associated with it, they are removed,
!              and the elements of VALUES become the values associated
!              with NAME. If NAME is not in the symbol table, a new
!              symbol is created, provided there is room in the symbol
!              table.
!
!     VALUES   are the new values associated with the symbol NAME.
!
!     N        is the number of elements in the VALUES array.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of an integer symbol table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of an integer symbol table.
!
!              If NAME has values associated with it, they are removed,
!              and the elements of VALUES become the values associated
!              with NAME. If NAME is not in the symbol table, a new
!              symbol is created, provided there is room in the symbol
!              table.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the addition of a new symbol causes an overflow in the
!         name table, the error SPICE(NAMETABLEFULL) is signaled.
!
!     2)  If the addition of a new symbol causes an overflow in the
!         pointer table, the error SPICE(POINTERTABLEFULL) is signaled.
!
!     3)  If the addition of new values causes an overflow in the
!         value table, the error SPICE(VALUETABLEFULL) is signaled.
!
!     4)  If N < 1, the error SPICE(INVALIDARGUMENT) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This subroutine is like SYSETC, but SYPUTC allows **several**
!     values to be associated with a symbol.
!
!     If NAME has values associated with it, they are removed, and
!     the elements of VALUES become the values associated with NAME.
!     If NAME is not in the symbol table, a new symbol is created,
!     provided there is room in the symbol table.
!
! $ Examples
!
!     The contents of the symbol table are:
!
!        books   -->   5
!        erasers -->   6
!        pencils -->  12
!                     23
!        pens    -->  10
!                     12
!                     24
!
!     If VALUES contains the elements,
!
!          12
!          24
!          36
!
!     the call
!
!        CALL SYPUTI ( 'desks', VALUES, 3, TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        books   -->   5
!        desks   -->  12
!                     24
!                     36
!        erasers -->   6
!        pencils -->  12
!                     23
!        pens    -->  10
!                     12
!                     24
!     The call,
!
!        CALL SYPUTI ( 'pens', VALUES, 3, TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        books   -->   5
!        desks   -->  12
!                     24
!                     36
!        erasers -->   6
!        pencils -->  12
!                     23
!        pens    -->  12
!                     24
!                     36
!
!     Note that the previous values associated with "pens" have been
!     replaced by the values in VALUES.
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
!-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
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
!     set the values associated with a symbol
!
!-&


!
!     SPICELIB functions
!

      INTEGER               CARDC
      INTEGER               CARDI
      INTEGER               LSTLEC
      LOGICAL               RETURN
      INTEGER               SIZEC
      INTEGER               SIZEI
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
      LOGICAL               OLDSYM
      INTEGER               NEWSYM
      INTEGER               NEWVAL



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYPUTI' )
      END IF

!
!     Check to see if the number of values is a valid quantity.
!
      IF ( N .LT. 1 ) THEN
         CALL SETMSG ( 'SYPUTI: The dimension of the values array is' // &
                       'less than one.' )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)' )
         CALL CHKOUT ( 'SYPUTI' )
         RETURN
      END IF


!
!     How many symbols to start with?
!
      NSYM = CARDC ( TABSYM )
      NPTR = CARDI ( TABPTR )
      NVAL = CARDI ( TABVAL )

!
!     Where does this symbol belong? is it already in the table?
!
      LOCSYM = LSTLEC ( NAME, NSYM, TABSYM(1) )
      OLDSYM = ( LOCSYM .NE. 0  .AND.  TABSYM(LOCSYM) .EQ. NAME )

!
!     If the new symbol already exists, we need to know its dimension
!     to check for overflow.
!
      IF ( OLDSYM ) THEN
         LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1
         DIMVAL = TABPTR(LOCSYM)
         NEWSYM = 0
      ELSE
         LOCVAL = SUMAI ( TABPTR(1), LOCSYM ) + 1
         DIMVAL = 0
         NEWSYM = 1
      END IF

      NEWVAL = N - DIMVAL

!
!     Can we do this without overflow?
!

      IF ( NSYM+NEWSYM .GT. SIZEC ( TABSYM ) ) THEN

         CALL SETMSG ( 'SYPUTI: Addition of the new symbol # causes ' // &
                       'an overflow in the name table.' )
         CALL ERRCH  ( '#', NAME )
         CALL SIGERR ( 'SPICE(NAMETABLEFULL)' )


      ELSE IF ( NPTR+NEWSYM .GT. SIZEI ( TABPTR ) ) THEN

         CALL SETMSG ( 'SYPUTI: Addition of the new symbol # causes ' // &
                       'an overflow in the pointer table.' )
         CALL ERRCH  ( '#', NAME )
         CALL SIGERR ( 'SPICE(POINTERTABLEFULL)' )


      ELSE IF ( NVAL+NEWVAL .GT. SIZEI ( TABVAL ) ) THEN

         CALL SETMSG ( 'SYPUTC: Addition of the new symbol # causes ' // &
                       'an overflow in the value table.' )
         CALL ERRCH  ( '#', NAME )
         CALL SIGERR ( 'SPICE(VALUETABLEFULL)' )


!
!     Looks like we can.
!
      ELSE

!
!        If the symbol exists, remove the current contents and
!        change the dimension. Otherwise add the new name and
!        dimension to the name and pointer tables.
!
         IF ( DIMVAL .GT. 0 ) THEN
            CALL REMLAI ( DIMVAL, LOCVAL, TABVAL(1), NVAL )
            CALL SCARDI ( NVAL, TABVAL )

            TABPTR(LOCSYM) = N

         ELSE
            CALL INSLAC ( NAME, 1, LOCSYM+1, TABSYM(1), NSYM )
            CALL SCARDC ( NSYM, TABSYM )

            CALL INSLAI ( N,    1, LOCSYM+1, TABPTR(1), NPTR )
            CALL SCARDI ( NPTR, TABPTR )
         END IF

!
!        In either case, insert the values from the input array into
!        the value table.
!
         CALL INSLAI ( VALUES, N, LOCVAL, TABVAL(1), NVAL )
         CALL SCARDI ( NVAL, TABVAL )

      END IF


      CALL CHKOUT ( 'SYPUTI' )
      RETURN
      END
