!$Procedure SYDUPI ( Create a duplicate of a symbol )

      SUBROUTINE SYDUPI ( NAME, COPY, TABSYM, TABPTR, TABVAL )

! $ Abstract
!
!     Create a duplicate of a symbol within an integer symbol table.
!     If a symbol with the new name already exists, its components
!     are replaced.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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
      CHARACTER*(*)         COPY
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      INTEGER               TABVAL     ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol to be duplicated.
!     COPY       I   Name of the new symbol.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol to be duplicated.
!
!              The components associated with NAME will be given to the
!              new symbol COPY. If NAME is not in the symbol table,
!              no duplicate symbol can be made.
!
!     COPY     is the name of the new symbol. If a symbol with the name
!              COPY already exists in the symbol table, its components
!              are replaced by the components of NAME.
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
!              On output, the symbol table contains a new symbol COPY
!              whose components are the same as the components of NAME.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the symbol NAME is not in the symbol table, the error
!         SPICE(NOSUCHSYMBOL) is signaled.
!
!     2)  If duplication of the symbol causes an overflow in the
!         name table, the error SPICE(NAMETABLEFULL) is signaled.
!
!     3)  If duplication of the symbol causes an overflow in the
!         pointer table, the error SPICE(POINTERTABLEFULL) is signaled.
!
!     4)  If duplication of the symbol causes an overflow in the
!         value table, the error SPICE(VALUETABLEFULL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     If the symbol NAME is not in the symbol table, no duplicate symbol
!     can be made.
!
!     If the symbol COPY is already in the symbol table, its components
!     are replaced by the components of NAME.
!
! $ Examples
!
!     The contents of the symbol table are:
!
!        books   -->   5
!        erasers -->   6
!        pencils -->  12
!        pens    -->  10
!                     12
!                     24
!
!     The code,
!
!        CALL SYDUPI ( 'books', 'tablets', TABSYM, TABPTR, TABVAL )
!
!     produces the symbol table:
!
!        books   -->   5
!        erasers -->   6
!        pencils -->  12
!        pens    -->  10
!                     12
!                     24
!        tablets -->   5
!
!     The code,
!
!        CALL SYDUPC ( 'desks', 'chairs', TABSYM, TABPTR, TABVAL )
!
!     produces the error SPICE(NOSUCHSYMBOL) because the symbol
!     "desks" is not in the symbol table.
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
!     create a duplicate of a symbol
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

      INTEGER               LOCSYM  ( 2 )
      INTEGER               LOCVAL  ( 2 )
      INTEGER               DIMVAL  ( 2 )
      LOGICAL               OLDSYM  ( 2 )
      INTEGER               NEWSYM
      INTEGER               NEWVAL
      INTEGER               I


!
!     Standard SPICE error handling
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYDUPI' )
      END IF

!
!     How many symbols to start with?
!
      NSYM = CARDC ( TABSYM )
      NPTR = CARDI ( TABPTR )
      NVAL = CARDI ( TABVAL )

!
!     Where do these symbols belong? Are they already in the table?
!
      LOCSYM(1) = LSTLEC ( NAME, NSYM, TABSYM(1) )
      LOCSYM(2) = LSTLEC ( COPY, NSYM, TABSYM(1) )

      OLDSYM(1) = (       LOCSYM(1)         .NE. 0 &
                    .AND. TABSYM(LOCSYM(1)) .EQ. NAME )

      OLDSYM(2) = (       LOCSYM(2)         .NE. 0 &
                    .AND. TABSYM(LOCSYM(2)) .EQ. COPY )

!
!     If the original symbol is not in the table, we can't make a copy.
!
      IF ( .NOT. OLDSYM(1) ) THEN

         CALL SETMSG ( 'SYDUPI: The symbol to be duplicated, #, is ' // &
                       'not in the symbol table.' )
         CALL ERRCH  ( '#', NAME )
         CALL SIGERR ( 'SPICE(NOSUCHSYMBOL)' )

!
!     Otherwise, we need to know the dimension, to check for overflow.
!
      ELSE

         LOCVAL(1) = SUMAI  ( TABPTR(1), LOCSYM(1)-1 ) + 1
         DIMVAL(1) = TABPTR(LOCSYM(1))

!
!        If the new symbol already exists, we need to know its dimension
!        too, for the same reason.
!
         IF ( OLDSYM(2) ) THEN
            LOCVAL(2) = SUMAI ( TABPTR(1), LOCSYM(2)-1 ) + 1
            DIMVAL(2) = TABPTR(LOCSYM(2))
            NEWSYM    = 0
         ELSE
            LOCVAL(2) = SUMAI ( TABPTR(1), LOCSYM(2) ) + 1
            DIMVAL(2) = 0
            NEWSYM    = 1
         END IF

         NEWVAL = DIMVAL(1) - DIMVAL(2)

!
!        Can we make a copy without overflow?
!
         IF ( NSYM+NEWSYM .GT. SIZEC ( TABSYM ) ) THEN

            CALL SETMSG ( 'SYDUPI: Duplication of the symbol # '      // &
                          'causes an overflow in the name table.' )
            CALL ERRCH  ( '#', NAME )
            CALL SIGERR ( 'SPICE(NAMETABLEFULL)' )


         ELSE IF ( NPTR+NEWSYM .GT. SIZEI ( TABPTR ) ) THEN

            CALL SETMSG ( 'SYDUPI: Duplication of the symbol # '      // &
                          'causes an overflow in the pointer table.' )
            CALL ERRCH  ( '#', NAME )
            CALL SIGERR ( 'SPICE(POINTERTABLEFULL)' )


         ELSE IF ( NVAL+NEWVAL .GT. SIZEI ( TABVAL ) ) THEN

            CALL SETMSG ( 'SYDUPI: Duplication of the symbol # '      // &
                          'causes an overflow in the value table.' )
            CALL ERRCH  ( '#', NAME )
            CALL SIGERR ( 'SPICE(VALUETABLEFULL)' )

!
!        Looks like we can.
!
         ELSE

!
!           If the copy exists, remove the current contents and
!           change the dimension. Otherwise add the new name and
!           dimension to the name and pointer tables.
!
            IF ( DIMVAL(2) .GT. 0 ) THEN
              CALL REMLAI ( DIMVAL(2), LOCVAL(2), TABVAL(1), NVAL )
              CALL SCARDI ( NVAL, TABVAL )

              TABPTR(LOCSYM(2)) = DIMVAL(1)

              IF ( LOCVAL(1) .GT. LOCVAL(2) ) THEN
                 LOCVAL(1) = LOCVAL(1) - DIMVAL(2)
              END IF

            ELSE
              CALL INSLAC ( COPY, 1, LOCSYM(2)+1, TABSYM(1), NSYM )
              CALL SCARDC ( NSYM, TABSYM )

              CALL INSLAI ( DIMVAL(1), 1, LOCSYM(2)+1, TABPTR(1), NPTR )
              CALL SCARDI ( NPTR, TABPTR )
            END IF

!
!           In either case, allocate space for the new symbol values,
!           and copy them in one by one. (INSLAx won't work if the
!           copy is earlier in the table than the original.)
!
            DO I = NVAL, LOCVAL(2), -1
              TABVAL(I+DIMVAL(1)) = TABVAL(I)
            END DO

            IF ( LOCVAL(1) .GT. LOCVAL(2) ) THEN
               LOCVAL(1) = LOCVAL(1) + DIMVAL(1)
            END IF

            DO I = 0, DIMVAL(1) - 1
              TABVAL(LOCVAL(2)+I) = TABVAL(LOCVAL(1)+I)
            END DO

            CALL SCARDI ( NVAL + DIMVAL(1), TABVAL )

         END IF

      END IF


      CALL CHKOUT ( 'SYDUPI' )
      RETURN
      END
