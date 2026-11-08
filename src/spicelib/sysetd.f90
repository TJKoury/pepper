!$Procedure SYSETD ( Set the value associated with a symbol )

      SUBROUTINE SYSETD ( NAME, VALUE, TABSYM, TABPTR, TABVAL )

! $ Abstract
!
!     Set the value of a particular symbol in a double precision symbol
!     table. If the symbol already exists, the previous values
!     associated with it are removed, otherwise a new symbol is created.
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
      DOUBLE PRECISION      VALUE
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      DOUBLE PRECISION      TABVAL     ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol whose associated value is to be
!                    set.
!     VALUE      I   Associated value of the symbol NAME.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose associated value is to
!              be set.
!
!              If NAME has values associated with it, they are removed,
!              and VALUE becomes the only value associated with NAME. If
!              NAME is not in the symbol table, a new symbol is created,
!              provided there is room in the symbol table.
!
!     VALUE    is the new value associated with the symbol NAME.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a double precision symbol table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a double precision symbol table.
!
!              If NAME has values associated with it, they are removed,
!              and VALUE becomes the only value associated with NAME. If
!              NAME is not in the symbol table, a new symbol is created,
!              provided there is room in the symbol table.
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
!     3)  If the addition of a new symbol causes an overflow in the
!         value table, the error SPICE(VALUETABLEFULL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     If NAME has values associated with it, they are removed, and VALUE
!     becomes the only value associated with NAME. If NAME is not in the
!     symbol table, a new symbol is created, provided there is room in
!     the symbol table.
!
! $ Examples
!
!     The contents of the symbol table are:
!
!        DELTA_T_A -->    3.2184D1
!        K         -->    1.657D-3
!        MEAN_ANOM -->    6.239996D0
!                         1.99096871D-7
!        ORBIT_ECC -->    1.671D-2
!
!     The call,
!
!     CALL SYSETD ( 'ORBIT_ECC', 1.67125D-2, TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        DELTA_T_A -->    3.2184D1
!        K         -->    1.657D-3
!        MEAN_ANOM -->    6.239996D0
!                         1.99096871D-7
!        ORBIT_ECC -->    1.67125D-2
!
!     Note that the previous value associated with the symbol
!     "ORBIT_ECC" has been deleted, and now the value 1.67125D-2
!     is associated with the symbol.
!
!
!     The next call,
!
!     CALL SYSETD ( 'EB', 1.671D-2, TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        DELTA_T_A -->    3.2184D1
!        EB        -->    1.671D-2
!        K         -->    1.657D-3
!        MEAN_ANOM -->    6.239996D0
!                         1.99096871D-7
!        ORBIT_ECC -->    1.67125D-2
!
!     Note that the new symbol "EB" was created by the last call.
!     A new symbol is created only if there is room in the symbol
!     table.
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
!-    SPICELIB Version 1.1.0, 17-JUN-2021 (JDR)
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
!     set the value associated with a symbol
!
!-&


!
!     SPICELIB functions
!

      INTEGER               CARDC
      INTEGER               CARDD
      INTEGER               CARDI
      INTEGER               LSTLEC
      LOGICAL               RETURN
      INTEGER               SIZEC
      INTEGER               SIZED
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



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYSETD' )
      END IF

!
!     How many symbols to start with?
!
      NSYM = CARDC ( TABSYM )
      NPTR = CARDI ( TABPTR )
      NVAL = CARDD ( TABVAL )

!
!     Where does this symbol belong? Is it already in the table?
!
      LOCSYM = LSTLEC ( NAME, NSYM, TABSYM(1) )
      OLDSYM = ( LOCSYM .NE. 0  .AND.  TABSYM(LOCSYM) .EQ. NAME )

!
!     If it's already in the table, there's no chance of overflow.
!     Leave the name where it is. Remove all but one of the existing
!     values, replacing that with the new value. And set the dimension
!     to one.
!
      IF ( OLDSYM ) THEN

         LOCVAL = SUMAI  ( TABPTR(1), LOCSYM-1 ) + 1
         DIMVAL = TABPTR(LOCSYM)

         IF ( DIMVAL .GT. 1 ) THEN
            CALL REMLAD ( DIMVAL-1, LOCVAL, TABVAL(1), NVAL )
            CALL SCARDD ( NVAL, TABVAL )
         END IF

         TABPTR(LOCSYM) = 1
         TABVAL(LOCVAL) = VALUE

!
!     Otherwise, we can't proceed unless we know that we have enough
!     room for one extra addition in all three tables.
!
      ELSE IF ( NSYM .GE. SIZEC ( TABSYM ) ) THEN

            CALL SETMSG ( 'SYSETD: Addition of the new symbol # '     // &
                          'causes an overflow in the name table.' )
            CALL ERRCH  ( '#', NAME )
            CALL SIGERR ( 'SPICE(NAMETABLEFULL)' )


      ELSE IF ( NPTR .GE. SIZEI ( TABPTR ) ) THEN

            CALL SETMSG ( 'SYSETD: Addition of the new symbol # '     // &
                          'causes an overflow in the pointer table.' )
            CALL ERRCH  ( '#', NAME )
            CALL SIGERR ( 'SPICE(POINTERTABLEFULL)' )


      ELSE IF ( NVAL .GE. SIZED ( TABVAL ) ) THEN

            CALL SETMSG ( 'SYSETD: Addition of the new symbol # '     // &
                          'causes an overflow in the value table.' )
            CALL ERRCH  ( '#', NAME )
            CALL SIGERR ( 'SPICE(VALUETABLEFULL)' )


!
!     If there's room, add the new name to the name table. Give the
!     symbol dimension one, and put the value in the right place.
!
      ELSE

         CALL INSLAC ( NAME, 1, LOCSYM+1, TABSYM(1), NSYM )
         CALL SCARDC ( NSYM, TABSYM )

         CALL INSLAI ( 1, 1, LOCSYM+1, TABPTR(1), NPTR )
         CALL SCARDI ( NPTR, TABPTR )

         LOCVAL = SUMAI ( TABPTR(1), LOCSYM ) + 1

         CALL INSLAD ( VALUE, 1, LOCVAL, TABVAL(1), NVAL )
         CALL SCARDD ( NVAL, TABVAL )

      END IF


      CALL CHKOUT ( 'SYSETD' )
      RETURN
      END
