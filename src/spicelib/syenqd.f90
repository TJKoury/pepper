!$Procedure SYENQD ( Enqueue a value onto a symbol )

      SUBROUTINE SYENQD ( NAME, VALUE, TABSYM, TABPTR, TABVAL )

! $ Abstract
!
!     Enqueue a value onto a particular symbol in a double precision
!     symbol table. If the symbol is not in the table, a new one
!     is created.
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
!     NAME       I   Name of the symbol onto which the value is
!                    enqueued.
!     VALUE      I   Value to be enqueued.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol onto which the value is to
!              be enqueued. If NAME is not in the symbol table, a new
!              symbol having the value VALUE is created.
!
!     VALUE    is the value to be enqueued onto the symbol, NAME.
!              The value is inserted in the value table after the
!              last value associated with the symbol.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a double precision symbol table.
!              The symbol NAME may or may not be in the symbol
!              table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a double precision symbol table.
!
!              On output, the value table contains the new value in
!              addition to the old values associated with the symbol
!              NAME. The pointer table is updated to reflect the change
!              in the dimension of the symbol.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the addition of the new value to the symbol table causes an
!         overflow in the value table, the error SPICE(VALUETABLEFULL)
!         is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     If the symbol NAME is not in the symbol table, a new symbol is
!     created which has the value VALUE.
!
! $ Examples
!
!     The contents of the symbol table are:
!
!         DELTA_T_A -->   32.184
!         K         -->    1.657D-3
!         MEAN_ANOM -->    6.239996D0
!                          1.99096871D-7
!         ORBIT_ECC -->    1.671D-2
!
!      The call,
!
!         CALL SYENQD ( 'BODY399_POLE_RA', 0.0D0,
!        .              TABSYM, TABPTR, TABVAL    )
!
!      produces the symbol table:
!
!         BODY399_POLE_RA -->    0.0D0
!         DELTA_T_A       -->   32.184
!         K               -->    1.657D-3
!         MEAN_ANOM       -->    6.239996D0
!                                1.99096871D-7
!         ORBIT_ECC       -->    1.671D-2
!
!      Notice that the new symbol "BODY399_POLE_RA" has been created and
!      has the value 0.0D0 associated with it.
!
!      The next call,
!
!         CALL SYENQD ( 'BODY399_POLE_RA', -6.4061614D-1,
!        .               TABSYM, TABPTR, TABVAL           )
!
!         CALL SYENQD ( 'BODY399_POLE_RA', -8.386D-5,
!        .               TABSYM, TABPTR, TABVAL           )
!
!      then produces the symbol table:
!
!         BODY399_POLE_RA -->    0.0D0
!                               -6.4061614D-1
!                               -8.386D-5
!         DELTA_T_A       -->   32.184
!         K               -->    1.657D-3
!         MEAN_ANOM       -->    6.239996D0
!                                1.99096871D-7
!         ORBIT_ECC       -->    1.671D-2
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
!     enqueue a value onto a symbol
!
!-&


!
!     SPICELIB functions
!

      INTEGER               CARDC
      INTEGER               CARDD
      INTEGER               LSTLEC
      LOGICAL               RETURN
      INTEGER               SIZED
      INTEGER               SUMAI

!
!     Local variables
!
      INTEGER               NSYM
      INTEGER               NVAL

      INTEGER               LOCSYM
      INTEGER               LOCVAL
      LOGICAL               OLDSYM





!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYENQD' )
      END IF

!
!     How many symbols to start with?
!
      NSYM = CARDC ( TABSYM )
      NVAL = CARDD ( TABVAL )

!
!     Where does this symbol belong? Is it already in the table?
!
      LOCSYM = LSTLEC ( NAME, NSYM, TABSYM(1) )
      OLDSYM = ( LOCSYM .NE. 0  .AND.  TABSYM(LOCSYM) .EQ. NAME )

!
!     If it's not already in the table, use SET to create a brand new
!     symbol.
!
      IF ( .NOT. OLDSYM ) THEN

         CALL SYSETD ( NAME, VALUE, TABSYM, TABPTR, TABVAL )

!
!     If it is in the table, we can't proceed unless we know that we
!     have enough room for one extra addition in the value table.
!
      ELSE IF (  NVAL .GE. SIZED ( TABVAL ) ) THEN

         CALL SETMSG ( 'SYENQD: The addition of the value $ to the '  // &
                       'symbol # causes an overflow in the value '    // &
                       'table.' )
         CALL ERRDP  ( '$', VALUE )
         CALL ERRCH  ( '#', NAME  )
         CALL SIGERR ( 'SPICE(VALUETABLEFULL)' )

!
!     If there's room, add the new value to the value table. Add one
!     to the dimension, and put the value in the right place.
!
      ELSE

         LOCVAL = SUMAI ( TABPTR(1), LOCSYM ) + 1

         CALL INSLAD ( VALUE, 1, LOCVAL, TABVAL(1), NVAL )
         CALL SCARDD ( NVAL, TABVAL )

         TABPTR(LOCSYM) = TABPTR(LOCSYM) + 1

      END IF


      CALL CHKOUT ( 'SYENQD' )
      RETURN
      END
