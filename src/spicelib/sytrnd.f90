!$Procedure SYTRND (Transpose two values associated with a symbol)

      SUBROUTINE SYTRND ( NAME, IDX1, IDX2, TABSYM, TABPTR, TABVAL )

! $ Abstract
!
!     Transpose two values associated with a particular symbol in a
!     double precision symbol table.
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
      INTEGER               IDX1
      INTEGER               IDX2
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      DOUBLE PRECISION      TABVAL     ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol whose associated values are to
!                    be transposed.
!     IDX1       I   Index of first associated value to be transposed.
!     IDX2       I   Index of second associated value to be transposed.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose associated values are
!              to be transposed. If NAME is not in the symbol table,
!              the symbol tables are not modified.
!
!     IDX1     is the index of the first associated value to be
!              transposed. If this index is not valid (IDX1 < 1 or
!              IDX1 > the dimension of NAME) the symbol table is not
!              modified.
!
!     IDX2     is the index of the second associated value to be
!              transposed. If this index is not valid (IDX2 < 1 or
!              IDX2 > the dimension of NAME) the symbol table is not
!              modified.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are components of the double precision symbol table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are components of the double precision symbol table.
!
!              If the symbol NAME is not in the symbol table the symbol
!              tables are not modified. Otherwise, the values that IDX1
!              and IDX2 refer to are transposed in the value table.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If IDX1 < 1, IDX2 < 1, IDX1 > the dimension of NAME, or
!         IDX2 > the dimension of NAME, the error SPICE(INVALIDINDEX)
!         is signaled.
!
!     2)  If NAME is not in the symbol table, the symbol tables are not
!         modified.
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
!        DELTA_T_A     -->    3.2184D1
!        K             -->    1.657D-3
!        MEAN_ANOM     -->    6.239996D0
!                             1.99096871D-7
!        ORBIT_ECC     -->    1.671D-2
!        BODY4_POLE_RA -->    3.17681D2
!                             1.08D-1
!                             0.0D0
!
!     The call,
!
!     CALL SYTRND ( 'BODY4_POLE_RA', 2, 3, TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        DELTA_T_A     -->    3.2184D1
!        K             -->    1.657D-3
!        MEAN_ANOM     -->    6.239996D0
!                             1.99096871D-7
!        ORBIT_ECC     -->    1.671D-2
!        BODY4_POLE_RA -->    3.17681D2
!                             0.0D0C
!                             1.08D-1
!     The next call,
!
!     CALL SYTRND ( 'MEAN_ANOM', 2, 4, TABSYM, TABPTR, TABVAL )
!
!     causes the error SPICE(INVALIDINDEX) to be signaled.
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
!-    SPICELIB Version 1.2.0, 08-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Changed the name of input arguments "I" and "J" to "IDX1" and
!        "IDX2" for consistency with other routines.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
!
!        Updated so no "exchange" occurs if IDX1 equals IDX2.
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
!     transpose two values associated with a symbol
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
!
!        Updated so no "exchange" occurs if IDX1 equals IDX2.
!
!-     Beta Version 2.0.0, 16-JAN-1989 (HAN)
!
!         If one of the indices of the values to be transposed is
!         invalid, an error is signaled and the symbol table is
!         not modified.
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
      END IF
      CALL CHKIN ( 'SYTRND' )


!
!     How many symbols?
!
      NSYM = CARDC ( TABSYM )

!
!     Is this symbol even in the table?
!
      LOCSYM = BSRCHC ( NAME, NSYM, TABSYM(1) )

      IF ( LOCSYM .GT. 0 ) THEN

!
!        Are there enough values associated with the symbol?
!
         N = TABPTR(LOCSYM)

!
!        Are the indices valid?
!

         IF (        IDX1 .GE. 1 &
              .AND.  IDX1 .LE. N &
              .AND.  IDX2 .GE. 1 &
              .AND.  IDX2 .LE. N ) THEN

!
!           Exchange the values in place.
!
            IF ( IDX1 .NE. IDX2 ) THEN

               LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1

               CALL SWAPD ( TABVAL(LOCVAL+IDX1-1), &
                            TABVAL(LOCVAL+IDX2-1) )

            END IF

         ELSE

            CALL SETMSG ( 'The first index was *. ' &
            //            'The second index was *.' )
            CALL ERRINT ( '*', IDX1                 )
            CALL ERRINT ( '*', IDX2                 )
            CALL SIGERR ( 'SPICE(INVALIDINDEX)'     )

         END IF

      END IF


      CALL CHKOUT ( 'SYTRND' )
      RETURN
      END
