!$Procedure SYTRNI (Transpose two values associated with a symbol)

      SUBROUTINE SYTRNI ( NAME, IDX1, IDX2, TABSYM, TABPTR, TABVAL )

! $ Abstract
!
!     Transpose two values associated with a particular symbol in an
!     integer symbol table.
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
      INTEGER               IDX1
      INTEGER               IDX2
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      INTEGER               TABVAL     ( LBCELL:* )

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
!              transposed.
!
!     IDX2     is the index of the second associated value to be
!              transposed.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are components of the integer symbol table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are components of the integer symbol table.
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
!        books   -->   5
!        erasers -->   6
!        pencils -->  12
!                     18
!                     24
!        pens    -->  10
!                     20
!                     30
!                     40
!
!     The call,
!
!     CALL SYTRNI ( 'pens', 2, 3, TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        books   -->   5
!        erasers -->   6
!        pencils -->  12
!                     18
!                     24
!        pens    -->  10
!                     30
!                     20
!                     40
!     The next call,
!
!     CALL SYTRNI ( 'pencils', 2, 4, TABSYM, TABPTR, TABVAL )
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
!-    Beta Version 2.0.0, 16-JAN-1989 (HAN)
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
      CALL CHKIN ( 'SYTRNI' )

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

               CALL SWAPI ( TABVAL(LOCVAL+IDX1-1), &
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


      CALL CHKOUT ( 'SYTRNI' )
      RETURN
      END
