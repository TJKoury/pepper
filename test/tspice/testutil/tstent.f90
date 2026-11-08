!$Procedure  TSTENT ( Produce EK column entries for EK testing )

      SUBROUTINE TSTENT ( FILENO, TABLE, SEGNO, COLUMN, ROWNO,  NMAX, &
                          NELTS,  CVALS, DVALS, IVALS,  TVALS,  ISNULL )
      IMPLICIT NONE

! $ Abstract
!
!     Make up EK column entries for use in EK testing.
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
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ektype.inc'


      INTEGER               MAXESZ
      PARAMETER           ( MAXESZ = 10 )

      INTEGER               FILENO
      CHARACTER*(*)         TABLE
      INTEGER               SEGNO
      CHARACTER*(*)         COLUMN
      INTEGER               ROWNO
      INTEGER               NMAX
      INTEGER               NELTS
      CHARACTER*(*)         CVALS ( * )
      DOUBLE PRECISION      DVALS ( * )
      INTEGER               IVALS ( * )
      DOUBLE PRECISION      TVALS ( * )
      LOGICAL               ISNULL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     MAXESZ     P   Maximum number of elements in the column entry.
!     FILENO     I   Index of EK test file in test file sequence.
!     TABLE      I   EK table name.
!     SEGNO      I   Segment index within table in current file.
!     COLUMN     I   Column name.
!     ROWNO      I   Row index within segment.
!     NMAX       I   Maximum number of column entry elements to return.
!     NELTS      O   Number of elements in specified column entry.
!     CVALS      O   Values making up character column entry.
!     DVALS      O   Values making up d.p. column entry.
!     IVALS      O   Values making up integer column entry.
!     TVALS      O   Values making up time column entry.
!     ISNULL     O   Flag indicating whether entry is null.
!
! $ Detailed_Input
!
!     FILENO         is the index of in the EK test file sequence of
!                    the EK containing the specified column entry.
!                    The entries are a function of which file they're
!                    from.
!
!     TABLE          is the name of the EK table containing the entry.
!                    This routine creates a fixed set of tables, each
!                    having a name given by this routine.  See the
!                    Particulars section for the table names and
!                    descriptions.
!
!     SEGNO          is the index within the specified table, within
!                    the specified file, of the segment containing the
!                    entry.
!
!     COLUMN         is the name of the column containing the entry.
!                    Within each table, this routine creates a fixed
!                    set of columns, each having a name given by this
!                    routine.  See the Particulars section for the
!                    table names and descriptions.
!
!     ROWNO          is the index within the specified segment of the
!                    row containing the entry.
!
!     NMAX           is the maximum number of column entry elements to
!                    return.  This input is used for error checking.
!
! $ Detailed_Output
!
!     NELTS          is the number of elements in the specified column
!                    entry.
!
!     CVALS          are the character values making up the column
!                    entry, if the specified column has character type.
!                    Otherwise, CVALS is undefined.
!
!     DVALS          are the d.p values  making up the column entry, if
!                    the specified column has double precision type.
!                    Otherwise, DVALS is undefined.
!
!     IVALS          are the integer values  making up the column
!                    entry, if the specified column has integer type.
!                    Otherwise, IVALS is undefined.
!
!     TVALS          are the ET values  making up the column entry, if
!                    the specified column has TIME type. Otherwise,
!                    TVALS is undefined.
!
!     ISNULL         is a logical flag indicating whether the returned
!                    column entry is null.
!
! $ Parameters
!
!     MAXESZ         Maximum number of elements in the column entry.
!
! $ Exceptions
!
!     1) If the input table name is not recognized, the error
!        SPICE(NOSUCHTABLE) is signaled.
!
!     2) If the input column name is not recognized, the error
!        SPICE(NOSUCHCOLUMN) is signaled.
!
! $ Files
!
!     The column entries created by this routine are intended to
!     belong to a particular test EK in a sequence of test EKs.  The
!     index of the EK within the sequence is given by the input argument
!     FILENO.  The index is also reflected in the returned column
!     values.
!
! $ Particulars
!
!     This routine is meant to support the automatic, non-interactive
!     testing of the SPICELIB EK system.  TSTENT creates a predictable
!     set of test data with which to populate a set of binary EK files,
!     and against which data fetched from those files can be compared.
!
!     The test data produced by this routine are reasonably
!     comprehensive in some respects:
!
!        - Both segment types (1 and 2) are represented.
!        - Every column class is represented.
!        - Indexed and non-indexed columns are represented.
!        - Null and non-null column entries for each column class
!          are created.
!        - Tables are continued across multiple files.
!
!     On the other hand, the tables created by this routine are not
!     adequate to exhaustively test all of the logic used to write and
!     read EK files.
!
!     The tables created by this routine are described below.
!
!
!        1) Table SCALAR_1:
!
!              Segment type:  1
!
!              Columns          Data type         Indexed?   Nulls ok?
!              -------          ---------         --------   ---------
!              TABLE_NAME       CHARACTER*(64)    No         No
!              FILE_NO          INTEGER           No         No
!              SEGMENT_NO       INTEGER           No         No
!              ROW_NO           INTEGER           No         No
!              C_COL_1          CHARACTER*(*)     No         No
!              D_COL_1          DOUBLE PRECISION  No         No
!              I_COL_1          INTEGER           No         No
!              T_COL_1          TIME              No         No
!
!        2) Table SCALAR_2:
!
!              Segment type:  1
!
!              Columns          Data type         Indexed?   Nulls ok?
!              -------          ---------         --------   ---------
!              TABLE_NAME       CHARACTER*(64)    Yes        No
!              FILE_NO          INTEGER           Yes        No
!              SEGMENT_NO       INTEGER           Yes        No
!              ROW_NO           INTEGER           Yes        No
!              C_COL_1          CHARACTER*(*)     Yes        Yes
!              C_COL_2          CHARACTER*(*)     Yes        Yes
!              C_COL_3          CHARACTER*(*)     Yes        Yes
!              C_COL_4          CHARACTER*(20)    Yes        Yes
!              C_COL_5          CHARACTER*(20)    Yes        Yes
!              C_COL_6          CHARACTER*(20)    Yes        Yes
!              D_COL_1          DOUBLE PRECISION  Yes        Yes
!              D_COL_2          DOUBLE PRECISION  Yes        Yes
!              D_COL_3          DOUBLE PRECISION  Yes        Yes
!              I_COL_1          INTEGER           Yes        Yes
!              I_COL_2          INTEGER           Yes        Yes
!              I_COL_3          INTEGER           Yes        Yes
!              T_COL_1          TIME              Yes        Yes
!              T_COL_2          TIME              Yes        Yes
!              T_COL_3          TIME              Yes        Yes
!
!
!        3) Table SCALAR_3:
!
!              Segment type:  2
!
!              Columns          Data type         Indexed?   Nulls ok?
!              -------          ---------         --------   ---------
!              TABLE_NAME       CHARACTER*(64)    No         No
!              FILE_NO          INTEGER           No         No
!              SEGMENT_NO       INTEGER           No         No
!              ROW_NO           INTEGER           No         No
!              C_COL_1          CHARACTER*(20)    No         No
!              D_COL_1          DOUBLE PRECISION  No         No
!              I_COL_1          INTEGER           No         No
!              T_COL_1          TIME              No         No
!
!
!        4) Table SCALAR_4:
!
!              Segment type:  2
!
!              Columns          Data type         Indexed?   Nulls ok?
!              -------          ---------         --------   ---------
!              TABLE_NAME       CHARACTER*(64)    Yes        No
!              FILE_NO          INTEGER           Yes        No
!              SEGMENT_NO       INTEGER           Yes        No
!              ROW_NO           INTEGER           Yes        No
!              C_COL_1          CHARACTER*(20)    Yes        Yes
!              C_COL_2          CHARACTER*(20)    Yes        Yes
!              C_COL_3          CHARACTER*(20)    Yes        Yes
!              D_COL_1          DOUBLE PRECISION  Yes        Yes
!              D_COL_2          DOUBLE PRECISION  Yes        Yes
!              D_COL_3          DOUBLE PRECISION  Yes        Yes
!              I_COL_1          INTEGER           Yes        Yes
!              I_COL_2          INTEGER           Yes        Yes
!              I_COL_3          INTEGER           Yes        Yes
!              T_COL_1          TIME              Yes        Yes
!              T_COL_2          TIME              Yes        Yes
!              T_COL_3          TIME              Yes        Yes
!
!        5) Table VECTOR_1:
!
!              Segment type:  1
!
!              Columns       Data type         Indexed? Nulls ok? Dim.
!              -------       ---------         -------- --------- ----
!              TABLE_NAME    CHARACTER*(64)    Yes      No        1
!              FILE_NO       INTEGER           Yes      No        1
!              SEGMENT_NO    INTEGER           Yes      No        1
!              ROW_NO        INTEGER           Yes      No        1
!              C_COL_1       CHARACTER*(1024)  No       No        3
!              C_COL_2       CHARACTER*(100)   No       No        *
!              D_COL_1       DOUBLE PRECISION  No       No        4
!              D_COL_2       DOUBLE PRECISION  No       No        *
!              I_COL_1       INTEGER           No       No        5
!              I_COL_2       INTEGER           No       No        *
!              T_COL_1       TIME              No       No        6
!              T_COL_2       TIME              No       No        *
!
!
!        6) Table VECTOR_2:
!
!              Segment type:  1
!
!              Columns       Data type         Indexed? Nulls ok? Dim.
!              -------       ---------         -------- --------- ----
!              TABLE_NAME    CHARACTER*(64)    Yes      No        1
!              FILE_NO       INTEGER           Yes      No        1
!              SEGMENT_NO    INTEGER           Yes      No        1
!              ROW_NO        INTEGER           Yes      No        1
!              C_COL_1       CHARACTER*(1024)  No       Yes       3
!              C_COL_2       CHARACTER*(1024)  No       Yes       5
!              C_COL_3       CHARACTER*(1024)  No       Yes       *
!              C_COL_4       CHARACTER*(1024)  No       Yes       *
!              D_COL_1       DOUBLE PRECISION  No       Yes       4
!              D_COL_2       DOUBLE PRECISION  No       Yes       6
!              D_COL_3       DOUBLE PRECISION  No       Yes       *
!              D_COL_4       DOUBLE PRECISION  No       Yes       *
!              I_COL_1       INTEGER           No       Yes       5
!              I_COL_2       INTEGER           No       Yes       7
!              I_COL_3       INTEGER           No       Yes       *
!              I_COL_4       INTEGER           No       Yes       *
!              T_COL_1       TIME              No       Yes       6
!              T_COL_2       TIME              No       Yes       8
!              T_COL_3       TIME              No       Yes       *
!              T_COL_4       TIME              No       Yes       *
!
! $ Examples
!
!     None.
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
!
! $ Version
!
!-    Testing Utilities Version 1.1.0, 23-DEC-2009 (EDW)
!
!        Save declaration altered to save all variables.
!
!-    Testing Utilities Version 1.0.0, 26-JUL-1999 (NJB)
!
!-&



!
!     SPICELIB functions
!
      INTEGER               ISRCHC
      INTEGER               RTRIM

      LOGICAL               ODD
      LOGICAL               RETURN


!
!     Local parameters
!
      INTEGER               DECLEN
      PARAMETER           ( DECLEN = 200 )

!
!     Local variables
!
      CHARACTER*(CNAMSZ)    CNAMES ( MXCLSG )
      CHARACTER*(DECLEN)    DECLS  ( MXCLSG )
      CHARACTER*(MAXSTR)    PAD

      INTEGER               COLIDX
      INTEGER               CCLASS ( MXCLSG )
      INTEGER               DIMS   ( MXCLSG )
      INTEGER               DTYPES ( MXCLSG )
      INTEGER               I
      INTEGER               BASVAL
      INTEGER               J
      INTEGER               NCOLS
      INTEGER               NROWS
      INTEGER               R
      INTEGER               SEGTYP
      INTEGER               STLENS ( MXCLSG )

      LOGICAL               FIRST
      LOGICAL               INDEXD ( MXCLSG )
      LOGICAL               NULLOK ( MXCLSG )

!
!     Save all.
!
      SAVE

!
!     Initial values
!
      DATA                  FIRST / .TRUE.  /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'TSTENT' )
      END IF


      IF ( FIRST ) THEN
!
!        Initialize the pad string for long character data values.
!
         DO I = 1, MAXSTR
            PAD(I:I) = 'X'
         END DO

         PAD ( 96       : 100    ) = ' 100>'
         PAD ( MAXSTR-6 : MAXSTR ) = ' 1024>'

         FIRST = .FALSE.

      END IF


!
!     Look up the schema for the indicated table.  This gives us
!     dimension information.  The value of "MXROWS" is irrelevant;
!     set it to the minimum allowed value.
!
      CALL TSTSCH ( TABLE, &
                    10,     SEGTYP, NROWS,  NCOLS,  CNAMES, CCLASS, &
                    DTYPES, STLENS, DIMS,   INDEXD, NULLOK, DECLS  )

!
!     Get the index of the requested column within the CNAMES array.
!
      COLIDX = ISRCHC ( COLUMN, NCOLS, CNAMES )

      IF ( COLIDX .EQ. 0 ) THEN

         CALL SETMSG ( 'Column # does not exist in table #.' )
         CALL ERRCH  ( '#',  COLUMN                          )
         CALL ERRCH  ( '#',  TABLE                           )
         CALL SIGERR ( 'SPICE(NOSUCHCOLUMN)'                 )
         CALL CHKOUT ( 'TSTENT'                              )
         RETURN

      END IF


!
!     Factor out the boiler-plate responses.  Set values for
!     the TABLE_NAME, FILE_NO, SEGMENT_NO and ROW_NO columns right here.
!
      IF (      ( TABLE .EQ. 'SCALAR_1' ) &
           .OR. ( TABLE .EQ. 'SCALAR_2' ) &
           .OR. ( TABLE .EQ. 'SCALAR_3' ) &
           .OR. ( TABLE .EQ. 'SCALAR_4' ) &
           .OR. ( TABLE .EQ. 'VECTOR_1' ) &
           .OR. ( TABLE .EQ. 'VECTOR_2' ) &
           .OR. ( TABLE .EQ. 'EMPTY_1'  ) &
           .OR. ( TABLE .EQ. 'EMPTY_2'  )  ) THEN

         IF ( COLUMN .EQ. 'TABLE_NAME' ) THEN

            NELTS    = 1
            CVALS(1) = TABLE
            ISNULL   = .FALSE.

            CALL CHKOUT ( 'TSTENT' )
            RETURN

         ELSE IF ( COLUMN .EQ. 'FILE_NO' ) THEN

            NELTS    = 1
            IVALS(1) = FILENO
            ISNULL   = .FALSE.

            CALL CHKOUT ( 'TSTENT' )
            RETURN

         ELSE IF ( COLUMN .EQ. 'SEGMENT_NO' ) THEN

            NELTS    = 1
            IVALS(1) = SEGNO
            ISNULL   = .FALSE.

            CALL CHKOUT ( 'TSTENT' )
            RETURN

         ELSE IF ( COLUMN .EQ. 'ROW_NO' ) THEN

            NELTS    = 1
            IVALS(1) = ROWNO
            ISNULL   = .FALSE.

            CALL CHKOUT ( 'TSTENT' )
            RETURN

         END IF

      END IF







      IF ( TABLE .EQ. 'SCALAR_1' ) THEN

         BASVAL  =  1000000*SEGNO + ROWNO


         IF ( COLUMN .EQ. 'C_COL_1' ) THEN

            NELTS    = 1
            CVALS(1) = 'SEG_#_#_ROW_#'
            CALL REPMI( CVALS(1), '#', SEGNO,  CVALS(1) )
            CALL REPMC( CVALS(1), '#', COLUMN, CVALS(1) )
            CALL REPMI( CVALS(1), '#', ROWNO,  CVALS(1) )
            ISNULL   = .FALSE.

         ELSE IF ( COLUMN .EQ. 'D_COL_1' ) THEN

            NELTS    = 1
            DVALS(1) = DBLE( BASVAL )
            ISNULL   = .FALSE.

         ELSE IF ( COLUMN .EQ. 'I_COL_1' ) THEN

            NELTS    = 1
            IVALS(1) = BASVAL
            ISNULL   = .FALSE.

         ELSE IF ( COLUMN .EQ. 'T_COL_1' ) THEN

            NELTS    = 1
            TVALS(1) = DBLE( BASVAL )
            ISNULL   = .FALSE.

         ELSE

            CALL SETMSG ( 'Column # does not exist in table SCALAR_1.' )
            CALL ERRCH  ( '#',  COLUMN                                 )
            CALL SIGERR ( 'SPICE(NOSUCHCOLUMN)'                        )
            CALL CHKOUT ( 'TSTENT'                                     )
            RETURN

         END IF




      ELSE IF ( TABLE .EQ. 'SCALAR_2' ) THEN

         BASVAL  =  1000000*SEGNO + ROWNO


         IF ( COLUMN .EQ. 'C_COL_1' ) THEN

            NELTS = 1

            CVALS(1) = 'SEG_#_#_ROW_#'
            CALL REPMI( CVALS(1), '#', SEGNO,  CVALS(1) )
            CALL REPMC( CVALS(1), '#', COLUMN, CVALS(1) )
            CALL REPMI( CVALS(1), '#', ROWNO,  CVALS(1) )

            ISNULL = .FALSE.


         ELSE IF ( COLUMN .EQ. 'C_COL_2' ) THEN

            NELTS = 1

            CVALS(1) = 'SEG_#_#_ROW_#_'
            CALL REPMI( CVALS(1), '#', SEGNO,  CVALS(1) )
            CALL REPMC( CVALS(1), '#', COLUMN, CVALS(1) )
            CALL REPMI( CVALS(1), '#', ROWNO,  CVALS(1) )

            J = MOD ( ROWNO, 100 )

            DO I = 1, J
               CALL SUFFIX ( 'X', 0, CVALS(1) )
            END DO

!
!           Create two rows that are identical up through the first
!           32 characters.  This tests some of the logic in ZZEKJSRT.
!
            IF (  ( ROWNO .EQ. 16 ) .OR. ( ROWNO .EQ. 18 )  ) THEN

               DO I = 1, 32
                  CALL PREFIX ( 'X', 0, CVALS(1) )
               END DO

            END IF

            ISNULL   = ODD(ROWNO)


         ELSE IF ( COLUMN .EQ. 'C_COL_3' ) THEN


            NELTS    = 1
            ISNULL   = .TRUE.


         ELSE IF ( COLUMN .EQ. 'C_COL_4' ) THEN

            NELTS = 1

            CVALS(1) = 'SEG_#_#_ROW_#_'
            CALL REPMI( CVALS(1), '#', SEGNO,  CVALS(1) )
            CALL REPMC( CVALS(1), '#', COLUMN, CVALS(1) )
            CALL REPMI( CVALS(1), '#', ROWNO,  CVALS(1) )

            J = MOD ( ROWNO, 100 )

            DO I = 1, J
               CALL SUFFIX ( 'X', 0, CVALS(1) )
            END DO

            ISNULL   = ODD(ROWNO)


         ELSE IF ( COLUMN .EQ. 'C_COL_5' ) THEN

            NELTS    = 1
            CVALS(1) = 'SEG_#_#_ROW_#_'
            CALL REPMI( CVALS(1), '#', SEGNO,  CVALS(1) )
            CALL REPMC( CVALS(1), '#', COLUMN, CVALS(1) )
            CALL REPMI( CVALS(1), '#', ROWNO,  CVALS(1) )

            ISNULL   = ODD(ROWNO)


         ELSE IF ( COLUMN .EQ. 'C_COL_6' ) THEN

            NELTS    = 1
            ISNULL   = .TRUE.


         ELSE IF ( COLUMN .EQ. 'D_COL_1' ) THEN

            NELTS    = 1
            DVALS(1) = DBLE( -BASVAL )
            ISNULL   = .FALSE.


         ELSE IF ( COLUMN .EQ. 'D_COL_2' ) THEN

            NELTS    = 1
            DVALS(1) = DBLE( -BASVAL )
            ISNULL   = ODD( ROWNO )


         ELSE IF ( COLUMN .EQ. 'D_COL_3' ) THEN

            NELTS    = 1
            ISNULL   = .TRUE.


         ELSE IF ( COLUMN .EQ. 'I_COL_1' ) THEN

            NELTS    = 1
            IVALS(1) = BASVAL
            ISNULL   = .FALSE.

         ELSE IF ( COLUMN .EQ. 'I_COL_2' ) THEN

            NELTS    = 1
            IVALS(1) = -BASVAL
            ISNULL   = ODD( ROWNO )

         ELSE IF ( COLUMN .EQ. 'I_COL_3' ) THEN

            NELTS    = 1
            ISNULL   = .TRUE.

         ELSE IF ( COLUMN .EQ. 'T_COL_1' ) THEN

            NELTS    = 1
            TVALS(1) = DBLE( -BASVAL )
            ISNULL   = .FALSE.


         ELSE IF ( COLUMN .EQ. 'T_COL_2' ) THEN

            NELTS    = 1
            TVALS(1) = DBLE( -BASVAL )
            ISNULL   = ODD( ROWNO )


         ELSE IF ( COLUMN .EQ. 'T_COL_3' ) THEN

            NELTS    = 1
            ISNULL   = .TRUE.


         ELSE

            CALL SETMSG ( 'Column # does not exist in table SCALAR_2.' )
            CALL ERRCH  ( '#',  COLUMN                                 )
            CALL SIGERR ( 'SPICE(NOSUCHCOLUMN)'                        )
            CALL CHKOUT ( 'TSTENT'                                     )
            RETURN

         END IF





      ELSE IF ( TABLE .EQ. 'SCALAR_3' ) THEN

         BASVAL  =  1000000*SEGNO + ROWNO


         IF ( COLUMN .EQ. 'C_COL_1' ) THEN

            NELTS    = 1

            CVALS(1) = 'SEG_#_#_ROW_#_'
            CALL REPMI( CVALS(1), '#', SEGNO,  CVALS(1) )
            CALL REPMC( CVALS(1), '#', COLUMN, CVALS(1) )
            CALL REPMI( CVALS(1), '#', ROWNO,  CVALS(1) )

            ISNULL   = .FALSE.

         ELSE IF ( COLUMN .EQ. 'D_COL_1' ) THEN

            NELTS    = 1
            DVALS(1) = DBLE( BASVAL )
            ISNULL   = .FALSE.

         ELSE IF ( COLUMN .EQ. 'I_COL_1' ) THEN

            NELTS    = 1
            IVALS(1) = BASVAL
            ISNULL   = .FALSE.

         ELSE IF ( COLUMN .EQ. 'T_COL_1' ) THEN

            NELTS    = 1
            TVALS(1) = DBLE( BASVAL )
            ISNULL   = .FALSE.

         ELSE

            CALL SETMSG ( 'Column # does not exist in table SCALAR_3.' )
            CALL ERRCH  ( '#',  COLUMN                                 )
            CALL SIGERR ( 'SPICE(NOSUCHCOLUMN)'                        )
            CALL CHKOUT ( 'TSTENT'                                     )
            RETURN

         END IF



      ELSE IF ( TABLE .EQ. 'SCALAR_4' ) THEN

         BASVAL  =  1000000*SEGNO + ROWNO


         IF ( COLUMN .EQ. 'C_COL_1' ) THEN

            NELTS    = 1

            CVALS(1) = 'SEG_#_#_ROW_#_'

            CALL REPMI( CVALS(1), '#', SEGNO,  CVALS(1) )
            CALL REPMC( CVALS(1), '#', COLUMN, CVALS(1) )
            CALL REPMI( CVALS(1), '#', ROWNO,  CVALS(1) )

            ISNULL   = .FALSE.


         ELSE IF ( COLUMN .EQ. 'C_COL_2' ) THEN

            NELTS    = 1

            CVALS(1) = 'SEG_#_#_ROW_#_'
            CALL REPMI( CVALS(1), '#', SEGNO,  CVALS(1) )
            CALL REPMC( CVALS(1), '#', COLUMN, CVALS(1) )
            CALL REPMI( CVALS(1), '#', ROWNO,  CVALS(1) )

            ISNULL   = ODD( ROWNO )


         ELSE IF ( COLUMN .EQ. 'C_COL_3' ) THEN

            NELTS    = 1
            ISNULL   = .TRUE.


         ELSE IF ( COLUMN .EQ. 'D_COL_1' ) THEN

            NELTS    = 1
            DVALS(1) = DBLE( BASVAL )
            ISNULL   = .FALSE.


         ELSE IF ( COLUMN .EQ. 'D_COL_2' ) THEN

            NELTS    = 1
            DVALS(1) = DBLE( -BASVAL )
            ISNULL   = .FALSE.


         ELSE IF ( COLUMN .EQ. 'D_COL_3' ) THEN

            NELTS    = 1
            ISNULL   = .TRUE.


         ELSE IF ( COLUMN .EQ. 'I_COL_1' ) THEN

            NELTS    = 1
            IVALS(1) = BASVAL
            ISNULL   = .FALSE.

         ELSE IF ( COLUMN .EQ. 'I_COL_2' ) THEN

            NELTS    = 1
            IVALS(1) = -BASVAL
            ISNULL   = ODD ( ROWNO )

         ELSE IF ( COLUMN .EQ. 'I_COL_3' ) THEN

            NELTS    = 1
            ISNULL   = .TRUE.

         ELSE IF ( COLUMN .EQ. 'T_COL_1' ) THEN

            NELTS    = 1
            TVALS(1) = DBLE( BASVAL )
            ISNULL   = .FALSE.


         ELSE IF ( COLUMN .EQ. 'T_COL_2' ) THEN

            NELTS    = 1
            TVALS(1) = DBLE( -BASVAL )
            ISNULL   = .FALSE.


         ELSE IF ( COLUMN .EQ. 'T_COL_3' ) THEN

            NELTS    = 1
            ISNULL   = .TRUE.


         ELSE

            CALL SETMSG ( 'Column # does not exist in table SCALAR_4.' )
            CALL ERRCH  ( '#',  COLUMN                                 )
            CALL SIGERR ( 'SPICE(NOSUCHCOLUMN)'                        )
            CALL CHKOUT ( 'TSTENT'                                     )
            RETURN

         END IF





      ELSE IF ( TABLE .EQ. 'VECTOR_1' ) THEN

         BASVAL  =  1000000*SEGNO + 100*ROWNO


         IF ( COLUMN .EQ. 'C_COL_1' ) THEN

            NELTS = 3

            DO I = 1, NELTS

               CVALS(I) = 'SEG_#_#_ROW_#_ELT_#_'
               CALL REPMI( CVALS(I), '#', SEGNO,  CVALS(I) )
               CALL REPMC( CVALS(I), '#', COLUMN, CVALS(I) )
               CALL REPMI( CVALS(I), '#', ROWNO,  CVALS(I) )
               CALL REPMI( CVALS(I), '#', I,      CVALS(I) )

               R = RTRIM( CVALS(I) )

               CVALS(I) ( R+3: ) = PAD( R+3: )

            END DO

            ISNULL   = .FALSE.


         ELSE IF ( COLUMN .EQ. 'C_COL_2' ) THEN

            NELTS = 1 + MOD ( ROWNO, 10 )

            DO I = 1, NELTS

               CVALS(I) = 'SEG_#_#_ROW_#_ELT_#_'
               CALL REPMI( CVALS(I), '#', SEGNO,  CVALS(I) )
               CALL REPMC( CVALS(I), '#', COLUMN, CVALS(I) )
               CALL REPMI( CVALS(I), '#', ROWNO,  CVALS(I) )
               CALL REPMI( CVALS(I), '#', I,      CVALS(I) )

               R = RTRIM( CVALS(I) )

               CVALS(I) ( R+3: ) = PAD( R+3: )

            END DO

            ISNULL   = .FALSE.


         ELSE IF ( COLUMN .EQ. 'D_COL_1' ) THEN

            NELTS = 4

            DO I = 1, NELTS

               DVALS(I) = BASVAL + I

            END DO

            ISNULL   = .FALSE.


         ELSE IF ( COLUMN .EQ. 'D_COL_2' ) THEN

            NELTS = 1 + MOD ( ROWNO, 11 )

            DO I = 1, NELTS

               DVALS(I) = BASVAL + I

            END DO

            ISNULL   = .FALSE.


         ELSE IF ( COLUMN .EQ. 'I_COL_1' ) THEN

            NELTS = 5

            DO I = 1, NELTS

               IVALS(I) = BASVAL + I

            END DO

            ISNULL   = .FALSE.


         ELSE IF ( COLUMN .EQ. 'I_COL_2' ) THEN

            NELTS = 1 + MOD ( ROWNO, 12 )

            DO I = 1, NELTS

               IVALS(I) = BASVAL + I

            END DO

            ISNULL   = .FALSE.


         ELSE IF ( COLUMN .EQ. 'T_COL_1' ) THEN

            NELTS = 6

            DO I = 1, NELTS

               TVALS(I) = BASVAL + I

            END DO

            ISNULL   = .FALSE.


         ELSE IF ( COLUMN .EQ. 'T_COL_2' ) THEN

            NELTS = 1 + MOD ( ROWNO, 11 )

            DO I = 1, NELTS

               TVALS(I) = BASVAL + I

            END DO

            ISNULL   = .FALSE.


         ELSE

            CALL SETMSG ( 'Column # does not exist in table VECTOR_1.' )
            CALL ERRCH  ( '#',  COLUMN                                 )
            CALL SIGERR ( 'SPICE(NOSUCHCOLUMN)'                        )
            CALL CHKOUT ( 'TSTENT'                                     )
            RETURN

         END IF



      ELSE IF ( TABLE .EQ. 'VECTOR_2' ) THEN


         BASVAL  =  1000000*SEGNO + 100*ROWNO


         IF ( COLUMN .EQ. 'C_COL_1' ) THEN

            NELTS = 3

            DO I = 1, NELTS

               CVALS(I) = 'SEG_#_#_ROW_#_ELT_#_'
               CALL REPMI( CVALS(I), '#', SEGNO,  CVALS(I) )
               CALL REPMC( CVALS(I), '#', COLUMN, CVALS(I) )
               CALL REPMI( CVALS(I), '#', ROWNO,  CVALS(I) )
               CALL REPMI( CVALS(I), '#', I,      CVALS(I) )

               R = RTRIM( CVALS(I) )

               CVALS(I) ( R+3: ) = PAD( R+3: )

            END DO

            ISNULL   = ODD ( ROWNO )


         ELSE IF ( COLUMN .EQ. 'C_COL_2' ) THEN

            NELTS  =  5
            ISNULL =  .TRUE.


         ELSE IF ( COLUMN .EQ. 'C_COL_3' ) THEN


            ISNULL = ODD ( ROWNO )

            IF ( ISNULL ) THEN
               NELTS = 1
            ELSE
               NELTS = 1 + MOD ( ROWNO, 10 )
            END IF

            DO I = 1, NELTS

               CVALS(I) = 'SEG_#_#_ROW_#_ELT_#_'
               CALL REPMI( CVALS(I), '#', SEGNO,  CVALS(I) )
               CALL REPMC( CVALS(I), '#', COLUMN, CVALS(I) )
               CALL REPMI( CVALS(I), '#', ROWNO,  CVALS(I) )
               CALL REPMI( CVALS(I), '#', I,      CVALS(I) )

               R = RTRIM( CVALS(I) )

               CVALS(I) ( R+3: ) = PAD( R+3: )

            END DO



         ELSE IF ( COLUMN .EQ. 'C_COL_4' ) THEN

            NELTS  =  1
            ISNULL =  .TRUE.




         ELSE IF ( COLUMN .EQ. 'D_COL_1' ) THEN

            NELTS = 4

            DO I = 1, NELTS

               DVALS(I) = BASVAL + I

            END DO

            ISNULL   = ODD ( ROWNO )


         ELSE IF ( COLUMN .EQ. 'D_COL_2' ) THEN

            NELTS    = 6
            ISNULL   = .TRUE.


         ELSE IF ( COLUMN .EQ. 'D_COL_3' ) THEN

            ISNULL = ODD ( ROWNO )

            IF ( ISNULL ) THEN
               NELTS = 1
            ELSE
               NELTS = 1 + MOD ( ROWNO, 11 )
            END IF


            DO I = 1, NELTS

               DVALS(I) = BASVAL + I

            END DO


         ELSE IF ( COLUMN .EQ. 'D_COL_4' ) THEN

            NELTS    = 1
            ISNULL   = .TRUE.




         ELSE IF ( COLUMN .EQ. 'I_COL_1' ) THEN

            NELTS = 5

            DO I = 1, NELTS

               IVALS(I) = BASVAL + I

            END DO

            ISNULL   = ODD ( ROWNO )


         ELSE IF ( COLUMN .EQ. 'I_COL_2' ) THEN

            NELTS    = 7
            ISNULL   = .TRUE.


         ELSE IF ( COLUMN .EQ. 'I_COL_3' ) THEN

            ISNULL = ODD ( ROWNO )

            IF ( ISNULL ) THEN
               NELTS = 1
            ELSE
               NELTS = 1 + MOD ( ROWNO, 12 )
            END IF

            DO I = 1, NELTS

               IVALS(I) = BASVAL + I

            END DO


         ELSE IF ( COLUMN .EQ. 'I_COL_4' ) THEN

            NELTS    = 1
            ISNULL   = .TRUE.



         ELSE IF ( COLUMN .EQ. 'T_COL_1' ) THEN

            NELTS = 6

            DO I = 1, NELTS

               TVALS(I) = BASVAL + I

            END DO

            ISNULL   = ODD ( ROWNO )


         ELSE IF ( COLUMN .EQ. 'T_COL_2' ) THEN

            NELTS    = 8
            ISNULL   = .TRUE.


         ELSE IF ( COLUMN .EQ. 'T_COL_3' ) THEN

            ISNULL = ODD ( ROWNO )

            IF ( ISNULL ) THEN
               NELTS = 1
            ELSE
               NELTS = 1 + MOD ( ROWNO, 11 )
            END IF


            DO I = 1, NELTS

               TVALS(I) = BASVAL + I

            END DO


         ELSE IF ( COLUMN .EQ. 'T_COL_4' ) THEN

            NELTS    = 1
            ISNULL   = .TRUE.


         ELSE

            CALL SETMSG ( 'Column # does not exist in table VECTOR_2.' )
            CALL ERRCH  ( '#',  COLUMN                                 )
            CALL SIGERR ( 'SPICE(NOSUCHCOLUMN)'                        )
            CALL CHKOUT ( 'TSTENT'                                     )
            RETURN

         END IF


      ELSE

         CALL SETMSG ( 'Table # does not exist.' )
         CALL ERRCH  ( '#',  TABLE               )
         CALL SIGERR ( 'SPICE(NOSUCHTABLE)'      )
         CALL CHKOUT ( 'TSTENT'                  )
         RETURN

      END IF


!
!     Make some final adjustments:  for fixed-length character columns,
!     truncate the non-blank portion of the column entries at the
!     declared column string length.
!
      IF ( DTYPES(COLIDX) .EQ. CHR ) THEN

         R = STLENS(COLIDX)

         IF ( R .EQ. IFALSE ) THEN

            IF ( LEN(CVALS(1)) .GT. 100 ) THEN

               DO I = 1, NELTS
                  CVALS(I) (101:) = ' '
               END DO

            END IF

         ELSE

            IF ( LEN(CVALS(1)) .GT. R ) THEN

               DO I = 1, NELTS
                  CVALS(I) (R+1:) = ' '
               END DO

            END IF

         END IF

      END IF

      CALL CHKOUT ( 'TSTENT' )
      RETURN
      END
