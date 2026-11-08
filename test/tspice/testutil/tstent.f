C$Procedure  TSTENT ( Produce EK column entries for EK testing )

      SUBROUTINE TSTENT ( FILENO, TABLE, SEGNO, COLUMN, ROWNO,  NMAX,
     .                    NELTS,  CVALS, DVALS, IVALS,  TVALS,  ISNULL )
      IMPLICIT NONE
      
C$ Abstract
C
C     Make up EK column entries for use in EK testing.  
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
C     None.
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations

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
      
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     MAXESZ     P   Maximum number of elements in the column entry.
C     FILENO     I   Index of EK test file in test file sequence.
C     TABLE      I   EK table name.
C     SEGNO      I   Segment index within table in current file.
C     COLUMN     I   Column name.
C     ROWNO      I   Row index within segment.
C     NMAX       I   Maximum number of column entry elements to return.
C     NELTS      O   Number of elements in specified column entry.
C     CVALS      O   Values making up character column entry.
C     DVALS      O   Values making up d.p. column entry.
C     IVALS      O   Values making up integer column entry.
C     TVALS      O   Values making up time column entry.
C     ISNULL     O   Flag indicating whether entry is null.
C  
C$ Detailed_Input
C
C     FILENO         is the index of in the EK test file sequence of
C                    the EK containing the specified column entry.
C                    The entries are a function of which file they're
C                    from.
C 
C     TABLE          is the name of the EK table containing the entry.
C                    This routine creates a fixed set of tables, each
C                    having a name given by this routine.  See the
C                    Particulars section for the table names and
C                    descriptions.
C
C     SEGNO          is the index within the specified table, within
C                    the specified file, of the segment containing the
C                    entry.
C
C     COLUMN         is the name of the column containing the entry.
C                    Within each table, this routine creates a fixed
C                    set of columns, each having a name given by this
C                    routine.  See the Particulars section for the
C                    table names and descriptions.
C
C     ROWNO          is the index within the specified segment of the
C                    row containing the entry.
C    
C     NMAX           is the maximum number of column entry elements to 
C                    return.  This input is used for error checking.
C
C$ Detailed_Output
C
C     NELTS          is the number of elements in the specified column 
C                    entry.
C
C     CVALS          are the character values making up the column 
C                    entry, if the specified column has character type.
C                    Otherwise, CVALS is undefined.
C
C     DVALS          are the d.p values  making up the column entry, if
C                    the specified column has double precision type.
C                    Otherwise, DVALS is undefined.
C
C     IVALS          are the integer values  making up the column
C                    entry, if the specified column has integer type.
C                    Otherwise, IVALS is undefined.
C
C     TVALS          are the ET values  making up the column entry, if
C                    the specified column has TIME type. Otherwise,
C                    TVALS is undefined.
C
C     ISNULL         is a logical flag indicating whether the returned
C                    column entry is null.
C
C$ Parameters
C
C     MAXESZ         Maximum number of elements in the column entry.
C
C$ Exceptions
C
C     1) If the input table name is not recognized, the error 
C        SPICE(NOSUCHTABLE) is signaled.
C
C     2) If the input column name is not recognized, the error 
C        SPICE(NOSUCHCOLUMN) is signaled.
C
C$ Files
C
C     The column entries created by this routine are intended to 
C     belong to a particular test EK in a sequence of test EKs.  The
C     index of the EK within the sequence is given by the input argument
C     FILENO.  The index is also reflected in the returned column
C     values.
C
C$ Particulars
C
C     This routine is meant to support the automatic, non-interactive
C     testing of the SPICELIB EK system.  TSTENT creates a predictable
C     set of test data with which to populate a set of binary EK files,
C     and against which data fetched from those files can be compared.
C     
C     The test data produced by this routine are reasonably 
C     comprehensive in some respects:
C
C        - Both segment types (1 and 2) are represented.
C        - Every column class is represented.
C        - Indexed and non-indexed columns are represented.
C        - Null and non-null column entries for each column class
C          are created.
C        - Tables are continued across multiple files. 
C
C     On the other hand, the tables created by this routine are not
C     adequate to exhaustively test all of the logic used to write and
C     read EK files. 
C
C     The tables created by this routine are described below.
C
C
C        1) Table SCALAR_1:
C
C              Segment type:  1
C              
C              Columns          Data type         Indexed?   Nulls ok?
C              -------          ---------         --------   ---------
C              TABLE_NAME       CHARACTER*(64)    No         No
C              FILE_NO          INTEGER           No         No
C              SEGMENT_NO       INTEGER           No         No
C              ROW_NO           INTEGER           No         No
C              C_COL_1          CHARACTER*(*)     No         No
C              D_COL_1          DOUBLE PRECISION  No         No
C              I_COL_1          INTEGER           No         No
C              T_COL_1          TIME              No         No
C
C        2) Table SCALAR_2:
C
C              Segment type:  1
C              
C              Columns          Data type         Indexed?   Nulls ok?
C              -------          ---------         --------   ---------
C              TABLE_NAME       CHARACTER*(64)    Yes        No
C              FILE_NO          INTEGER           Yes        No
C              SEGMENT_NO       INTEGER           Yes        No
C              ROW_NO           INTEGER           Yes        No
C              C_COL_1          CHARACTER*(*)     Yes        Yes
C              C_COL_2          CHARACTER*(*)     Yes        Yes
C              C_COL_3          CHARACTER*(*)     Yes        Yes
C              C_COL_4          CHARACTER*(20)    Yes        Yes
C              C_COL_5          CHARACTER*(20)    Yes        Yes
C              C_COL_6          CHARACTER*(20)    Yes        Yes
C              D_COL_1          DOUBLE PRECISION  Yes        Yes
C              D_COL_2          DOUBLE PRECISION  Yes        Yes
C              D_COL_3          DOUBLE PRECISION  Yes        Yes
C              I_COL_1          INTEGER           Yes        Yes
C              I_COL_2          INTEGER           Yes        Yes
C              I_COL_3          INTEGER           Yes        Yes
C              T_COL_1          TIME              Yes        Yes
C              T_COL_2          TIME              Yes        Yes
C              T_COL_3          TIME              Yes        Yes
C
C
C        3) Table SCALAR_3:
C
C              Segment type:  2
C              
C              Columns          Data type         Indexed?   Nulls ok?
C              -------          ---------         --------   ---------
C              TABLE_NAME       CHARACTER*(64)    No         No
C              FILE_NO          INTEGER           No         No
C              SEGMENT_NO       INTEGER           No         No
C              ROW_NO           INTEGER           No         No
C              C_COL_1          CHARACTER*(20)    No         No
C              D_COL_1          DOUBLE PRECISION  No         No
C              I_COL_1          INTEGER           No         No
C              T_COL_1          TIME              No         No
C
C
C        4) Table SCALAR_4:
C
C              Segment type:  2
C              
C              Columns          Data type         Indexed?   Nulls ok?
C              -------          ---------         --------   ---------
C              TABLE_NAME       CHARACTER*(64)    Yes        No
C              FILE_NO          INTEGER           Yes        No
C              SEGMENT_NO       INTEGER           Yes        No
C              ROW_NO           INTEGER           Yes        No
C              C_COL_1          CHARACTER*(20)    Yes        Yes
C              C_COL_2          CHARACTER*(20)    Yes        Yes
C              C_COL_3          CHARACTER*(20)    Yes        Yes
C              D_COL_1          DOUBLE PRECISION  Yes        Yes
C              D_COL_2          DOUBLE PRECISION  Yes        Yes
C              D_COL_3          DOUBLE PRECISION  Yes        Yes
C              I_COL_1          INTEGER           Yes        Yes
C              I_COL_2          INTEGER           Yes        Yes
C              I_COL_3          INTEGER           Yes        Yes
C              T_COL_1          TIME              Yes        Yes
C              T_COL_2          TIME              Yes        Yes
C              T_COL_3          TIME              Yes        Yes
C
C        5) Table VECTOR_1:
C
C              Segment type:  1
C              
C              Columns       Data type         Indexed? Nulls ok? Dim.
C              -------       ---------         -------- --------- ----
C              TABLE_NAME    CHARACTER*(64)    Yes      No        1
C              FILE_NO       INTEGER           Yes      No        1
C              SEGMENT_NO    INTEGER           Yes      No        1
C              ROW_NO        INTEGER           Yes      No        1
C              C_COL_1       CHARACTER*(1024)  No       No        3
C              C_COL_2       CHARACTER*(100)   No       No        *
C              D_COL_1       DOUBLE PRECISION  No       No        4
C              D_COL_2       DOUBLE PRECISION  No       No        *
C              I_COL_1       INTEGER           No       No        5
C              I_COL_2       INTEGER           No       No        *
C              T_COL_1       TIME              No       No        6
C              T_COL_2       TIME              No       No        *
C
C
C        6) Table VECTOR_2:
C
C              Segment type:  1
C              
C              Columns       Data type         Indexed? Nulls ok? Dim.
C              -------       ---------         -------- --------- ----
C              TABLE_NAME    CHARACTER*(64)    Yes      No        1
C              FILE_NO       INTEGER           Yes      No        1
C              SEGMENT_NO    INTEGER           Yes      No        1
C              ROW_NO        INTEGER           Yes      No        1
C              C_COL_1       CHARACTER*(1024)  No       Yes       3
C              C_COL_2       CHARACTER*(1024)  No       Yes       5
C              C_COL_3       CHARACTER*(1024)  No       Yes       *
C              C_COL_4       CHARACTER*(1024)  No       Yes       *
C              D_COL_1       DOUBLE PRECISION  No       Yes       4
C              D_COL_2       DOUBLE PRECISION  No       Yes       6
C              D_COL_3       DOUBLE PRECISION  No       Yes       *
C              D_COL_4       DOUBLE PRECISION  No       Yes       *
C              I_COL_1       INTEGER           No       Yes       5
C              I_COL_2       INTEGER           No       Yes       7
C              I_COL_3       INTEGER           No       Yes       *
C              I_COL_4       INTEGER           No       Yes       *
C              T_COL_1       TIME              No       Yes       6
C              T_COL_2       TIME              No       Yes       8
C              T_COL_3       TIME              No       Yes       *
C              T_COL_4       TIME              No       Yes       *
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.1.0, 23-DEC-2009 (EDW)
C
C        Save declaration altered to save all variables.
C
C-    Testing Utilities Version 1.0.0, 26-JUL-1999 (NJB)
C
C-&



C
C     SPICELIB functions
C  
      INTEGER               ISRCHC
      INTEGER               RTRIM
      
      LOGICAL               ODD
      LOGICAL               RETURN
      

C
C     Local parameters
C
      INTEGER               DECLEN
      PARAMETER           ( DECLEN = 200 )
      
C
C     Local variables
C
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

C
C     Save all.
C
      SAVE
      
C
C     Initial values
C
      DATA                  FIRST / .TRUE.  /
      
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'TSTENT' )
      END IF


      IF ( FIRST ) THEN
C
C        Initialize the pad string for long character data values.
C
         DO I = 1, MAXSTR
            PAD(I:I) = 'X'
         END DO

         PAD ( 96       : 100    ) = ' 100>'
         PAD ( MAXSTR-6 : MAXSTR ) = ' 1024>'
         
         FIRST = .FALSE.
         
      END IF


C
C     Look up the schema for the indicated table.  This gives us
C     dimension information.  The value of "MXROWS" is irrelevant;
C     set it to the minimum allowed value.
C      
      CALL TSTSCH ( TABLE,   
     .              10,     SEGTYP, NROWS,  NCOLS,  CNAMES, CCLASS,
     .              DTYPES, STLENS, DIMS,   INDEXD, NULLOK, DECLS  )

C
C     Get the index of the requested column within the CNAMES array.
C
      COLIDX = ISRCHC ( COLUMN, NCOLS, CNAMES )
      
      IF ( COLIDX .EQ. 0 ) THEN
      
         CALL SETMSG ( 'Column # does not exist in table #.' )
         CALL ERRCH  ( '#',  COLUMN                          )
         CALL ERRCH  ( '#',  TABLE                           )
         CALL SIGERR ( 'SPICE(NOSUCHCOLUMN)'                 )
         CALL CHKOUT ( 'TSTENT'                              )
         RETURN
      
      END IF
      
      
C
C     Factor out the boiler-plate responses.  Set values for
C     the TABLE_NAME, FILE_NO, SEGMENT_NO and ROW_NO columns right here.
C
      IF (      ( TABLE .EQ. 'SCALAR_1' )  
     .     .OR. ( TABLE .EQ. 'SCALAR_2' )
     .     .OR. ( TABLE .EQ. 'SCALAR_3' )
     .     .OR. ( TABLE .EQ. 'SCALAR_4' )
     .     .OR. ( TABLE .EQ. 'VECTOR_1' )
     .     .OR. ( TABLE .EQ. 'VECTOR_2' )
     .     .OR. ( TABLE .EQ. 'EMPTY_1'  )
     .     .OR. ( TABLE .EQ. 'EMPTY_2'  )  ) THEN

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

C
C           Create two rows that are identical up through the first
C           32 characters.  This tests some of the logic in ZZEKJSRT.   
C  
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


C
C     Make some final adjustments:  for fixed-length character columns,
C     truncate the non-blank portion of the column entries at the
C     declared column string length.
C
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
