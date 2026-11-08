C$Procedure  TSTSCH ( Produce EK table schemas for EK testing )

      SUBROUTINE TSTSCH ( TABLE,  MXROWS, 
     .                    SEGTYP, NROWS,  NCOLS, CNAMES, CCLASS, 
     .                    DTYPES, STLENS, DIMS,  INDEXD, NULLOK, DECLS )
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
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektype.inc'
      
      
      CHARACTER*(*)         TABLE
      INTEGER               MXROWS
      INTEGER               SEGTYP
      INTEGER               NROWS
      INTEGER               NCOLS
      CHARACTER*(*)         CNAMES ( * )
      INTEGER               CCLASS ( * )
      INTEGER               DTYPES ( * )
      INTEGER               STLENS ( * )
      INTEGER               DIMS   ( * )
      LOGICAL               INDEXD ( * )
      LOGICAL               NULLOK ( * )
      CHARACTER*(*)         DECLS  ( * )
      
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TABLE      I   Name of table whose schema is requested.
C     MXROWS     I   Maximum number of rows allowed in any table.
C     SEGTYP     O   Segment type used to implement table.
C     NROWS      O   Number of rows in table.
C     NCOLS      O   Number of columns in table.
C     CNAMES     O   Column names.
C     CCLASS     O   Column classes.
C     DTYPES     O   Column data types.
C     STLENS     O   String lengths for character columns.
C     DIMS       O   Column entry sizes.
C     INDEXD     O   Flags indicating whether columns are indexed.
C     NULLOK     O   Flags indicating whether columns allow null values.
C     DECLS      O   Declaration strings for columns.
C
C$ Detailed_Input
C
C     TABLE          is the name of the table whose schema is to be
C                    returned.
C
C     MXROWS         is the maximum number of rows allowed in any table.
C                    MXROWS should be at least 4000 for robust testing,
C                    but it may be set as small as 10 for quick tests.
C$ Detailed_Output
C
C     SEGTYP         is an integer indicating the segment type to be
C                    used to implement the specified table.  Possible
C                    values are 1 or 2.
C
C     NROWS          is the number of rows in the specified table.
C                    This is not actually a property of the schema, but
C                    the number of rows is selected here for uniformity.
C
C     NCOLS          is the number of columns in the specified table.
C
C     CNAMES         is an array of names of the columns in the table.
C
C     CCLASS         is an array of integer column class codes for the 
C                    columns in the specified segment.  The class code
C                    of a column indicates the implementation of the
C                    data structure used to store the column's data.
C
C     DTYPES         is an array of integer data type codes for the 
C                    columns.  Values may be any of CHR, DP, INT, or
C                    TIME.  These parameters are declared in ektype.inc.
C
C     STLENS         is an array of string lengths for the columns.
C                    If the Ith column has fixed-length strings, the
C                    Ith element of STLENS gives that length.  Variable
C                    string length is indicated by the value IFALSE,
C                    which is defined in ekbool.inc.  For non-character
C                    columns, the corresponding element of STLENS is 0.
C                    
C     DIMS           is an array of element sizes for the columns.
C                    If the Ith column has fixed-size entries, the
C                    Ith element of DIMS gives that size.  Variable
C                    entry size is indicated by the value IFALSE,
C                    which is defined in ekbool.inc. 
C
C     INDEXD         is an array of logical flags indicating whether the
C                    corresponding columns are indexed.  
C
C     NULLOK         is an array of logical flags indicating whether the
C                    corresponding columns allow null values.  
C
C     DECLS          is an array of strings containing column 
C                    declarations as required by EKBSEG or EKIFLD.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input table name is not recognized, the error 
C        SPICE(NOSUCHTABLE) is signaled.
C
C$ Files
C
C     The table schemas created by this routine are intended to apply
C     to any test EK in a sequence of test EKs. 
C
C$ Particulars
C
C     This routine is meant to support the automatic, non-interactive
C     testing of the SPICELIB EK system.  TSTSCH creates table schemas
C     for a series of tables to be used in EK testing.  The tables are
C     intended to be populated with data created by TSTENT, which
C     produces a predictable set of test data. 
C
C     The table schemas created by this routine are described below.
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
C-    Testing Utilities Version 1.0.0, 15-JUL-1999 (NJB)
C
C-&



C
C     SPICELIB functions
C  
      LOGICAL               RETURN
      

C
C     Local variables
C
      INTEGER               I      

      
      
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'TSTSCH' )
      END IF

C
C     Check MXROWS.
C
      IF ( MXROWS .LT. 10 ) THEN
      
         CALL SETMSG ( 'Sorry, MXROWS was #; must be at least 10.' )
         CALL ERRINT ( '#', MXROWS                                 )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'TSTSCH'                                    )
         RETURN
          
      END IF
      
C
C     The table-specific assignments follow.  Declaration strings are
C     built at the end, since the logic is identical for each table.
C     
      IF ( TABLE .EQ. 'SCALAR_1' ) THEN
      
         SEGTYP = 1
         NROWS  = 10
         NCOLS  = 8
         
         CNAMES(1) = 'TABLE_NAME'
         CNAMES(2) = 'FILE_NO'
         CNAMES(3) = 'SEGMENT_NO'
         CNAMES(4) = 'ROW_NO'
         CNAMES(5) = 'C_COL_1'
         CNAMES(6) = 'D_COL_1'
         CNAMES(7) = 'I_COL_1'
         CNAMES(8) = 'T_COL_1'
         
         DTYPES(1) = CHR
         DTYPES(2) = INT
         DTYPES(3) = INT
         DTYPES(4) = INT
         DTYPES(5) = CHR
         DTYPES(6) = DP
         DTYPES(7) = INT
         DTYPES(8) = TIME
         
         CALL CLEARI ( NCOLS, STLENS )
         
         STLENS(1) = TNAMSZ
         STLENS(5) = IFALSE

                  
         DO I = 1, NCOLS
         
            DIMS(I)   =  1
            INDEXD(I) = .FALSE.
            NULLOK(I) = .FALSE.
            
         END DO

         CCLASS(1) = 3
         CCLASS(2) = 1
         CCLASS(3) = 1
         CCLASS(4) = 1
         CCLASS(5) = 3
         CCLASS(6) = 2
         CCLASS(7) = 1
         CCLASS(8) = 2
         


      ELSE IF ( TABLE .EQ. 'SCALAR_2' ) THEN
      
         
         SEGTYP = 1
         NROWS  = MXROWS
         NCOLS  = 19
         
         CNAMES(1)  = 'TABLE_NAME'
         CNAMES(2)  = 'FILE_NO'
         CNAMES(3)  = 'SEGMENT_NO'
         CNAMES(4)  = 'ROW_NO'
         CNAMES(5)  = 'C_COL_1'
         CNAMES(6)  = 'C_COL_2'
         CNAMES(7)  = 'C_COL_3'
         CNAMES(8)  = 'C_COL_4'
         CNAMES(9)  = 'C_COL_5'
         CNAMES(10) = 'C_COL_6'
         CNAMES(11) = 'D_COL_1'
         CNAMES(12) = 'D_COL_2'
         CNAMES(13) = 'D_COL_3'
         CNAMES(14) = 'I_COL_1'
         CNAMES(15) = 'I_COL_2'
         CNAMES(16) = 'I_COL_3'
         CNAMES(17) = 'T_COL_1'
         CNAMES(18) = 'T_COL_2'
         CNAMES(19) = 'T_COL_3'
         
         DTYPES(1)  = CHR
         DTYPES(2)  = INT
         DTYPES(3)  = INT
         DTYPES(4)  = INT
         DTYPES(5)  = CHR
         DTYPES(6)  = CHR
         DTYPES(7)  = CHR
         DTYPES(8)  = CHR
         DTYPES(9)  = CHR
         DTYPES(10) = CHR
         DTYPES(11) = DP
         DTYPES(12) = DP
         DTYPES(13) = DP
         DTYPES(14) = INT
         DTYPES(15) = INT
         DTYPES(16) = INT
         DTYPES(17) = TIME
         DTYPES(18) = TIME
         DTYPES(19) = TIME
         
         CALL CLEARI ( NCOLS, STLENS )
         
         STLENS(1)  = TNAMSZ
         STLENS(5)  = IFALSE
         STLENS(6)  = IFALSE
         STLENS(7)  = IFALSE
         STLENS(8)  = 20
         STLENS(9)  = 20
         STLENS(10) = 20
                  
         DO I = 1, NCOLS
         
            DIMS(I)   =  1
            INDEXD(I) = .TRUE.
            NULLOK(I) =  I .GT. 4
            
         END DO

         CCLASS(1)  = 3
         CCLASS(2)  = 1
         CCLASS(3)  = 1
         CCLASS(4)  = 1
         CCLASS(5)  = 3
         CCLASS(6)  = 3
         CCLASS(7)  = 3
         CCLASS(8)  = 3
         CCLASS(9)  = 3
         CCLASS(10) = 3
         CCLASS(11) = 2
         CCLASS(12) = 2
         CCLASS(13) = 2
         CCLASS(14) = 1
         CCLASS(15) = 1
         CCLASS(16) = 1
         CCLASS(17) = 2
         CCLASS(18) = 2
         CCLASS(19) = 2
            

      ELSE IF ( TABLE .EQ. 'SCALAR_3' ) THEN
      
         SEGTYP = 2
         NROWS  = 10
         NCOLS  = 8
         
         CNAMES(1) = 'TABLE_NAME'
         CNAMES(2) = 'FILE_NO'
         CNAMES(3) = 'SEGMENT_NO'
         CNAMES(4) = 'ROW_NO'
         CNAMES(5) = 'C_COL_1'
         CNAMES(6) = 'D_COL_1'
         CNAMES(7) = 'I_COL_1'
         CNAMES(8) = 'T_COL_1'
         
         DTYPES(1) = CHR
         DTYPES(2) = INT
         DTYPES(3) = INT
         DTYPES(4) = INT
         DTYPES(5) = CHR
         DTYPES(6) = DP
         DTYPES(7) = INT
         DTYPES(8) = TIME
      
         CALL CLEARI ( NCOLS, STLENS )
            
         STLENS(1) = TNAMSZ
         STLENS(5) = 20
   
                  
         DO I = 1, NCOLS
         
            DIMS(I)   =  1
            INDEXD(I) = .FALSE.
            NULLOK(I) = .FALSE.
            
         END DO

         CCLASS(1) = 9
         CCLASS(2) = 7
         CCLASS(3) = 7
         CCLASS(4) = 7
         CCLASS(5) = 9
         CCLASS(6) = 8
         CCLASS(7) = 7
         CCLASS(8) = 8


      ELSE IF ( TABLE .EQ. 'SCALAR_4' ) THEN

         SEGTYP = 2
         NROWS  = MXROWS
         NCOLS  = 16
         
         CNAMES(1)  = 'TABLE_NAME'
         CNAMES(2)  = 'FILE_NO'
         CNAMES(3)  = 'SEGMENT_NO'
         CNAMES(4)  = 'ROW_NO'
         CNAMES(5)  = 'C_COL_1'
         CNAMES(6)  = 'C_COL_2'
         CNAMES(7)  = 'C_COL_3'
         CNAMES(8)  = 'D_COL_1'
         CNAMES(9)  = 'D_COL_2'
         CNAMES(10) = 'D_COL_3'
         CNAMES(11) = 'I_COL_1'
         CNAMES(12) = 'I_COL_2'
         CNAMES(13) = 'I_COL_3'
         CNAMES(14) = 'T_COL_1'
         CNAMES(15) = 'T_COL_2'
         CNAMES(16) = 'T_COL_3'
         
         DTYPES(1)  = CHR
         DTYPES(2)  = INT
         DTYPES(3)  = INT
         DTYPES(4)  = INT
         DTYPES(5)  = CHR
         DTYPES(6)  = CHR
         DTYPES(7)  = CHR
         DTYPES(8)  = DP
         DTYPES(9)  = DP
         DTYPES(10) = DP
         DTYPES(11) = INT
         DTYPES(12) = INT
         DTYPES(13) = INT
         DTYPES(14) = TIME
         DTYPES(15) = TIME
         DTYPES(16) = TIME
         
         CALL CLEARI ( NCOLS, STLENS )
         
         STLENS(1)  = TNAMSZ
         STLENS(5)  = 20
         STLENS(6)  = 20
         STLENS(7)  = 20
                  
         DO I = 1, NCOLS
         
            DIMS(I)   =  1
            INDEXD(I) = .TRUE.
            NULLOK(I) =  I .GT. 4
            
         END DO

         CCLASS(1)  = 9
         CCLASS(2)  = 7
         CCLASS(3)  = 7
         CCLASS(4)  = 7
         CCLASS(5)  = 9
         CCLASS(6)  = 9
         CCLASS(7)  = 9
         CCLASS(8)  = 8
         CCLASS(9)  = 8
         CCLASS(10) = 8
         CCLASS(11) = 7
         CCLASS(12) = 7
         CCLASS(13) = 7
         CCLASS(14) = 8
         CCLASS(15) = 8
         CCLASS(16) = 8


      ELSE IF ( TABLE .EQ. 'VECTOR_1' ) THEN

         SEGTYP = 1
         NROWS  = 3
         NCOLS  = 12
         
         CNAMES(1)  = 'TABLE_NAME'
         CNAMES(2)  = 'FILE_NO'
         CNAMES(3)  = 'SEGMENT_NO'
         CNAMES(4)  = 'ROW_NO'
         CNAMES(5)  = 'C_COL_1'
         CNAMES(6)  = 'C_COL_2'
         CNAMES(7)  = 'D_COL_1'
         CNAMES(8)  = 'D_COL_2'
         CNAMES(9)  = 'I_COL_1'
         CNAMES(10) = 'I_COL_2'
         CNAMES(11) = 'T_COL_1'
         CNAMES(12) = 'T_COL_2'
         
         DTYPES(1)  = CHR
         DTYPES(2)  = INT
         DTYPES(3)  = INT
         DTYPES(4)  = INT
         DTYPES(5)  = CHR
         DTYPES(6)  = CHR
         DTYPES(7)  = DP
         DTYPES(8)  = DP
         DTYPES(9)  = INT
         DTYPES(10) = INT
         DTYPES(11) = TIME
         DTYPES(12) = TIME
         
         CALL CLEARI ( NCOLS, STLENS )
         
         STLENS(1) = TNAMSZ
         STLENS(5) = 1024
         STLENS(6) = 100
                  
         DIMS(1)  = 1
         DIMS(2)  = 1
         DIMS(3)  = 1
         DIMS(4)  = 1
         DIMS(5)  = 3
         DIMS(6)  = IFALSE
         DIMS(7)  = 4
         DIMS(8)  = IFALSE
         DIMS(9)  = 5
         DIMS(10) = IFALSE
         DIMS(11) = 6
         DIMS(12) = IFALSE


         DO I = 1, NCOLS
         
            INDEXD(I) = .FALSE.
            NULLOK(I) = .FALSE.
            
         END DO

         CCLASS(1)  = 3
         CCLASS(2)  = 1
         CCLASS(3)  = 1
         CCLASS(4)  = 1
         CCLASS(5)  = 6
         CCLASS(6)  = 6
         CCLASS(7)  = 5
         CCLASS(8)  = 5
         CCLASS(9)  = 4
         CCLASS(10) = 4
         CCLASS(11) = 5
         CCLASS(12) = 5

      
      ELSE IF ( TABLE .EQ. 'VECTOR_2' ) THEN

         SEGTYP = 1
         NROWS  = 3
         NCOLS  = 20
         
         CNAMES(1)  = 'TABLE_NAME'
         CNAMES(2)  = 'FILE_NO'
         CNAMES(3)  = 'SEGMENT_NO'
         CNAMES(4)  = 'ROW_NO'
         CNAMES(5)  = 'C_COL_1'
         CNAMES(6)  = 'C_COL_2'
         CNAMES(7)  = 'C_COL_3'
         CNAMES(8)  = 'C_COL_4'
         CNAMES(9)  = 'D_COL_1'
         CNAMES(10) = 'D_COL_2'
         CNAMES(11) = 'D_COL_3'
         CNAMES(12) = 'D_COL_4'
         CNAMES(13) = 'I_COL_1'
         CNAMES(14) = 'I_COL_2'
         CNAMES(15) = 'I_COL_3'
         CNAMES(16) = 'I_COL_4'
         CNAMES(17) = 'T_COL_1'
         CNAMES(18) = 'T_COL_2'
         CNAMES(19) = 'T_COL_3'
         CNAMES(20) = 'T_COL_4'
         
         DTYPES(1)  = CHR
         DTYPES(2)  = INT
         DTYPES(3)  = INT
         DTYPES(4)  = INT
         DTYPES(5)  = CHR
         DTYPES(6)  = CHR
         DTYPES(7)  = CHR
         DTYPES(8)  = CHR
         DTYPES(9)  = DP
         DTYPES(10) = DP
         DTYPES(11) = DP
         DTYPES(12) = DP
         DTYPES(13) = INT
         DTYPES(14) = INT
         DTYPES(15) = INT
         DTYPES(16) = INT
         DTYPES(17) = TIME
         DTYPES(18) = TIME
         DTYPES(19) = TIME
         DTYPES(20) = TIME
         
         CALL CLEARI ( NCOLS, STLENS )
         
         STLENS(1) = TNAMSZ
         STLENS(5) = 1024
         STLENS(6) = 1024
         STLENS(7) = 1024
         STLENS(8) = 1024
                  
         DIMS(1)  = 1
         DIMS(2)  = 1
         DIMS(3)  = 1
         DIMS(4)  = 1
         DIMS(5)  = 3
         DIMS(6)  = 5
         DIMS(7)  = IFALSE
         DIMS(8)  = IFALSE
         DIMS(9)  = 4
         DIMS(10) = 6
         DIMS(11) = IFALSE
         DIMS(12) = IFALSE
         DIMS(13) = 5
         DIMS(14) = 7
         DIMS(15) = IFALSE
         DIMS(16) = IFALSE
         DIMS(17) = 6
         DIMS(18) = 8
         DIMS(19) = IFALSE
         DIMS(20) = IFALSE


         DO I = 1, NCOLS
         
            INDEXD(I) =  I .LE. 4
            NULLOK(I) =  I .GT. 4
            
         END DO


         CCLASS(1)  = 3
         CCLASS(2)  = 1
         CCLASS(3)  = 1
         CCLASS(4)  = 1
         CCLASS(5)  = 6
         CCLASS(6)  = 6
         CCLASS(7)  = 6
         CCLASS(8)  = 6
         CCLASS(9)  = 5
         CCLASS(10) = 5
         CCLASS(11) = 5
         CCLASS(12) = 5
         CCLASS(13) = 4
         CCLASS(14) = 4
         CCLASS(15) = 4
         CCLASS(16) = 4
         CCLASS(17) = 5
         CCLASS(18) = 5
         CCLASS(19) = 5
         CCLASS(20) = 5

     
      ELSE
            
         CALL SETMSG ( 'Table # does not exist.' )
         CALL ERRCH  ( '#',  TABLE               )
         CALL SIGERR ( 'SPICE(NOSUCHTABLE)'      )
         CALL CHKOUT ( 'TSTSCH'                  )
         RETURN
         
      END IF


C
C     Build the declaration strings for the columns. 
C
      DO I = 1, NCOLS
C
C        Fill in the data type assigment.  For string-valued columns,
C        this includes a string length.
C
         DECLS(I) =  'DATATYPE = '
         
         IF ( DTYPES(I) .EQ. CHR ) THEN
            
            CALL SUFFIX ( 'CHARACTER*(#)', 1, DECLS(I) )
            
            IF ( STLENS(I) .GT. 0 ) THEN
            
               CALL REPMI ( DECLS(I), '#', STLENS(I), DECLS(I) )
            ELSE
               CALL REPMC ( DECLS(I), '#', '*',       DECLS(I) )
            END IF
            
            
         ELSE IF ( DTYPES(I) .EQ. DP ) THEN
         
            CALL SUFFIX ( 'DOUBLE PRECISION', 1, DECLS(I) )
         
         ELSE IF ( DTYPES(I) .EQ. INT ) THEN
         
            CALL SUFFIX ( 'INTEGER', 1, DECLS(I) )
            
         ELSE 
         
            CALL SUFFIX ( 'TIME', 1, DECLS(I) )
      
         END IF
         
C
C        If the dimension is not 1, add a dimension 
C        specifier.
C
         IF ( DIMS(I) .GT. 1 ) THEN
         
            CALL SUFFIX ( ', SIZE = #', 0, DECLS(I) )
            
            CALL REPMI ( DECLS(I), '#', DIMS(I), DECLS(I) )
            
         ELSE IF ( DIMS(I) .EQ. IFALSE ) THEN
C
C           This column has variable dimension.
C  
            CALL SUFFIX ( ', SIZE = #', 0, DECLS(I) )
            
            CALL REPMC ( DECLS(I), '#', 'VARIABLE', DECLS(I) )
      
         END IF
      
C
C        If the column is indexed, add an index specifier.
C
         IF ( INDEXD(I) ) THEN
            CALL SUFFIX ( ', INDEXED = TRUE', 0, DECLS(I) )
         END IF
         
         
C
C        If the column may contain nulls, add a null value specifier.
C
         IF ( NULLOK(I) ) THEN
            CALL SUFFIX ( ', NULLS_OK = TRUE', 0, DECLS(I) )
         END IF
         
C
C        If the segment has type 2, the column must be a fixed count
C        column.  Add the fixed count specifier in this case.
C        
         IF ( SEGTYP .EQ. 2 ) THEN
            CALL SUFFIX ( ', FIXED_COUNT = TRUE', 0, DECLS(I) )
         END IF
            
      END DO
      

      CALL CHKOUT ( 'TSTSCH' )
      RETURN
      END
