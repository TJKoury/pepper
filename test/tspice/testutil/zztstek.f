C$Procedure ZZTSTEK ( Private --- produce an EK for testing )
 
      SUBROUTINE ZZTSTEK ( FILE, FILENO, MXROWS )
      IMPLICIT NONE
 
C$ Abstract
C
C     Create EK files for testing the EK writing and reading routines.
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
 
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektype.inc'
 
 
      CHARACTER*(*)         FILE
      INTEGER               FILENO
      INTEGER               MXROWS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FILE       I   The name of an SPK file to create.
C     FILENO     I   Index of file in sequence of test EKs.
C     MXROWS     I   Maximum number of rows allowed in any table.
C     LOAD       I   Logical indicating if file should be loaded.
C     HANDLE     O   Handle if file is loaded by TSTEK.
C
C$ Detailed_Input
C
C     FILE           is the name of an EK file to create for use in
C                    software testing.
C
C                    If the file specified already exists, the existing
C                    file is deleted and a new one created with the same
C                    name in its place.
C
C     FILENO         is the ordinal position of the named EK in a
C                    sequence of test EKs.  FILENO will be reflected in
C                    the EK data, so data may be traced to their source
C                    EK files.
C
C     MXROWS         is the maximum number of rows allowed in any table.
C                    MXROWS should be at least 4000 for robust testing,
C                    but it may be set as small as 10 for quick tests.
C

C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the specified file already exists, it is deleted and
C        replaced by the file created by this routine.
C
C     2) All other exceptions are diagnosed by routines in the call tree
C        of this routine.
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
C     testing of the SPICELIB EK system.  TSTEK uses TSTENT to create a
C     predictable set of test data with which it populates a set of
C     binary EK files.  Data fetched from those files can be compared
C     with outputs obtained directly from TSTENT.
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
C              C_COL_4          CHARACTER*(12)    Yes        Yes
C              C_COL_5          CHARACTER*(12)    Yes        Yes
C              C_COL_6          CHARACTER*(12)    Yes        Yes
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
C              C_COL_1          CHARACTER*(12)    No         No
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
C              C_COL_1          CHARACTER*(12)    Yes        Yes
C              C_COL_2          CHARACTER*(12)    Yes        Yes
C              C_COL_3          CHARACTER*(12)    Yes        Yes
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
C     B.V. Semenov       (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-   TESTUTIL Version 1.1.0, 14-OCT-2021 (BVS)
C
C        Bug fix: fixed routine name in CHKIN/CHKOUT calls
C        (TSTEK -> ZZTSTEK).
C
C-   TESTUTIL Version 1.0.0,  16-FEB-2010 (EDW)
C
C       Reimplementation of TSTEK, removing arguments and code
C       to load created file and register with the test subsystem.
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               EXISTS
 
C
C     Local parameters
C
      INTEGER               DECLEN
      PARAMETER           ( DECLEN = 200 )
 
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
 
      INTEGER               NTABS
      PARAMETER           ( NTABS  = 6 )
 
      INTEGER               MAXROW
      PARAMETER           ( MAXROW = 5000 )
 
      INTEGER               MAXENT
      PARAMETER           ( MAXENT = 20 )
 
      INTEGER               MAXFAT
      PARAMETER           ( MAXFAT = 100 )
 
      INTEGER               MAXFVL
      PARAMETER           ( MAXFVL = MAXFAT * MAXENT )
 
      INTEGER               CVALSZ
      PARAMETER           ( CVALSZ = 100  )
 
      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL = MAXROW * MAXENT / 2 )
 
C
C     Local variables
C
      CHARACTER*(CNAMSZ)    CNAMES ( MXCLSG )
      CHARACTER*(CVALSZ)    CVALS  ( MAXVAL )
      CHARACTER*(DECLEN)    DECLS  ( MXCLSG )
      CHARACTER*(MAXSTR)    FATBUF ( MAXFVL )
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(TNAMSZ)    TABLES ( NTABS  )
 
      DOUBLE PRECISION      DVALS  ( MAXVAL )
      DOUBLE PRECISION      TVALS  ( MAXVAL )
 
      INTEGER               CCLASS ( MXCLSG )
      INTEGER               COLNO
      INTEGER               DIMS   ( MXCLSG )
      INTEGER               DTYPES ( MXCLSG )
      INTEGER               ENTSZS ( MAXROW )
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IVALS  ( MAXVAL )
      INTEGER               LOC
      INTEGER               MAXSTL
      INTEGER               NCOLS
      INTEGER               NCOMCH
      INTEGER               NROWS
      INTEGER               P
      INTEGER               RCPTRS ( MAXROW )
      INTEGER               SEGNO
      INTEGER               SEGTYP
      INTEGER               STLENS ( MXCLSG )
      INTEGER               TABNO
      INTEGER               WKINDX ( MAXROW )
 
      LOGICAL               INDEXD ( MXCLSG )
      LOGICAL               NLFLGS ( MAXROW )
      LOGICAL               NULLOK ( MXCLSG )
 
C
C     Save ALL variables to prevent TSPICE_C crashing on cygwin.
C
      SAVE
 
C
C     Initial values
C
      DATA                  TABLES / 'SCALAR_1',
     .                               'SCALAR_2',
     .                               'SCALAR_3',
     .                               'SCALAR_4',
     .                               'VECTOR_1',
     .                               'VECTOR_2'   /
 
 
C
C     Use discovery check-in.
C
 
C
C     Delete the old version of the file, if it exists.
C
      IF ( EXISTS(FILE) ) THEN
         CALL DELFIL ( FILE )
      END IF
 
C
C     Set up the number of reserved comment characters and the internal
C     file name.
C
      NCOMCH  =  1024 * FILENO
      IFNAME  =  'EK TEST FILE #*'
      CALL REPMI ( IFNAME, '*', FILENO, IFNAME )
 
C
C     Open a new E-kernel.
C
      CALL EKOPN ( FILE, IFNAME, NCOMCH, HANDLE )
 
C
C     For each table:
C
      DO TABNO = 1, NTABS
 
C
C        Look up the schema for this table.
C
         CALL TSTSCH ( TABLES(TABNO), MXROWS,
     .                 SEGTYP,        NROWS,   NCOLS,
     .                 CNAMES,        CCLASS,  DTYPES,  STLENS,
     .                 DIMS,          INDEXD,  NULLOK,  DECLS )
 
C
C        Make sure we're not getting more rows than we expected.
C
         CALL MAXAI ( STLENS, NCOLS, MAXSTL, LOC )
 
         IF ( NROWS .GT. MAXROW ) THEN
 
            CALL ERRACT ( 'SET', 'ABORT'  )
            CALL ERRDEV ( 'SET', 'SCREEN' )
            CALL ERRPRT ( 'SET', 'ALL'    )
 
            CALL CHKIN  ( 'ZZTSTEK'                                 )
            CALL SETMSG ( 'Oops! Max number of rows that can be '   //
     .                    'handled by this routine is #; number '   //
     .                    'handed back by TSTSCH is #.'             )
            CALL ERRINT ( '#',  MAXROW                              )
            CALL ERRINT ( '#',  NROWS                               )
            CALL SIGERR ( 'SPICE(BUG)'                              )
            CALL CHKOUT ( 'ZZTSTEK'                                 )
 
         ELSE IF (        ( NROWS  .GT. MAXFAT )
     .             .AND.  ( MAXSTL .GT. CVALSZ )  ) THEN
 
            CALL ERRACT ( 'SET', 'ABORT'  )
            CALL ERRDEV ( 'SET', 'SCREEN' )
            CALL ERRPRT ( 'SET', 'ALL'    )
 
            CALL CHKIN  ( 'ZZTSTEK'                                    )
            CALL SETMSG ( 'Oops! Max number of long rows that can be '//
     .                    'handled by this routine is #; number '     //
     .                    'handed back by TSTSCH is #.'                )
            CALL ERRINT ( '#',  MAXFAT                                 )
            CALL ERRINT ( '#',  NROWS                                  )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'ZZTSTEK'                                    )
 
         END IF
 
C
C        Initiate a fast load.
C
         CALL EKIFLD (  HANDLE,  TABLES(TABNO),  NCOLS,  NROWS,
     .                  CNAMES,  DECLS,          SEGNO,  RCPTRS  )
 
C
C        Load the columns one by one. Each column and associated
C        arrays must be filled in first.  Obtain data, entry sizes,
C        and null flags from TSTENT.
C
         DO COLNO = 1, NCOLS
C
C           Initialize the data pointer to point to the first free
C           slot.
C
            P  =  1
 
            DO I = 1, NROWS
 
               IF ( STLENS(COLNO) .GT. CVALSZ ) THEN
C
C                 We're dealing with some very long strings.  Use
C                 the fat string buffer.
C
                  CALL TSTENT ( FILENO,        TABLES(TABNO), SEGNO,
     .                          CNAMES(COLNO), I,             MAXENT,
     .                          ENTSZS(I),     FATBUF(P),     DVALS(P),
     .                          IVALS(P),      TVALS(P),      NLFLGS(I))
 
               ELSE
C
C                 Use the normal buffers.
C
                  CALL TSTENT ( FILENO,        TABLES(TABNO), SEGNO,
     .                          CNAMES(COLNO), I,             MAXENT,
     .                          ENTSZS(I),     CVALS(P),      DVALS(P),
     .                          IVALS(P),      TVALS(P),      NLFLGS(I))
               END IF

C
C              Advance the data pointer by the number of elements in
C              the current entry.
C
               P  =  P + ENTSZS(I)
 
            END DO
 
C
C           The column is ready to be added.  Choose the addition
C           routine based on the column's data type.
C
            IF ( DTYPES(COLNO) .EQ. CHR ) THEN
 
 
               IF ( STLENS(COLNO) .GT. CVALSZ ) THEN
 
                  CALL EKACLC ( HANDLE, SEGNO,  CNAMES(COLNO), FATBUF,
     .                          ENTSZS, NLFLGS, RCPTRS,        WKINDX )
 
               ELSE
 
                  CALL EKACLC ( HANDLE, SEGNO,  CNAMES(COLNO), CVALS,
     .                          ENTSZS, NLFLGS, RCPTRS,        WKINDX )
               END IF
 
 
            ELSE IF ( DTYPES(COLNO) .EQ. DP ) THEN
 
               CALL EKACLD (  HANDLE,  SEGNO,   CNAMES(COLNO),  DVALS,
     .                        ENTSZS,  NLFLGS,  RCPTRS,         WKINDX )
 
            ELSE IF ( DTYPES(COLNO) .EQ. INT ) THEN
 
               CALL EKACLI (  HANDLE,  SEGNO,   CNAMES(COLNO),  IVALS,
     .                        ENTSZS,  NLFLGS,  RCPTRS,         WKINDX )
 
            ELSE
C
C              The data type is TIME.  Use the DP column add routine.
C
               CALL EKACLD (  HANDLE,  SEGNO,   CNAMES(COLNO),  TVALS,
     .                        ENTSZS,  NLFLGS,  RCPTRS,         WKINDX )
 
            END IF
 
         END DO
 
C
C        Finish the fast load for this table.
C
         CALL EKFFLD ( HANDLE, SEGNO, RCPTRS )
 
 
      END DO
 
C
C     Close the EK file.
C
      CALL EKCLS ( HANDLE )

 
      RETURN
      END
