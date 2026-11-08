!$Procedure  TSTEK ( Produce EK column entries for EK testing )

      SUBROUTINE TSTEK ( FILE, FILENO, MXROWS, LOAD, HANDLE )
      IMPLICIT NONE

! $ Abstract
!
!     Create EK files for testing the EK writing and reading routines.
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

      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektype.inc'


      CHARACTER*(*)         FILE
      INTEGER               FILENO
      INTEGER               MXROWS
      LOGICAL               LOAD
      INTEGER               HANDLE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     FILE       I   The name of an SPK file to create.
!     FILENO     I   Index of file in sequence of test EKs.
!     MXROWS     I   Maximum number of rows allowed in any table.
!     LOAD       I   Logical indicating if file should be loaded.
!     HANDLE     O   Handle if file is loaded by TSTEK.
!
! $ Detailed_Input
!
!     FILE           is the name of an EK file to create for use in
!                    software testing.
!
!                    If the file specified already exists, the existing
!                    file is deleted and a new one created with the same
!                    name in its place.
!
!     FILENO         is the ordinal position of the named EK in a
!                    sequence of test EKs.  FILENO will be reflected in
!                    the EK data, so data may be traced to their source
!                    EK files.
!
!     MXROWS         is the maximum number of rows allowed in any table.
!                    MXROWS should be at least 4000 for robust testing,
!                    but it may be set as small as 10 for quick tests.
!
!     LOAD           is a logical flag indicating whether or not the
!                    created SPK file should be loaded.  If LOAD is
!                    TRUE the file is loaded.  If LOAD is FALSE the
!                    file is not loaded by this routine.
!
! $ Detailed_Output
!
!     HANDLE         is the handle attached to the EK file if LOAD is
!                    .TRUE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the specified file already exists, it is deleted and
!        replaced by the file created by this routine.
!
!     2) All other exceptions are diagnosed by routines in the call tree
!        of this routine.
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
!     testing of the SPICELIB EK system.  TSTEK uses TSTENT to create a
!     predictable set of test data with which it populates a set of
!     binary EK files.  Data fetched from those files can be compared
!     with outputs obtained directly from TSTENT.
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
!              C_COL_4          CHARACTER*(12)    Yes        Yes
!              C_COL_5          CHARACTER*(12)    Yes        Yes
!              C_COL_6          CHARACTER*(12)    Yes        Yes
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
!              C_COL_1          CHARACTER*(12)    No         No
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
!              C_COL_1          CHARACTER*(12)    Yes        Yes
!              C_COL_2          CHARACTER*(12)    Yes        Yes
!              C_COL_3          CHARACTER*(12)    Yes        Yes
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
!-    Testing Utilities Version 1.4.0, 10-NOV-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in calls to TSTENT.
!
!-    Testing Utilities Version 1.3.0, 05-JAN-2005 (BVS)
!
!        Added SAVE statement to prevent TSPICE_C crashing
!        on cygwin.
!
!-    Testing Utilities Version 1.2.0, 07-SEP-2001 (NJB)
!
!        Bug fix:  call to REPMC replaced by call to REPMI.
!
!-    Testing Utilities Version 1.1.0, 28-JUL-1999 (WLT)
!
!        Added code so that the E-kernel will be registered with
!        the Test Utilities File Registry (FILREG).  This way
!        when a test family is done, the file will automatically
!        be deleted.
!
!-    Testing Utilities Version 1.0.0, 15-JUL-1999 (NJB)
!
!-&

! $ Revisions
!
!-    Testing Utilities Version 1.2.0, 07-SEP-2001 (NJB)
!
!        Bug fix:  call to REPMC replaced by call to REPMI.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               EXISTS

!
!     Local parameters
!
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

!
!     Local variables
!
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

!
!     Save ALL variables to prevent TSPICE_C crashing on cygwin.
!
      SAVE

!
!     Initial values
!
      DATA                  TABLES / 'SCALAR_1', &
                                     'SCALAR_2', &
                                     'SCALAR_3', &
                                     'SCALAR_4', &
                                     'VECTOR_1', &
                                     'VECTOR_2'   /


!
!     Use discovery check-in.
!

!
!     Delete the old version of the file, if it exists.
!
      IF ( EXISTS(FILE) ) THEN
         CALL DELFIL ( FILE )
      END IF

!
!     Set up the number of reserved comment characters and the internal
!     file name.
!
      NCOMCH  =  1024 * FILENO
      IFNAME  =  'EK TEST FILE #*'
      CALL REPMI ( IFNAME, '*', FILENO, IFNAME )

!
!     Open a new E-kernel.
!
      CALL EKOPN ( FILE, IFNAME, NCOMCH, HANDLE )

!
!     For each table:
!
      DO TABNO = 1, NTABS

!
!        Look up the schema for this table.
!
         CALL TSTSCH ( TABLES(TABNO), MXROWS, &
                       SEGTYP,        NROWS,   NCOLS, &
                       CNAMES,        CCLASS,  DTYPES,  STLENS, &
                       DIMS,          INDEXD,  NULLOK,  DECLS )

!
!        Make sure we're not getting more rows than we expected.
!
         CALL MAXAI ( STLENS, NCOLS, MAXSTL, LOC )

         IF ( NROWS .GT. MAXROW ) THEN

            CALL ERRACT ( 'SET', 'ABORT'  )
            CALL ERRDEV ( 'SET', 'SCREEN' )
            CALL ERRPRT ( 'SET', 'ALL'    )

            CALL CHKIN  ( 'TSTEK'                                   )
            CALL SETMSG ( 'Oops! Max number of rows that can be '   // &
                          'handled by this routine is #; number '   // &
                          'handed back by TSTSCH is #.'             )
            CALL ERRINT ( '#',  MAXROW                              )
            CALL ERRINT ( '#',  NROWS                               )
            CALL SIGERR ( 'SPICE(BUG)'                              )
            CALL CHKOUT ( 'TSTEK'                                   )

         ELSE IF (        ( NROWS  .GT. MAXFAT ) &
                   .AND.  ( MAXSTL .GT. CVALSZ )  ) THEN

            CALL ERRACT ( 'SET', 'ABORT'  )
            CALL ERRDEV ( 'SET', 'SCREEN' )
            CALL ERRPRT ( 'SET', 'ALL'    )

            CALL CHKIN  ( 'TSTEK'                                      )
            CALL SETMSG ( 'Oops! Max number of long rows that can be '// &
                          'handled by this routine is #; number '     // &
                          'handed back by TSTSCH is #.'                )
            CALL ERRINT ( '#',  MAXFAT                                 )
            CALL ERRINT ( '#',  NROWS                                  )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'TSTEK'                                      )

         END IF

!
!        Initiate a fast load.
!
         CALL EKIFLD (  HANDLE,  TABLES(TABNO),  NCOLS,  NROWS, &
                        CNAMES,  DECLS,          SEGNO,  RCPTRS  )

!
!        Load the columns one by one. Each column and associated
!        arrays must be filled in first.  Obtain data, entry sizes,
!        and null flags from TSTENT.
!
         DO COLNO = 1, NCOLS
!
!           Initialize the data pointer to point to the first free
!           slot.
!
            P  =  1

            DO I = 1, NROWS

               IF ( STLENS(COLNO) .GT. CVALSZ ) THEN
!
!                 We're dealing with some very long strings.  Use
!                 the fat string buffer.
!
                  CALL TSTENT ( FILENO,        TABLES(TABNO), SEGNO, &
                                CNAMES(COLNO), I,             MAXENT, &
                                ENTSZS(I),     FATBUF(P),     DVALS(P), &
                                IVALS(P),      TVALS(P),      NLFLGS(I))

               ELSE
!
!                 Use the normal buffers.
!
                  CALL TSTENT ( FILENO,        TABLES(TABNO), SEGNO, &
                                CNAMES(COLNO), I,             MAXENT, &
                                ENTSZS(I),     CVALS(P),      DVALS(P), &
                                IVALS(P),      TVALS(P),      NLFLGS(I))
               END IF

!
!              Advance the data pointer by the number of elements in
!              the current entry.
!
               P  =  P + ENTSZS(I)

            END DO

!
!           The column is ready to be added.  Choose the addition
!           routine based on the column's data type.
!
            IF ( DTYPES(COLNO) .EQ. CHR ) THEN


               IF ( STLENS(COLNO) .GT. CVALSZ ) THEN

                  CALL EKACLC ( HANDLE, SEGNO,  CNAMES(COLNO), FATBUF, &
                                ENTSZS, NLFLGS, RCPTRS,        WKINDX )

               ELSE

                  CALL EKACLC ( HANDLE, SEGNO,  CNAMES(COLNO), CVALS, &
                                ENTSZS, NLFLGS, RCPTRS,        WKINDX )
               END IF


            ELSE IF ( DTYPES(COLNO) .EQ. DP ) THEN

               CALL EKACLD (  HANDLE,  SEGNO,   CNAMES(COLNO),  DVALS, &
                              ENTSZS,  NLFLGS,  RCPTRS,         WKINDX )

            ELSE IF ( DTYPES(COLNO) .EQ. INT ) THEN

               CALL EKACLI (  HANDLE,  SEGNO,   CNAMES(COLNO),  IVALS, &
                              ENTSZS,  NLFLGS,  RCPTRS,         WKINDX )

            ELSE
!
!              The data type is TIME.  Use the DP column add routine.
!
               CALL EKACLD (  HANDLE,  SEGNO,   CNAMES(COLNO),  TVALS, &
                              ENTSZS,  NLFLGS,  RCPTRS,         WKINDX )

            END IF

         END DO

!
!        Finish the fast load for this table.
!
         CALL EKFFLD ( HANDLE, SEGNO, RCPTRS )


      END DO

!
!     Close the EK file.
!
      CALL EKCLS ( HANDLE )

!
!     Load the file if commanded to do so.
!
      IF ( LOAD ) THEN
         CALL EKLEF ( FILE, HANDLE )
      END IF

!
!     Register this file with the FILREG so that it can be
!     removed when the current test family is done with its
!     task.
!
      CALL TFILES ( FILE )

      RETURN
      END
