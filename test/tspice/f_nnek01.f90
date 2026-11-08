!$Procedure F_NNEK01 ( EK test, subset 1, non-native )

      SUBROUTINE F_NNEK01 ( OK )

! $ Abstract
!
!     Exercise the SPICELIB EK routines, subset 1. Use EKs having
!     non-native binary formats.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'ekattdsc.inc'
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektype.inc'
      INCLUDE 'errhnd.inc'
      INCLUDE 'zzddhman.inc'


      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine performs the same tests as F_EK01, but the file
!     EK1 is converted to non-native binary format before use in this
!     routine.
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
!     N.J. Bachman     (JPL)
!     E.D. Wright      (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 11-FEB-2015 (NJB)
!
!        Based on TSPICE Version 1.1.0, 18-NOV-2010 (EDW)
!
!-&

!
!     SPICELIB functions
!
      INTEGER               EKNSEG
      INTEGER               LASTNB

      LOGICAL               EXISTS

!
!     Local Parameters
!
      CHARACTER*(*)         BADCLK
      PARAMETER           ( BADCLK = '1/2/315662457.1839' )

      CHARACTER*(*)         BADUTC
      PARAMETER           ( BADUTC = '1990 JANN 01 12:00:00' )

      CHARACTER*(*)         CK
      PARAMETER           ( CK     = 'SCLKtest.bc' )

      CHARACTER*(*)         CLKNAM
      PARAMETER           ( CLKNAM = 'TEST_SCLK_NAME' )

      CHARACTER*(*)         CLKNM2
      PARAMETER           ( CLKNM2 = 'TEST_SCLK_NAME_2' )

      CHARACTER*(*)         CLKUDF
      PARAMETER           ( CLKUDF = 'UNDEFINED_SCLK' )

      CHARACTER*(*)         EK1
      PARAMETER           ( EK1    = 'test1nn.ek' )

      CHARACTER*(*)         EK1NAT
      PARAMETER           ( EK1NAT = 'test1.ek' )

      CHARACTER*(*)         EK2
      PARAMETER           ( EK2    = 'test2.ek' )

      CHARACTER*(*)         SCLK
      PARAMETER           ( SCLK   = 'testsclk.tsc' )

      CHARACTER*(*)         CLKSTR
      PARAMETER           ( CLKSTR = '1/315662457.1839' )

      CHARACTER*(*)         UTCSTR
      PARAMETER           ( UTCSTR = '1990 JAN 01 12:00:00' )


      INTEGER               CLSLEN
      PARAMETER           ( CLSLEN = 4 )

      INTEGER               DECLEN
      PARAMETER           ( DECLEN = 200 )

      INTEGER               NLOAD
      PARAMETER           ( NLOAD  = 20 )

      INTEGER               MAXENT
      PARAMETER           ( MAXENT = 20 )

      INTEGER               MAXROW
      PARAMETER           ( MAXROW = 20 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 4 )

      INTEGER               CVALSZ
      PARAMETER           ( CVALSZ = MAXSTR )

      INTEGER               NTABS
      PARAMETER           ( NTABS  = 6 )

      INTEGER               NTYPES
      PARAMETER           ( NTYPES = 4 )

      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL = MAXENT )

      INTEGER               SCLKID
      PARAMETER           ( SCLKID = -9 )

!
!     Local Variables
!
      CHARACTER*(CVALSZ)    CDATA
      CHARACTER*(TYPLEN)    CHTYPS ( NTYPES )
      CHARACTER*(CNAMSZ)    CNAMES ( MXCLSG )
      CHARACTER*(CVALSZ)    CVALS  ( MAXVAL )
      CHARACTER*(CNAMSZ)    COLUMN
      CHARACTER*(DECLEN)    DECLS  ( MXCLSG )
      CHARACTER*(TYPLEN)    DTYPE
      CHARACTER*(LMSGLN)    ERRMSG
      CHARACTER*(LMSGLN)    MSG
      CHARACTER*(MAXQRY)    QUERY
      CHARACTER*(DECLEN)    SHRTDC ( 1      )
      CHARACTER*(CNAMSZ)    SHRTNM ( 1      )
      CHARACTER*(CNAMSZ)    SSCNMS ( MXCLSG )
      CHARACTER*(TNAMSZ)    SSTNAM
      CHARACTER*(TYPLEN)    SSTYPS ( MXCLSG )
      CHARACTER*(TNAMSZ)    TABLES ( NTABS  )
      CHARACTER*(TNAMSZ)    TABNAM
      CHARACTER*(TNAMSZ)    TABS   ( MXCLSG )
      CHARACTER*(TIMLEN)    TIMSTR
      CHARACTER*(CLSLEN)    XCLASS ( MXCLSG )
      CHARACTER*(LMSGLN)    XMSG
      CHARACTER*(TYPLEN)    XTYPES ( MXCLSG )

      DOUBLE PRECISION      DDATA  ( MAXENT )
      DOUBLE PRECISION      DVALS  ( MAXVAL )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      TDATA  ( MAXENT )
      DOUBLE PRECISION      TVALS  ( MAXENT )
      DOUBLE PRECISION      XET

      INTEGER               CCLASS ( MXCLSG )
      INTEGER               COLNO
      INTEGER               DIMS   ( MXCLSG )
      INTEGER               DTYPES ( MXCLSG )
      INTEGER               ELTIDX
      INTEGER               FILENO
      INTEGER               HANDLE
      INTEGER               EK1HAN
      INTEGER               EK2HAN
      INTEGER               I
      INTEGER               IDATA  ( MAXENT )
      INTEGER               IVALS  ( MAXVAL )
      INTEGER               N
      INTEGER               NATBFF
      INTEGER               NCOLS
      INTEGER               NELTS
      INTEGER               NMROWS
      INTEGER               NNBFF
      INTEGER               NROWS
      INTEGER               NSEG
      INTEGER               ROWNO
      INTEGER               RCPTRS ( MAXVAL )
      INTEGER               SEGNO
      INTEGER               SEGTYP
      INTEGER               SELIDX
      INTEGER               SSDIMS ( MXCLSG )
      INTEGER               SSLENS ( MXCLSG )
      INTEGER               SSNCOL
      INTEGER               SSNROW
      INTEGER               STLENS ( MXCLSG )
      INTEGER               TABNO
      INTEGER               UNIT
      INTEGER               XBEGS  ( MXCLSG )
      INTEGER               XPBEGS ( MXCLSG )
      INTEGER               XENDS  ( MXCLSG )
      INTEGER               XPENDS ( MXCLSG )
      INTEGER               XNELTS
      INTEGER               XNROWS
      INTEGER               WKINDX ( MAXVAL )

      LOGICAL               ERROR
      LOGICAL               FOUND
      LOGICAL               INDEXD ( MXCLSG )
      LOGICAL               ISNULL
      LOGICAL               NLFLGS ( MAXVAL )
      LOGICAL               NULLOK ( MXCLSG )
      LOGICAL               SSIDXD ( MXCLSG )
      LOGICAL               SSNLOK ( MXCLSG )
      LOGICAL               XNULL

!
!     Saved all.
!
      SAVE

!
!     Initial values
!
      DATA                 CHTYPS / 'CHR', 'DP', 'INT', 'TIME' /

      DATA                 TABLES / 'SCALAR_1', &
                                    'SCALAR_2', &
                                    'SCALAR_3', &
                                    'SCALAR_4', &
                                    'VECTOR_1', &
                                    'VECTOR_2'  /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_NNEK01' )


!
! --- Setup
!
      CALL TCASE ( 'Set native and non-native binary file format ' &
      //           'codes.'                                          )


      CALL ZZDDHNFC ( NATBFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( NATBFF .EQ. LTLI3E ) THEN

         NNBFF = BIGI3E
      ELSE
         NNBFF = LTLI3E
      END IF

!
! --- Case 1
!
      CALL TCASE ( 'Test the EK writing and fast load routines:  '    // &
                   'EKOPN, EKIFLD, EKACLC, EKACLD, EKACLI, '          // &
                   'EKFFLD, EKCLS.  Also test EKSSUM.  All of '       // &
                   'this is done by TSTEK, which is called here.'     )

!
!     Create an EK. Convert to non-native format.
!
      FILENO = 1

      CALL TSTEK  ( EK1NAT, FILENO, MAXROW, .FALSE., EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_BINGO ( EK1NAT, EK1, NNBFF )
      CALL CHCKXC ( .FALSE., ' ', OK    )

      CALL DELFIL ( EK1NAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Register the non-native EK with the FILREG so that it can be
!     removed when the current test family is done with its
!     task.
!
      CALL TFILES ( EK1 )


!
! --- Case 2
!
      CALL TCASE ( 'Test EKOPR, EKSSUM and EKNSEG.  Get segment ' // &
                   'summaries and make sure they''re compatible ' // &
                   'with the schemas returned by TSTSCH.'         )

!
!     Find out how many segments are in the EK.  By the specification
!     of TSTEK, there's one segment per table.
!
      CALL EKOPR  ( EK1, EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )


      NSEG = EKNSEG ( EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO SEGNO = 1, NSEG
!
!        Start a new test case.
!
         MSG = 'Testing EKTNAM, EKCCNT, EKCII for segment #'
         CALL REPMI ( MSG, '#', SEGNO, MSG )


!
! --- Case 3
!
         CALL TCASE ( MSG )

!
!        Get the summary for this segment.
!
!         WRITE (*,*) 'SEGNO  = ', SEGNO
!         WRITE (*,*) 'EK1HAN = ', EK1HAN


         CALL EKSSUM ( EK1HAN,  SEGNO,   SSTNAM,  SSNROW, &
                       SSNCOL,  SSCNMS,  SSTYPS,  SSDIMS, &
                       SSLENS,  SSIDXD,  SSNLOK          )

         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Compare the attributes given by the segment summary to those
!        returned by TSTSCH.  These are:
!
!           - table name
!           - column count
!           - row count
!           - column names
!           - column descriptors
!
!        For each column descriptor, compare the attributes:
!
!           - data type
!           - string length
!           - size
!           - is the column indexed?
!           - does the column allow null values?
!
!
!        Look up the schema for this table.
!
         TABNAM = TABLES(SEGNO)

         CALL TSTSCH ( TABNAM, MAXROW, SEGTYP, NROWS,  NCOLS, &
                       CNAMES, CCLASS, DTYPES, STLENS, DIMS, &
                       INDEXD, NULLOK, DECLS                   )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check the table name.
!
         CALL CHCKSC ( 'SSTNAM', SSTNAM, '=', TABNAM, OK )


!
!        Check the row and column counts.
!
         CALL CHCKSI ( 'NROWS from EKSSUM', &
                        SSNROW, '=', NROWS, 0, OK )

         CALL CHCKSI ( 'NCOLS from EKSSUM', &
                        SSNCOL, '=', NCOLS, 0, OK )

!
!        For each column in the current table, check the column's
!        attributes.  The attribute block index parameters are defined
!        in the include file ekattdsc.inc.
!
         DO I = 1, NCOLS
!
!           Check the column name.
!
            CALL CHCKSC ( 'Column name', SSCNMS(I), '=', CNAMES(I), OK )

!
!           Check the current column's data type.
!
            CALL CHCKSC ( 'Column data type', &
                           SSTYPS(I), '=', CHTYPS(DTYPES(I)), OK )

!
!           If the data type is character, check the string length.
!
            IF ( DTYPES(I) .EQ. CHR ) THEN

                CALL CHCKSI ( 'Column string length', &
                              SSLENS(I), '=', STLENS(I), 0, OK )
            END IF

!
!           Check the current column's entry size.
!
            CALL CHCKSI ( 'Column entry size', &
                           SSDIMS(I), '=', DIMS(I), 0, OK )

!
!           Check the current column's index flag.
!
            CALL CHCKSL ( 'Column index flag', &
                          SSIDXD(I), INDEXD(I), OK )

!
!           Check the current column's null ok flag.
!
            CALL CHCKSL ( 'Column null ok flag', &
                          SSNLOK(I), NULLOK(I), OK )

         END DO

!
!        We're done with the current column.
!

      END DO
!
!     We're done with the current table.
!

!
!     Close the EK.
!
      CALL EKCLS ( EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case 4
!
!
!
!     Load the EK into the query system.
!
      CALL TCASE ( 'Ah, the nitty gritty.  Test EKFIND, ENELT, and '// &
                   'the fetching triplets EKGC, EKGD, EKGI.'        )

      CALL EKLEF ( EK1, EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We start off with a simple case.
!
      QUERY = 'select c_col_1, d_col_1, i_col_1, t_col_1 from ' // &
              'scalar_2 order by row_no'


      CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTMSG ( '#', 'The error message was:  #' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ERROR flag', ERROR, .FALSE., OK )

!
!     The table 'SCALAR_2' occupies the second segment of the file
!     designated by EK1.  Segment numbers start at 1 and increment from
!     there.
!
      SEGNO = 2

      CALL EKSSUM ( EK1HAN,  SEGNO,   SSTNAM,  SSNROW, &
                    SSNCOL,  SSCNMS,  SSTYPS,  SSDIMS, &
                    SSLENS,  SSIDXD,  SSNLOK          )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NMROWS', &
                     NMROWS, '=', SSNROW, 0, OK )


!
!     Check the data.
!
      DO ROWNO = 1, NMROWS
!
!        First, fetch and test the character data.
!
         SELIDX = 1
         ELTIDX = 1

         CALL TSTMSG ( '#', &
                       'table = SCALAR_2; selidx = 1; col = c_col_1; '// &
                       'row = #; eltidx = 1.'                         )

         CALL TSTMSI ( ROWNO )

!
!        Fetch the value for c_col_1 from the current row.
!
         CALL EKGC ( SELIDX, ROWNO, ELTIDX, CDATA, ISNULL, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        Look up the expected column entry.
!
         CALL TSTENT ( FILENO,      'SCALAR_2',   SEGNO, &
                       'C_COL_1',   ROWNO,        MAXVAL, &
                       XNELTS,      CVALS,        DVALS, &
                       IVALS,       TVALS,        XNULL  )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check the null flag returned by EKGC.
!
         CALL CHCKSL ( 'ISNULL', ISNULL, XNULL, OK )

!
!        Check the number of elements in the entry.
!
         CALL EKNELT ( SELIDX, ROWNO, NELTS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'NELTS', NELTS, '=', XNELTS, 0, OK )


         IF ( .NOT. ISNULL ) THEN
!
!           Check the character string returned by EKGC.
!
            CALL CHCKSC ( 'char value from EKGC', &
                          CDATA, '=', CVALS, OK   )

         END IF

!
!        Check the d.p. data next.
!
         SELIDX = 2
         ELTIDX = 1

         CALL TSTMSG ( '#', &
                       'table = SCALAR_2; selidx = 2; col = d_col_1; '// &
                       'row = #; eltidx = 1.'                         )

         CALL TSTMSI ( ROWNO )

!
!        Fetch the value for d_col_1 from the current row.
!
         CALL EKGD ( SELIDX, ROWNO, ELTIDX, DDATA, ISNULL, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        Look up the expected column entry.
!
         CALL TSTENT ( FILENO,      'SCALAR_2',   SEGNO, &
                       'D_COL_1',   ROWNO,        MAXVAL, &
                       XNELTS,      CVALS,        DVALS, &
                       IVALS,       TVALS,        XNULL  )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check the null flag returned by EKGD.
!
         CALL CHCKSL ( 'ISNULL', ISNULL, XNULL, OK )

!
!        Check the number of elements in the entry.
!
         CALL EKNELT ( SELIDX, ROWNO, NELTS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'NELTS', NELTS, '=', XNELTS, 0, OK )


         IF ( .NOT. ISNULL ) THEN
!
!           Check the d.p. value returned by EKGD.
!
            CALL CHCKSD ( 'D.P. value from EKGD', &
                          DDATA, '=', DVALS, 0.D0, OK   )

         END IF

!
!        Check the integer data.
!
         SELIDX = 3
         ELTIDX = 1

         CALL TSTMSG ( '#', &
                       'table = SCALAR_2; selidx = 3; col = i_col_1; '// &
                       'row = #; eltidx = 1.'                         )

         CALL TSTMSI ( ROWNO )

!
!        Fetch the value for i_col_1 from the current row.
!
         CALL EKGI ( SELIDX, ROWNO, ELTIDX, IDATA, ISNULL, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        Look up the expected column entry.
!
         CALL TSTENT ( FILENO,      'SCALAR_2',   SEGNO, &
                       'I_COL_1',   ROWNO,        MAXVAL, &
                       XNELTS,      CVALS,        DVALS, &
                       IVALS,       TVALS,        XNULL  )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check the null flag returned by EKGI.
!
         CALL CHCKSL ( 'ISNULL', ISNULL, XNULL, OK )

!
!        Check the number of elements in the entry.
!
         CALL EKNELT ( SELIDX, ROWNO, NELTS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'NELTS', NELTS, '=', XNELTS, 0, OK )


         IF ( .NOT. ISNULL ) THEN
!
!           Check the integer value returned by EKGI.
!
            CALL CHCKSI ( 'Integer value from EKGI', &
                          IDATA, '=', IVALS, 0, OK   )

         END IF

!
!        Check the time data.
!
         SELIDX = 4
         ELTIDX = 1

         CALL TSTMSG ( '#', &
                       'table = SCALAR_2; selidx = 4; col = t_col_1; '// &
                       'row = #; eltidx = 1.'                         )

         CALL TSTMSI ( ROWNO )

!
!        Fetch the value for t_col_1 from the current row.
!
         CALL EKGD ( SELIDX, ROWNO, ELTIDX, TDATA, ISNULL, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        Look up the expected column entry.
!
         CALL TSTENT ( FILENO,      'SCALAR_2',   SEGNO, &
                       'T_COL_1',   ROWNO,        MAXVAL, &
                       XNELTS,      CVALS,        DVALS, &
                       IVALS,       TVALS,        XNULL  )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check the null flag returned by EKGD.
!
         CALL CHCKSL ( 'ISNULL', ISNULL, XNULL, OK )

!
!        Check the number of elements in the entry.
!
         CALL EKNELT ( SELIDX, ROWNO, NELTS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'NELTS', NELTS, '=', XNELTS, 0, OK )


         IF ( .NOT. ISNULL ) THEN
!
!           Check the time value returned by EKGD.
!
            CALL CHCKSD ( 'Time value from EKGD', &
                          DDATA, '=', TVALS, 0.D0, OK   )

         END IF

      END DO



!
!     Now for a more comprehensive test.
!
!
! --- Case 5
!
      CALL TCASE ( 'This time, we loop over all tables, and we ' // &
                   ' check all entries in each table.'           )

      DO TABNO = 1, NTABS

!
!        Get the row and column count for this table.
!
         SEGNO = TABNO

!
!        Get the summary for this segment.
!
         CALL EKSSUM ( EK1HAN,  SEGNO,   SSTNAM,  SSNROW, &
                       SSNCOL,  SSCNMS,  SSTYPS,  SSDIMS, &
                       SSLENS,  SSIDXD,  SSNLOK          )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Build the query string:  select all columns from the current
!        table.
!
         QUERY = 'SELECT'
         CALL SUFFIX ( SSCNMS(1), 1, QUERY )

         DO COLNO = 2, SSNCOL

            CALL SUFFIX ( ',',           0, QUERY )
            CALL SUFFIX ( SSCNMS(COLNO), 1, QUERY )

         END DO

         CALL SUFFIX ( 'FROM',            1, QUERY )
         CALL SUFFIX ( TABLES(TABNO),     1, QUERY )
         CALL SUFFIX ( 'ORDER BY ROW_NO', 1, QUERY )

!
!        Issue the query.
!
         CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Make sure there was no query resolution error.
!
         CALL TSTMSG ( '#', 'The error message was:  #' )
         CALL TSTMSC ( ERRMSG )

         CALL CHCKSL ( 'ERROR flag', ERROR, .FALSE., OK )

!
!        Check NMROWS.
!
         CALL CHCKSI ( 'NMROWS', NMROWS, '=', SSNROW, 0, OK )

!
!        Check the data.
!
         NCOLS = SSNCOL

         DO ROWNO = 1, NMROWS


            DO SELIDX = 1, NCOLS
!
!              Get the name and data type of the current column;
!              process the column accordingly.
!
               COLUMN = SSCNMS(SELIDX)

               DTYPE  = SSTYPS(SELIDX)


               MSG =  '#' // &
                      'Table is #. Column is #. Row is #. ' // &
                      'Current select index is #.'

               CALL REPMC ( MSG, '#', TABLES(TABNO), MSG )
               CALL REPMC ( MSG, '#', COLUMN,        MSG )
               CALL REPMI ( MSG, '#', ROWNO,         MSG )
               CALL REPMI ( MSG, '#', SELIDX,        MSG )

!
! --- Case 6
!
               CALL TCASE ( MSG )

!
!              Look up the expected column entry.
!
               CALL TSTENT ( FILENO,      TABLES(TABNO),   SEGNO, &
                             COLUMN,      ROWNO,           MAXVAL, &
                             XNELTS,      CVALS,           DVALS, &
                             IVALS,       TVALS,           XNULL  )

               CALL CHCKXC ( .FALSE., ' ', OK )


!
!              Check the number of elements in the entry.
!
               CALL EKNELT ( SELIDX, ROWNO, NELTS )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSI ( 'NELTS', NELTS, '=', XNELTS, 0, OK )


!
!              Check the data.
!
               DO ELTIDX = 1, NELTS

                  CALL TSTMSG ( '#', &
                                'Table is #. Column is #. Row is #. ' // &
                                'Current element index is #.'         )
                  CALL TSTMSC ( TABLES(TABNO) )
                  CALL TSTMSC ( COLUMN        )
                  CALL TSTMSI ( ROWNO         )
                  CALL TSTMSI ( ELTIDX        )


                  IF ( DTYPE .EQ. 'CHR' ) THEN

                     CALL EKGC ( SELIDX, ROWNO,  ELTIDX, &
                                 CDATA,  ISNULL, FOUND  )

!
!                    Make sure no error was signaled.
!
                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    Make sure the element was found.
!
                     CALL CHCKSL ( 'FOUND flag', FOUND, .TRUE., OK )

!
!                    Check the null flag returned by EKGC.
!
                     CALL CHCKSL ( 'NULL flag', ISNULL, XNULL, OK )

                     IF ( .NOT. ISNULL ) THEN
!
!                       Check the character string returned by EKGC.
!
                        CALL CHCKSC ( 'char value from EKGC', &
                                      CDATA, '=', CVALS(ELTIDX), OK )
                     END IF



                  ELSE IF ( DTYPE .EQ. 'DP' ) THEN

                     CALL EKGD ( SELIDX, ROWNO,  ELTIDX, &
                                 DDATA,  ISNULL, FOUND  )

!
!                    Make sure no error was signaled.
!
                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    Make sure the element was found.
!
                     CALL CHCKSL ( 'FOUND flag', FOUND, .TRUE., OK )

!
!                    Check the null flag returned by EKGD.
!
                     CALL CHCKSL ( 'NULL flag', ISNULL, XNULL, OK )

                     IF ( .NOT. ISNULL ) THEN
!
!                       Check the d.p. value returned by EKGD.
!
                        CALL CHCKSD ( 'd.p. value from EKGD', &
                                      DDATA, &
                                      '=', &
                                      DVALS(ELTIDX), 0.D0, OK )
                     END IF


                  ELSE IF ( DTYPE .EQ. 'INT' ) THEN

                     CALL EKGI ( SELIDX, ROWNO,  ELTIDX, &
                                 IDATA,  ISNULL, FOUND  )

!
!                    Make sure no error was signaled.
!
                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    Make sure the element was found.
!
                     CALL CHCKSL ( 'FOUND flag', FOUND, .TRUE., OK )

!
!                    Check the null flag returned by EKGI.
!
                     CALL CHCKSL ( 'NULL flag', ISNULL, XNULL, OK )

                     IF ( .NOT. ISNULL ) THEN
!
!                       Check the integer value returned by EKGI.
!
                        CALL CHCKSI ( 'Integer value from EKGI', &
                                      IDATA, &
                                      '=', &
                                      IVALS(ELTIDX), 0, OK )
                     END IF

                  ELSE
!
!                    DTYPE == 'TIME'
!

                     CALL EKGD ( SELIDX, ROWNO,  ELTIDX, &
                                 DDATA,  ISNULL, FOUND  )

!
!                    Make sure no error was signaled.
!
                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    Make sure the element was found.
!
                     CALL CHCKSL ( 'FOUND flag', FOUND, .TRUE., OK )

!
!                    Check the null flag returned by EKGD.
!
                     CALL CHCKSL ( 'NULL flag', ISNULL, XNULL, OK )

                     IF ( .NOT. ISNULL ) THEN
!
!                       Check the time value returned by EKGD.
!
                        CALL CHCKSD ( 'Time value from EKGD', &
                                      DDATA, &
                                      '=', &
                                      TVALS(ELTIDX), 0.D0, OK )
                     END IF

                  END IF

!
!                 Done with the current element.
!
               END DO
!
!              Done with the current column.
!
            END DO
!
!           Done with the current row.
!
         END DO
!
!        Done with the current table.
!
      END DO
!
!     We've queried each EK table and checked all data returned by
!     the queries.
!



!
! --- Case 7
!

      CALL TCASE (  'Open a scratch EK.  Write to it.  Make sure ' // &
                    'the data''s there.  Close it.  Make sure it ' // &
                    'goes away.'                                   )


      CALL EKOPS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SHRTNM(1) =   'COL_1'
      SHRTDC(1) =   'DATATYPE = INTEGER'

      CALL EKIFLD ( HANDLE, &
                    'TABLE_1', &
                    1, &
                    1, &
                    SHRTNM, &
                    SHRTDC, &
                    SEGNO, &
                    RCPTRS                )

      NLFLGS(1) =  .FALSE.
      IVALS (1) =  99

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add the column to the table.
!
      CALL EKACLI ( HANDLE,  SEGNO,   'COL_1',  IVALS, &
                    1,       NLFLGS,  RCPTRS,   WKINDX )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Finish the fast load for this table.
!
      CALL EKFFLD ( HANDLE, SEGNO, RCPTRS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the summary for the table.
!
      CALL EKSSUM ( HANDLE,  SEGNO,   SSTNAM,  SSNROW, &
                    SSNCOL,  SSCNMS,  SSTYPS,  SSDIMS, &
                    SSLENS,  SSIDXD,  SSNLOK          )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the table and column name.  That's enough.
!
      CALL CHCKSC ( 'Table name',  SSTNAM,    '=', 'TABLE_1', OK )
      CALL CHCKSC ( 'Column name', SSCNMS(1), '=', 'COL_1',   OK )

!
!     Close this file.
!
      CALL EKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Is the file still there?  Shouldn't be.
!
      CALL DASHLU ( HANDLE, UNIT )

      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHHANDLE)', OK )



!
! --- Case 8
!
      CALL TCASE (  'Open an EK.  Write to it.  Make sure '  // &
                    'the data''s there.  Close it.  Open it '// &
                    'for appending. Write some more. Make '  // &
                    'sure the data''s there.'                )


      CALL TSTMSG ( '#', 'About to open EK file #.' )
      CALL TSTMSC ( EK2 )

      IF ( EXISTS(EK2) ) THEN

         CALL DELFIL ( EK2 )

      END IF


      CALL EKOPN ( EK2, EK2, 0, EK2HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SHRTNM(1) =   'COL_1'
      SHRTDC(1) =   'DATATYPE = INTEGER'

      CALL EKIFLD ( EK2HAN, &
                    'TABLE_1', &
                    1, &
                    1, &
                    SHRTNM, &
                    SHRTDC, &
                    SEGNO, &
                    RCPTRS                )

      NLFLGS(1) =  .FALSE.
      IVALS (1) =  100

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add the column to the table.
!
      CALL EKACLI ( EK2HAN,  SEGNO,   'COL_1',  IVALS, &
                    1,       NLFLGS,  RCPTRS,   WKINDX )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Finish the fast load for this table.
!
      CALL EKFFLD ( EK2HAN, SEGNO, RCPTRS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the summary for the table.
!
      CALL EKSSUM ( EK2HAN,  SEGNO,   SSTNAM,  SSNROW, &
                    SSNCOL,  SSCNMS,  SSTYPS,  SSDIMS, &
                    SSLENS,  SSIDXD,  SSNLOK          )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the table and column name.  That's enough.
!
      CALL CHCKSC ( 'Table name',  SSTNAM,    '=', 'TABLE_1', OK )
      CALL CHCKSC ( 'Column name', SSCNMS(1), '=', 'COL_1',   OK )

!
!     Close this file.
!
      CALL EKCLS ( EK2HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now open the file for write access and add more data.
!
      CALL TSTMSG ( '#', 'About to open EK file # for appending.' )
      CALL TSTMSC ( EK2 )

      CALL EKOPW  ( EK2, EK2HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SHRTNM(1) =   'COL_2'
      SHRTDC(1) =   'DATATYPE = INTEGER'

      CALL EKIFLD ( EK2HAN, &
                    'TABLE_2', &
                    1, &
                    1, &
                    SHRTNM, &
                    SHRTDC, &
                    SEGNO, &
                    RCPTRS                )

      NLFLGS(1) =  .FALSE.
      IVALS (1) =  200

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add the column to the table.
!
      CALL EKACLI ( EK2HAN,  SEGNO,   'COL_2',  IVALS, &
                    1,       NLFLGS,  RCPTRS,   WKINDX )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Finish the fast load for this table.
!
      CALL EKFFLD ( EK2HAN, SEGNO, RCPTRS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close this file.
!
      CALL EKCLS ( EK2HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Open the file for read access.
!
      CALL EKOPR  ( EK2,  EK2HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the summary for the table.
!
      CALL EKSSUM ( EK2HAN,  SEGNO,   SSTNAM,  SSNROW, &
                    SSNCOL,  SSCNMS,  SSTYPS,  SSDIMS, &
                    SSLENS,  SSIDXD,  SSNLOK          )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the table and column name.  That's enough.
!
      CALL CHCKSC ( 'Table name',  SSTNAM,    '=', 'TABLE_2', OK )
      CALL CHCKSC ( 'Column name', SSCNMS(1), '=', 'COL_2',   OK )

!
!     Close this file.
!
      CALL EKCLS ( EK2HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )




!
!
! --- Case 9
!
      CALL TCASE ( 'Test EKPSEL.  Build a query with a complex ' // &
                   'SELECT clause; tear it apart with EKPSEL. '  // &
                   'Verify that EKPSEL identifies the columns'' '// &
                   'attributes correctly.'                       )

!
!     Get segment summary for the SCALAR_2 table.
!
      CALL EKLEF ( EK1, EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEGNO = 2

      CALL EKSSUM ( EK1HAN,  SEGNO,   SSTNAM,  SSNROW, &
                    SSNCOL,  SSCNMS,  SSTYPS,  SSDIMS, &
                    SSLENS,  SSIDXD,  SSNLOK          )

      CALL CHCKXC ( .FALSE., ' ', OK )

      NCOLS = SSNCOL

!
!     Build the query string:  select all columns from the current
!     table.  Save the beginning and end locations of the column name
!     tokens.
!
      QUERY = 'SELECT'


      XPBEGS(1) = LASTNB( QUERY ) + 2

      CALL SUFFIX ( SSCNMS(1), 1, QUERY )

      XPENDS(1) = LASTNB( QUERY )


      DO COLNO = 2, SSNCOL

         CALL SUFFIX ( ',', 0, QUERY )

         XPBEGS(COLNO) = LASTNB( QUERY ) + 2

         CALL SUFFIX ( SSCNMS(COLNO), 1, QUERY )

         XPENDS(COLNO) = LASTNB( QUERY )

      END DO

      CALL SUFFIX ( 'FROM',            1, QUERY )
      CALL SUFFIX ( TABLES(SEGNO),     1, QUERY )
      CALL SUFFIX ( 'ORDER BY ROW_NO', 1, QUERY )


!
!     Analyze the query.
!
      CALL EKPSEL ( QUERY, &
                    N,       XBEGS,  XENDS,   XTYPES, &
                    XCLASS,  TABS,   CNAMES,  ERROR,   ERRMSG )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure query parsed correctly.
!
      CALL CHCKSL ( 'ERROR flag', ERROR, .FALSE., OK )

!
!     Make sure the error message is blank.
!
      CALL CHCKSC ( 'ERROR message', ERRMSG, '=', ' ', OK )

!
!     Check the number of SELECT expressions.
!
      CALL CHCKSI ( 'number of SELECT expressions N', &
                    N, '=', NCOLS, 0, OK              )


!
!     Check the expression bounds.
!
      CALL CHCKAI ( 'XBEGS', XBEGS, '=', XPBEGS, N, OK )
      CALL CHCKAI ( 'XENDS', XENDS, '=', XPENDS, N, OK )

!
!     For each selected column, check the
!
!        - Column name
!        - Table name
!        - Data type
!        - Expression class
!
      DO I = 1, N

         CALL CHCKSC ( 'Column names', CNAMES(I), '=', SSCNMS(I), OK )
         CALL CHCKSC ( 'Table names',  TABS(I),   '=', SSTNAM,    OK )

         CALL CHCKSC ( 'Data types',   XTYPES(I), '=', SSTYPS(I), OK )

         CALL CHCKSC ( 'Expression class', &
                        XCLASS(I),  '=',  'COL',  OK )

      END DO

!
!     Unload the EK.
!
      CALL EKUEF ( EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case 10
!
      CALL TCASE ( 'Test sorting:  select rows from scalar_2, ' // &
                   'ordering by c_col_2 (ascending).'           )
!
!     Use EK1.
!
      CALL EKLEF  ( EK1, EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the query.
!
      QUERY = 'select c_col_2 from scalar_2 ' // &
              'where '                        // &
              '(c_col_2 between '             // &
              '"SEG_2_C_COL_2_ROW_10_" '      // &
              'and '                          // &
              '"SEG_2_C_COL_2_ROW_19" ) '     // &
              'or '                           // &
              'c_col_2 like "X*" '            // &
              'order by c_col_2'


!
!     Issue the query.
!
      CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure there was no query resolution error.
!
      CALL TSTMSG ( '#', 'The error message was:  #' )
      CALL TSTMSC ( ERRMSG )

      CALL CHCKSL ( 'ERROR flag', ERROR, .FALSE., OK )

      IF ( .NOT. ERROR ) THEN
!
!        Check NMROWS.  We expect to find 5 rows.
!
         XNROWS = 5

         CALL CHCKSI ( 'NMROWS', NMROWS, '=', XNROWS, 0, OK )

!
!        Check the data.
!
         DO ROWNO = 1, XNROWS
!
!           Look up the expected column entry. Skip  over null entries.
!
            FILENO = 1
            SEGNO  = 2
            SELIDX = 1
            ELTIDX = 1

            CALL TSTENT ( FILENO,      'SCALAR_2',        SEGNO, &
                          'C_COL_2',   10 + 2*(ROWNO-1),  MAXVAL, &
                          XNELTS,      CVALS,             DVALS, &
                          IVALS,       TVALS,             XNULL  )

            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Fetch the actual column entry from the current row.
!
            CALL EKGC ( SELIDX, ROWNO,  ELTIDX, &
                        CDATA,  ISNULL, FOUND  )

!
!           Make sure no error was signaled.
!
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Make sure the element was found.
!
            CALL CHCKSL ( 'FOUND flag', FOUND, .TRUE., OK )

!
!           Check the null flag returned by EKGC.
!
            CALL CHCKSL ( 'ISNULL', ISNULL, XNULL, OK )


            IF ( .NOT. ISNULL ) THEN
!
!              Check the character string returned by EKGC.
!
               CALL CHCKSC ( 'char value from EKGC', &
                             CDATA, '=', CVALS(ELTIDX), OK   )

            END IF

         END DO

      END IF

!
! --- Case 11
!
      CALL TCASE ( 'Test sorting:  select rows from scalar_2, ' // &
                   'ordering by c_col_2 (descending).'          )

!
!     Do the same query, but sort in descending order.
!
!     Create the query.
!
      QUERY = 'select c_col_2 from scalar_2 ' // &
              'where '                        // &
              '(c_col_2 between '             // &
              '"SEG_2_C_COL_2_ROW_10_" '      // &
              'and '                          // &
              '"SEG_2_C_COL_2_ROW_19" ) '     // &
              'or '                           // &
              'c_col_2 like "X*" '            // &
              'order by c_col_2 desc'


!
!     Issue the query.
!
      CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure there was no query resolution error.
!
      CALL TSTMSG ( '#', 'The error message was:  #' )
      CALL TSTMSC ( ERRMSG )

      CALL CHCKSL ( 'ERROR flag', ERROR, .FALSE., OK )

      IF ( .NOT. ERROR ) THEN
!
!        Check NMROWS.  We expect to find 5 rows.
!
         XNROWS = 5

         CALL CHCKSI ( 'NMROWS', NMROWS, '=', XNROWS, 0, OK )

!
!        Check the data.
!
         DO ROWNO = 1, XNROWS
!
!           Look up the expected column entry. Skip  over null entries.
!
            FILENO = 1
            SEGNO  = 2
            SELIDX = 1
            ELTIDX = 1

            CALL TSTENT ( FILENO,      'SCALAR_2',        SEGNO, &
                          'C_COL_2',   18 - 2*(ROWNO-1),  MAXVAL, &
                          XNELTS,      CVALS,             DVALS, &
                          IVALS,       TVALS,             XNULL  )

            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Fetch the actual column entry from the current row.
!
            CALL EKGC ( SELIDX, ROWNO,  ELTIDX, &
                        CDATA,  ISNULL, FOUND  )

!
!           Make sure no error was signaled.
!
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Make sure the element was found.
!
            CALL CHCKSL ( 'FOUND flag', FOUND, .TRUE., OK )

!
!           Check the null flag returned by EKGC.
!
            CALL CHCKSL ( 'ISNULL', ISNULL, XNULL, OK )


            IF ( .NOT. ISNULL ) THEN
!
!              Check the character string returned by EKGC.
!
               CALL CHCKSC ( 'char value from EKGC', &
                             CDATA, '=', CVALS(ELTIDX), OK   )

            END IF

         END DO

      END IF

!
!     Unload the EK.
!
      CALL EKUEF ( EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case 12
!
      CALL TCASE ( 'Test EKUEF; make sure we don''t accumulate ' // &
                   'DAS links when we reload a file repeatedly.' )


      DO I = 1, 2*NLOAD

         CALL EKLEF  ( EK1, EK1HAN )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL EKUEF  ( EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASFNH ( EK1, EK1HAN )
      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHFILE)', OK )



!
!     EK time parsing tests follow.
!
!
! --- Case 13
!
      CALL TCASE ( 'Test ZZEKTCNV; convert SCLK string.' )

!
!     Create and load a leapseconds kernel.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create an SCLK kernel.  The routine we use for this purpose also
!     creates a C-kernel, which we don't need.
!
      CALL TSTCK3 ( CK, SCLK, .FALSE., .TRUE., .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Associate the name and ID of the clock.
!
      CALL BODDEF ( CLKNAM, SCLKID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Convert an SCLK string to ET; make sure we get the same result
!     returned by SCS2E.
!
      CALL SCS2E  ( SCLKID, CLKSTR, XET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TIMSTR = '# SCLK #'
      CALL REPMC ( TIMSTR, '#', CLKNAM, TIMSTR )
      CALL REPMC ( TIMSTR, '#', CLKSTR, TIMSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Convert the time string.
!
      CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the error flag.
!
      CALL CHCKSL ( 'ERROR', ERROR, .FALSE., OK )

!
!     Check the error message.
!
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', ' ', OK   )

!
!     Check the time.
!
      CALL CHCKSD ( 'ET', ET, '=', XET, 0.D0, OK )


!
! --- Case 14
!
!
!     Now use a name that doesn't contain the substring SCLK.
!
      CALL TCASE ( 'SCLK name doesn''t contain the substring SCLK.' )

      CALL BODDEF ( CLKNM2, SCLKID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCS2E  ( SCLKID, CLKSTR, XET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TIMSTR = '# SCLK #'
      CALL REPMC ( TIMSTR, '#', CLKNAM, TIMSTR )
      CALL REPMC ( TIMSTR, '#', CLKSTR, TIMSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Convert the time string.
!
      CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the error flag.
!
      CALL CHCKSL ( 'ERROR', ERROR, .FALSE., OK )

!
!     Check the error message.
!
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', ' ', OK   )

!
!     Check the time.
!
      CALL CHCKSD ( 'ET', ET, '=', XET, 0.D0, OK )




!
! --- Case 15
!
!
!     Now attempt conversion using an SCLK name that doesn't map to
!     an ID code.
!
      CALL TCASE ( 'Error: SCLK does not map to ID code.' )

      TIMSTR = '# SCLK #'
      CALL REPMC ( TIMSTR, '#', CLKUDF, TIMSTR )
      CALL REPMC ( TIMSTR, '#', CLKSTR, TIMSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Convert the time string.
!
      CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the error flag.
!
      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

!
!     Check the error message.
!
      XMSG = 'Time conversion failed; SCLK type <#> ' // &
             'was not recognized.'

      CALL REPMC ( XMSG, '#', CLKUDF, XMSG )

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )



!
! --- Case 16
!
!
!     Now attempt conversion using an SCLK string with no clock name.
!
      CALL TCASE ( 'Error: SCLK string lacks SCLK name.' )

      TIMSTR = ' SCLK #'
      CALL REPMC ( TIMSTR, '#', CLKSTR, TIMSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Convert the time string.
!
      CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the error flag.
!
      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

!
!     Check the error message.
!
      XMSG = 'Time conversion failed; SCLK name ' // &
             'was not supplied.'

      CALL REPMC ( XMSG, '#', CLKUDF, XMSG )

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )

!
! --- Case 17
!
!
!     Try the conversion without an SCLK kernel loaded.
!
      CALL TCASE ( 'Error: SCLK kernel is not loaded.' )

      CALL ZZEKTCNV ( 'GLL SCLK 1/1000:00:0:0', ET, ERROR, ERRMSG )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


!
! --- Case 18
!
!
!     Try the conversion using a string having invalid syntax.
!
      CALL TCASE ( 'SCLK string has invalid syntax.' )

      TIMSTR = '# SCLK #'
      CALL REPMC ( TIMSTR, '#', CLKNM2, TIMSTR )
      CALL REPMC ( TIMSTR, '#', BADCLK, TIMSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the error flag.
!
      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

!
!     The error message is going to be complex; don't attempt
!     to match it.  Just make sure the error message is non-blank.
!
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '!=', ' ', OK )



!
! --- Case 19
!
!
!     Try a conversion without having SCLK coefficients loaded. This is
!     intended to trigger an SCS2E SPICE error.
!
      CALL TCASE ( 'SCLK coefficients are not loaded.' )

      CALL DVPOOL ( 'SCLK01_COEFFICIENTS_9' )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TIMSTR = '# SCLK #'
      CALL REPMC ( TIMSTR, '#', CLKNAM, TIMSTR )
      CALL REPMC ( TIMSTR, '#', CLKSTR, TIMSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Convert the time string.
!
      CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


!
! --- Case 20
!
      CALL TCASE ( 'Conversion using a normal UTC string.' )


!
!     Create an SCLK kernel.  The routine we use for this purpose also
!     creates a C-kernel, which we don't need.
!
      CALL TSTCK3 ( CK, SCLK, .FALSE., .TRUE., .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Associate the name and ID of the clock.
!
      CALL BODDEF ( CLKNAM, SCLKID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCS2E  ( SCLKID, CLKSTR, XET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TIMSTR =  UTCSTR
!
!     Convert the UTC string to ET; make sure we get the same result
!     returned by STR2ET.
!
      CALL STR2ET ( TIMSTR, XET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Convert the time string.
!
      CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the error flag.
!
      CALL CHCKSL ( 'ERROR', ERROR, .FALSE., OK )

!
!     Check the error message.
!
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', ' ', OK   )

!
!     Check the time.
!
      CALL CHCKSD ( 'ET', ET, '=', XET, 0.D0, OK )



!
! --- Case 21
!
      CALL TCASE ( 'Conversion using an invalid UTC string.' )


      TIMSTR =  BADUTC

!
!     Convert the time string.
!
      CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the error flag.
!
      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

!
!     The error message is going to be complex; don't attempt
!     to match it.  Just make sure the error message is non-blank.
!
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '!=', ' ', OK )


!
! --- Case 22
!
!
!     Unload the EKs.  The TSPICE system will delete the EK1 file.
!
      CALL TCASE  ( 'Unload EKs from query system.' )

      CALL EKUEF ( EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EKUEF ( EK2HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Delete the EK2 file.
!
      CALL DELFIL ( EK2 )

!
!     EK1 wil be deleted by the test system.
!

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
