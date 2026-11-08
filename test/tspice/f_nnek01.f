C$Procedure F_NNEK01 ( EK test, subset 1, non-native )
 
      SUBROUTINE F_NNEK01 ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB EK routines, subset 1. Use EKs having
C     non-native binary formats.
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
C     TEST FAMILY
C
C$ Declarations
 
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
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine performs the same tests as F_EK01, but the file
C     EK1 is converted to non-native binary format before use in this
C     routine.
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
C     N.J. Bachman     (JPL)
C     E.D. Wright      (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 11-FEB-2015 (NJB)
C
C        Based on TSPICE Version 1.1.0, 18-NOV-2010 (EDW)
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               EKNSEG
      INTEGER               LASTNB

      LOGICAL               EXISTS

C
C     Local Parameters
C
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

C
C     Local Variables
C
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

C
C     Saved all.
C     
      SAVE 

C
C     Initial values
C
      DATA                 CHTYPS / 'CHR', 'DP', 'INT', 'TIME' /

      DATA                 TABLES / 'SCALAR_1',
     .                              'SCALAR_2',
     .                              'SCALAR_3',
     .                              'SCALAR_4',
     .                              'VECTOR_1',
     .                              'VECTOR_2'  /

C
C     Open the test family.
C
      CALL TOPEN ( 'F_NNEK01' )


C
C --- Setup
C
      CALL TCASE ( 'Set native and non-native binary file format '
     .//           'codes.'                                          )


      CALL ZZDDHNFC ( NATBFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( NATBFF .EQ. LTLI3E ) THEN

         NNBFF = BIGI3E
      ELSE
         NNBFF = LTLI3E
      END IF

C
C --- Case 1
C
      CALL TCASE ( 'Test the EK writing and fast load routines:  '    //
     .             'EKOPN, EKIFLD, EKACLC, EKACLD, EKACLI, '          //
     .             'EKFFLD, EKCLS.  Also test EKSSUM.  All of '       //
     .             'this is done by TSTEK, which is called here.'     )

C
C     Create an EK. Convert to non-native format.
C
      FILENO = 1

      CALL TSTEK  ( EK1NAT, FILENO, MAXROW, .FALSE., EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_BINGO ( EK1NAT, EK1, NNBFF )
      CALL CHCKXC ( .FALSE., ' ', OK    )

      CALL DELFIL ( EK1NAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Register the non-native EK with the FILREG so that it can be
C     removed when the current test family is done with its
C     task.
C
      CALL TFILES ( EK1 )

      
C
C --- Case 2
C
      CALL TCASE ( 'Test EKOPR, EKSSUM and EKNSEG.  Get segment ' //
     .             'summaries and make sure they''re compatible ' //
     .             'with the schemas returned by TSTSCH.'         )

C
C     Find out how many segments are in the EK.  By the specification
C     of TSTEK, there's one segment per table.
C
      CALL EKOPR  ( EK1, EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )


      NSEG = EKNSEG ( EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO SEGNO = 1, NSEG
C
C        Start a new test case.
C
         MSG = 'Testing EKTNAM, EKCCNT, EKCII for segment #'
         CALL REPMI ( MSG, '#', SEGNO, MSG )


C
C --- Case 3
C
         CALL TCASE ( MSG )

C
C        Get the summary for this segment.
C
C         WRITE (*,*) 'SEGNO  = ', SEGNO
C         WRITE (*,*) 'EK1HAN = ', EK1HAN


         CALL EKSSUM ( EK1HAN,  SEGNO,   SSTNAM,  SSNROW,
     .                 SSNCOL,  SSCNMS,  SSTYPS,  SSDIMS,
     .                 SSLENS,  SSIDXD,  SSNLOK          )

         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Compare the attributes given by the segment summary to those
C        returned by TSTSCH.  These are:
C
C           - table name
C           - column count
C           - row count
C           - column names
C           - column descriptors
C        
C        For each column descriptor, compare the attributes:
C
C           - data type 
C           - string length
C           - size
C           - is the column indexed?
C           - does the column allow null values?
C
C
C        Look up the schema for this table.
C
         TABNAM = TABLES(SEGNO)

         CALL TSTSCH ( TABNAM, MAXROW, SEGTYP, NROWS,  NCOLS,
     .                 CNAMES, CCLASS, DTYPES, STLENS, DIMS,
     .                 INDEXD, NULLOK, DECLS                   )

         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check the table name.
C
         CALL CHCKSC ( 'SSTNAM', SSTNAM, '=', TABNAM, OK )


C
C        Check the row and column counts.
C
         CALL CHCKSI ( 'NROWS from EKSSUM', 
     .                  SSNROW, '=', NROWS, 0, OK )

         CALL CHCKSI ( 'NCOLS from EKSSUM', 
     .                  SSNCOL, '=', NCOLS, 0, OK )

C
C        For each column in the current table, check the column's
C        attributes.  The attribute block index parameters are defined
C        in the include file ekattdsc.inc.
C
         DO I = 1, NCOLS
C
C           Check the column name.
C
            CALL CHCKSC ( 'Column name', SSCNMS(I), '=', CNAMES(I), OK )

C
C           Check the current column's data type.
C
            CALL CHCKSC ( 'Column data type', 
     .                     SSTYPS(I), '=', CHTYPS(DTYPES(I)), OK )

C
C           If the data type is character, check the string length.
C           
            IF ( DTYPES(I) .EQ. CHR ) THEN

                CALL CHCKSI ( 'Column string length', 
     .                        SSLENS(I), '=', STLENS(I), 0, OK )
            END IF

C
C           Check the current column's entry size.
C
            CALL CHCKSI ( 'Column entry size', 
     .                     SSDIMS(I), '=', DIMS(I), 0, OK )

C
C           Check the current column's index flag.
C
            CALL CHCKSL ( 'Column index flag', 
     .                    SSIDXD(I), INDEXD(I), OK )

C
C           Check the current column's null ok flag.
C
            CALL CHCKSL ( 'Column null ok flag', 
     .                    SSNLOK(I), NULLOK(I), OK )

         END DO

C
C        We're done with the current column.
C        

      END DO
C
C     We're done with the current table.
C     

C
C     Close the EK.
C
      CALL EKCLS ( EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case 4
C
C
C
C     Load the EK into the query system.
C
      CALL TCASE ( 'Ah, the nitty gritty.  Test EKFIND, ENELT, and '//
     .             'the fetching triplets EKGC, EKGD, EKGI.'        ) 

      CALL EKLEF ( EK1, EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We start off with a simple case.
C     
      QUERY = 'select c_col_1, d_col_1, i_col_1, t_col_1 from ' //
     .        'scalar_2 order by row_no'


      CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
      CALL CHCKXC ( .FALSE., ' ', OK )
           
      CALL TSTMSG ( '#', 'The error message was:  #' )
      CALL CHCKXC ( .FALSE., ' ', OK )
   
      CALL CHCKSL ( 'ERROR flag', ERROR, .FALSE., OK )

C
C     The table 'SCALAR_2' occupies the second segment of the file
C     designated by EK1.  Segment numbers start at 1 and increment from
C     there.
C
      SEGNO = 2

      CALL EKSSUM ( EK1HAN,  SEGNO,   SSTNAM,  SSNROW,
     .              SSNCOL,  SSCNMS,  SSTYPS,  SSDIMS,
     .              SSLENS,  SSIDXD,  SSNLOK          )

      CALL CHCKXC ( .FALSE., ' ', OK )
     
      CALL CHCKSI ( 'NMROWS', 
     .               NMROWS, '=', SSNROW, 0, OK )


C
C     Check the data.
C
      DO ROWNO = 1, NMROWS
C
C        First, fetch and test the character data.
C
         SELIDX = 1
         ELTIDX = 1 

         CALL TSTMSG ( '#', 
     .                 'table = SCALAR_2; selidx = 1; col = c_col_1; '//
     .                 'row = #; eltidx = 1.'                         )

         CALL TSTMSI ( ROWNO )

C
C        Fetch the value for c_col_1 from the current row.
C
         CALL EKGC ( SELIDX, ROWNO, ELTIDX, CDATA, ISNULL, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C        Look up the expected column entry.
C
         CALL TSTENT ( FILENO,      'SCALAR_2',   SEGNO,  
     .                 'C_COL_1',   ROWNO,        MAXVAL,
     .                 XNELTS,      CVALS,        DVALS,    
     .                 IVALS,       TVALS,        XNULL  )  

         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check the null flag returned by EKGC.
C
         CALL CHCKSL ( 'ISNULL', ISNULL, XNULL, OK )

C
C        Check the number of elements in the entry.
C
         CALL EKNELT ( SELIDX, ROWNO, NELTS )
         CALL CHCKXC ( .FALSE., ' ', OK )
      
         CALL CHCKSI ( 'NELTS', NELTS, '=', XNELTS, 0, OK )

         
         IF ( .NOT. ISNULL ) THEN
C
C           Check the character string returned by EKGC.
C
            CALL CHCKSC ( 'char value from EKGC',
     .                    CDATA, '=', CVALS, OK   )

         END IF

C
C        Check the d.p. data next.
C
         SELIDX = 2
         ELTIDX = 1 

         CALL TSTMSG ( '#', 
     .                 'table = SCALAR_2; selidx = 2; col = d_col_1; '//
     .                 'row = #; eltidx = 1.'                         )

         CALL TSTMSI ( ROWNO )

C
C        Fetch the value for d_col_1 from the current row.
C
         CALL EKGD ( SELIDX, ROWNO, ELTIDX, DDATA, ISNULL, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C        Look up the expected column entry.
C
         CALL TSTENT ( FILENO,      'SCALAR_2',   SEGNO,  
     .                 'D_COL_1',   ROWNO,        MAXVAL,
     .                 XNELTS,      CVALS,        DVALS,    
     .                 IVALS,       TVALS,        XNULL  )  

         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check the null flag returned by EKGD.
C
         CALL CHCKSL ( 'ISNULL', ISNULL, XNULL, OK )

C
C        Check the number of elements in the entry.
C
         CALL EKNELT ( SELIDX, ROWNO, NELTS )
         CALL CHCKXC ( .FALSE., ' ', OK )
      
         CALL CHCKSI ( 'NELTS', NELTS, '=', XNELTS, 0, OK )

         
         IF ( .NOT. ISNULL ) THEN
C
C           Check the d.p. value returned by EKGD.
C
            CALL CHCKSD ( 'D.P. value from EKGD',
     .                    DDATA, '=', DVALS, 0.D0, OK   )

         END IF

C
C        Check the integer data.
C
         SELIDX = 3
         ELTIDX = 1 

         CALL TSTMSG ( '#', 
     .                 'table = SCALAR_2; selidx = 3; col = i_col_1; '//
     .                 'row = #; eltidx = 1.'                         )

         CALL TSTMSI ( ROWNO )

C
C        Fetch the value for i_col_1 from the current row.
C
         CALL EKGI ( SELIDX, ROWNO, ELTIDX, IDATA, ISNULL, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C        Look up the expected column entry.
C
         CALL TSTENT ( FILENO,      'SCALAR_2',   SEGNO,  
     .                 'I_COL_1',   ROWNO,        MAXVAL,
     .                 XNELTS,      CVALS,        DVALS,    
     .                 IVALS,       TVALS,        XNULL  )  

         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check the null flag returned by EKGI.
C
         CALL CHCKSL ( 'ISNULL', ISNULL, XNULL, OK )

C
C        Check the number of elements in the entry.
C
         CALL EKNELT ( SELIDX, ROWNO, NELTS )
         CALL CHCKXC ( .FALSE., ' ', OK )
      
         CALL CHCKSI ( 'NELTS', NELTS, '=', XNELTS, 0, OK )

         
         IF ( .NOT. ISNULL ) THEN
C
C           Check the integer value returned by EKGI.
C
            CALL CHCKSI ( 'Integer value from EKGI',
     .                    IDATA, '=', IVALS, 0, OK   )

         END IF

C
C        Check the time data.
C
         SELIDX = 4
         ELTIDX = 1 

         CALL TSTMSG ( '#', 
     .                 'table = SCALAR_2; selidx = 4; col = t_col_1; '//
     .                 'row = #; eltidx = 1.'                         )

         CALL TSTMSI ( ROWNO )

C
C        Fetch the value for t_col_1 from the current row.
C
         CALL EKGD ( SELIDX, ROWNO, ELTIDX, TDATA, ISNULL, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C        Look up the expected column entry.
C
         CALL TSTENT ( FILENO,      'SCALAR_2',   SEGNO,  
     .                 'T_COL_1',   ROWNO,        MAXVAL,
     .                 XNELTS,      CVALS,        DVALS,    
     .                 IVALS,       TVALS,        XNULL  )  

         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check the null flag returned by EKGD.
C
         CALL CHCKSL ( 'ISNULL', ISNULL, XNULL, OK )

C
C        Check the number of elements in the entry.
C
         CALL EKNELT ( SELIDX, ROWNO, NELTS )
         CALL CHCKXC ( .FALSE., ' ', OK )
      
         CALL CHCKSI ( 'NELTS', NELTS, '=', XNELTS, 0, OK )

         
         IF ( .NOT. ISNULL ) THEN
C
C           Check the time value returned by EKGD.
C
            CALL CHCKSD ( 'Time value from EKGD',
     .                    DDATA, '=', TVALS, 0.D0, OK   )

         END IF

      END DO



C
C     Now for a more comprehensive test.
C
C
C --- Case 5
C
      CALL TCASE ( 'This time, we loop over all tables, and we ' //
     .             ' check all entries in each table.'           )

      DO TABNO = 1, NTABS
               
C
C        Get the row and column count for this table.
C
         SEGNO = TABNO

C
C        Get the summary for this segment.
C
         CALL EKSSUM ( EK1HAN,  SEGNO,   SSTNAM,  SSNROW,
     .                 SSNCOL,  SSCNMS,  SSTYPS,  SSDIMS,
     .                 SSLENS,  SSIDXD,  SSNLOK          )

         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Build the query string:  select all columns from the current
C        table.
C
         QUERY = 'SELECT'
         CALL SUFFIX ( SSCNMS(1), 1, QUERY )

         DO COLNO = 2, SSNCOL

            CALL SUFFIX ( ',',           0, QUERY )
            CALL SUFFIX ( SSCNMS(COLNO), 1, QUERY )

         END DO

         CALL SUFFIX ( 'FROM',            1, QUERY )
         CALL SUFFIX ( TABLES(TABNO),     1, QUERY )
         CALL SUFFIX ( 'ORDER BY ROW_NO', 1, QUERY )

C
C        Issue the query.
C
         CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Make sure there was no query resolution error.
C        
         CALL TSTMSG ( '#', 'The error message was:  #' )
         CALL TSTMSC ( ERRMSG )

         CALL CHCKSL ( 'ERROR flag', ERROR, .FALSE., OK )

C
C        Check NMROWS.
C
         CALL CHCKSI ( 'NMROWS', NMROWS, '=', SSNROW, 0, OK )

C
C        Check the data.
C
         NCOLS = SSNCOL

         DO ROWNO = 1, NMROWS


            DO SELIDX = 1, NCOLS
C
C              Get the name and data type of the current column;
C              process the column accordingly.
C
               COLUMN = SSCNMS(SELIDX)

               DTYPE  = SSTYPS(SELIDX)


               MSG =  '#' //
     .                'Table is #. Column is #. Row is #. ' //
     .                'Current select index is #.'         

               CALL REPMC ( MSG, '#', TABLES(TABNO), MSG )
               CALL REPMC ( MSG, '#', COLUMN,        MSG )
               CALL REPMI ( MSG, '#', ROWNO,         MSG )
               CALL REPMI ( MSG, '#', SELIDX,        MSG )

C
C --- Case 6
C
               CALL TCASE ( MSG )

C
C              Look up the expected column entry.
C
               CALL TSTENT ( FILENO,      TABLES(TABNO),   SEGNO,  
     .                       COLUMN,      ROWNO,           MAXVAL,
     .                       XNELTS,      CVALS,           DVALS,    
     .                       IVALS,       TVALS,           XNULL  )  

               CALL CHCKXC ( .FALSE., ' ', OK )


C
C              Check the number of elements in the entry.
C
               CALL EKNELT ( SELIDX, ROWNO, NELTS )
               CALL CHCKXC ( .FALSE., ' ', OK )
      
               CALL CHCKSI ( 'NELTS', NELTS, '=', XNELTS, 0, OK )


C
C              Check the data.
C
               DO ELTIDX = 1, NELTS

                  CALL TSTMSG ( '#', 
     .                          'Table is #. Column is #. Row is #. ' //
     .                          'Current element index is #.'         )
                  CALL TSTMSC ( TABLES(TABNO) )
                  CALL TSTMSC ( COLUMN        )
                  CALL TSTMSI ( ROWNO         )
                  CALL TSTMSI ( ELTIDX        )

                  
                  IF ( DTYPE .EQ. 'CHR' ) THEN

                     CALL EKGC ( SELIDX, ROWNO,  ELTIDX, 
     .                           CDATA,  ISNULL, FOUND  )

C
C                    Make sure no error was signaled.
C
                     CALL CHCKXC ( .FALSE., ' ', OK )

C
C                    Make sure the element was found. 
C
                     CALL CHCKSL ( 'FOUND flag', FOUND, .TRUE., OK )

C 
C                    Check the null flag returned by EKGC.
C
                     CALL CHCKSL ( 'NULL flag', ISNULL, XNULL, OK )

                     IF ( .NOT. ISNULL ) THEN
C
C                       Check the character string returned by EKGC.
C                       
                        CALL CHCKSC ( 'char value from EKGC',
     .                                CDATA, '=', CVALS(ELTIDX), OK )
                     END IF



                  ELSE IF ( DTYPE .EQ. 'DP' ) THEN

                     CALL EKGD ( SELIDX, ROWNO,  ELTIDX, 
     .                           DDATA,  ISNULL, FOUND  )

C
C                    Make sure no error was signaled.
C
                     CALL CHCKXC ( .FALSE., ' ', OK )

C
C                    Make sure the element was found. 
C
                     CALL CHCKSL ( 'FOUND flag', FOUND, .TRUE., OK )

C 
C                    Check the null flag returned by EKGD.
C
                     CALL CHCKSL ( 'NULL flag', ISNULL, XNULL, OK )

                     IF ( .NOT. ISNULL ) THEN
C
C                       Check the d.p. value returned by EKGD.
C                       
                        CALL CHCKSD ( 'd.p. value from EKGD',
     .                                DDATA, 
     .                                '=', 
     .                                DVALS(ELTIDX), 0.D0, OK )
                     END IF


                  ELSE IF ( DTYPE .EQ. 'INT' ) THEN

                     CALL EKGI ( SELIDX, ROWNO,  ELTIDX, 
     .                           IDATA,  ISNULL, FOUND  )

C
C                    Make sure no error was signaled.
C
                     CALL CHCKXC ( .FALSE., ' ', OK )

C
C                    Make sure the element was found. 
C
                     CALL CHCKSL ( 'FOUND flag', FOUND, .TRUE., OK )

C 
C                    Check the null flag returned by EKGI.
C
                     CALL CHCKSL ( 'NULL flag', ISNULL, XNULL, OK )

                     IF ( .NOT. ISNULL ) THEN
C
C                       Check the integer value returned by EKGI.
C                       
                        CALL CHCKSI ( 'Integer value from EKGI',
     .                                IDATA, 
     .                                '=', 
     .                                IVALS(ELTIDX), 0, OK )
                     END IF

                  ELSE  
C
C                    DTYPE == 'TIME'
C

                     CALL EKGD ( SELIDX, ROWNO,  ELTIDX, 
     .                           DDATA,  ISNULL, FOUND  )

C
C                    Make sure no error was signaled.
C
                     CALL CHCKXC ( .FALSE., ' ', OK )

C
C                    Make sure the element was found. 
C
                     CALL CHCKSL ( 'FOUND flag', FOUND, .TRUE., OK )

C 
C                    Check the null flag returned by EKGD.
C
                     CALL CHCKSL ( 'NULL flag', ISNULL, XNULL, OK )

                     IF ( .NOT. ISNULL ) THEN
C
C                       Check the time value returned by EKGD.
C                       
                        CALL CHCKSD ( 'Time value from EKGD',
     .                                DDATA, 
     .                                '=', 
     .                                TVALS(ELTIDX), 0.D0, OK )
                     END IF

                  END IF

C
C                 Done with the current element.
C
               END DO
C
C              Done with the current column.
C
            END DO
C
C           Done with the current row.
C
         END DO
C
C        Done with the current table.
C
      END DO
C
C     We've queried each EK table and checked all data returned by
C     the queries.
C



C
C --- Case 7
C

      CALL TCASE (  'Open a scratch EK.  Write to it.  Make sure ' //
     .              'the data''s there.  Close it.  Make sure it ' //
     .              'goes away.'                                   )


      CALL EKOPS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SHRTNM(1) =   'COL_1'
      SHRTDC(1) =   'DATATYPE = INTEGER'

      CALL EKIFLD ( HANDLE,
     .              'TABLE_1', 
     .              1,
     .              1,
     .              SHRTNM,
     .              SHRTDC,
     .              SEGNO,
     .              RCPTRS                )

      NLFLGS(1) =  .FALSE.
      IVALS (1) =  99

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add the column to the table.
C
      CALL EKACLI ( HANDLE,  SEGNO,   'COL_1',  IVALS,
     .              1,       NLFLGS,  RCPTRS,   WKINDX )

      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Finish the fast load for this table.
C
      CALL EKFFLD ( HANDLE, SEGNO, RCPTRS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the summary for the table.
C   
      CALL EKSSUM ( HANDLE,  SEGNO,   SSTNAM,  SSNROW,
     .              SSNCOL,  SSCNMS,  SSTYPS,  SSDIMS,
     .              SSLENS,  SSIDXD,  SSNLOK          )

      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Check the table and column name.  That's enough.
C
      CALL CHCKSC ( 'Table name',  SSTNAM,    '=', 'TABLE_1', OK )
      CALL CHCKSC ( 'Column name', SSCNMS(1), '=', 'COL_1',   OK )
                  
C
C     Close this file.
C
      CALL EKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
   
C
C     Is the file still there?  Shouldn't be.
C
      CALL DASHLU ( HANDLE, UNIT )
 
      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHHANDLE)', OK )
   


C
C --- Case 8
C
      CALL TCASE (  'Open an EK.  Write to it.  Make sure '  //
     .              'the data''s there.  Close it.  Open it '//
     .              'for appending. Write some more. Make '  //
     .              'sure the data''s there.'                )


      CALL TSTMSG ( '#', 'About to open EK file #.' )
      CALL TSTMSC ( EK2 )

      IF ( EXISTS(EK2) ) THEN

         CALL DELFIL ( EK2 )

      END IF


      CALL EKOPN ( EK2, EK2, 0, EK2HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SHRTNM(1) =   'COL_1'
      SHRTDC(1) =   'DATATYPE = INTEGER'

      CALL EKIFLD ( EK2HAN,
     .              'TABLE_1', 
     .              1,
     .              1,
     .              SHRTNM,
     .              SHRTDC,
     .              SEGNO,
     .              RCPTRS                )

      NLFLGS(1) =  .FALSE.
      IVALS (1) =  100

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add the column to the table.
C
      CALL EKACLI ( EK2HAN,  SEGNO,   'COL_1',  IVALS,
     .              1,       NLFLGS,  RCPTRS,   WKINDX )

      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Finish the fast load for this table.
C
      CALL EKFFLD ( EK2HAN, SEGNO, RCPTRS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the summary for the table.
C   
      CALL EKSSUM ( EK2HAN,  SEGNO,   SSTNAM,  SSNROW,
     .              SSNCOL,  SSCNMS,  SSTYPS,  SSDIMS,
     .              SSLENS,  SSIDXD,  SSNLOK          )

      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Check the table and column name.  That's enough.
C
      CALL CHCKSC ( 'Table name',  SSTNAM,    '=', 'TABLE_1', OK )
      CALL CHCKSC ( 'Column name', SSCNMS(1), '=', 'COL_1',   OK )
                  
C
C     Close this file.
C
      CALL EKCLS ( EK2HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )
   
C
C     Now open the file for write access and add more data.
C
      CALL TSTMSG ( '#', 'About to open EK file # for appending.' )
      CALL TSTMSC ( EK2 )

      CALL EKOPW  ( EK2, EK2HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SHRTNM(1) =   'COL_2'
      SHRTDC(1) =   'DATATYPE = INTEGER'

      CALL EKIFLD ( EK2HAN,
     .              'TABLE_2', 
     .              1,
     .              1,
     .              SHRTNM,
     .              SHRTDC, 
     .              SEGNO,
     .              RCPTRS                )

      NLFLGS(1) =  .FALSE.
      IVALS (1) =  200

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add the column to the table.
C
      CALL EKACLI ( EK2HAN,  SEGNO,   'COL_2',  IVALS,
     .              1,       NLFLGS,  RCPTRS,   WKINDX )

      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Finish the fast load for this table.
C
      CALL EKFFLD ( EK2HAN, SEGNO, RCPTRS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close this file.
C
      CALL EKCLS ( EK2HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Open the file for read access.
C
      CALL EKOPR  ( EK2,  EK2HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the summary for the table.
C   
      CALL EKSSUM ( EK2HAN,  SEGNO,   SSTNAM,  SSNROW,
     .              SSNCOL,  SSCNMS,  SSTYPS,  SSDIMS,
     .              SSLENS,  SSIDXD,  SSNLOK          )

      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Check the table and column name.  That's enough.
C
      CALL CHCKSC ( 'Table name',  SSTNAM,    '=', 'TABLE_2', OK )
      CALL CHCKSC ( 'Column name', SSCNMS(1), '=', 'COL_2',   OK )
                  
C
C     Close this file.
C
      CALL EKCLS ( EK2HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )




C
C
C --- Case 9
C
      CALL TCASE ( 'Test EKPSEL.  Build a query with a complex ' //
     .             'SELECT clause; tear it apart with EKPSEL. '  //
     .             'Verify that EKPSEL identifies the columns'' '//
     .             'attributes correctly.'                       )

C
C     Get segment summary for the SCALAR_2 table.
C
      CALL EKLEF ( EK1, EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEGNO = 2

      CALL EKSSUM ( EK1HAN,  SEGNO,   SSTNAM,  SSNROW,
     .              SSNCOL,  SSCNMS,  SSTYPS,  SSDIMS,
     .              SSLENS,  SSIDXD,  SSNLOK          )

      CALL CHCKXC ( .FALSE., ' ', OK )

      NCOLS = SSNCOL
 
C
C     Build the query string:  select all columns from the current
C     table.  Save the beginning and end locations of the column name
C     tokens.
C
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


C
C     Analyze the query.
C
      CALL EKPSEL ( QUERY, 
     .              N,       XBEGS,  XENDS,   XTYPES,
     .              XCLASS,  TABS,   CNAMES,  ERROR,   ERRMSG )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure query parsed correctly.
C
      CALL CHCKSL ( 'ERROR flag', ERROR, .FALSE., OK )

C
C     Make sure the error message is blank.
C
      CALL CHCKSC ( 'ERROR message', ERRMSG, '=', ' ', OK )

C
C     Check the number of SELECT expressions.
C
      CALL CHCKSI ( 'number of SELECT expressions N',
     .              N, '=', NCOLS, 0, OK              )


C
C     Check the expression bounds.
C
      CALL CHCKAI ( 'XBEGS', XBEGS, '=', XPBEGS, N, OK )
      CALL CHCKAI ( 'XENDS', XENDS, '=', XPENDS, N, OK )

C
C     For each selected column, check the 
C 
C        - Column name
C        - Table name
C        - Data type
C        - Expression class
C     
      DO I = 1, N

         CALL CHCKSC ( 'Column names', CNAMES(I), '=', SSCNMS(I), OK )
         CALL CHCKSC ( 'Table names',  TABS(I),   '=', SSTNAM,    OK )

         CALL CHCKSC ( 'Data types',   XTYPES(I), '=', SSTYPS(I), OK )
 
         CALL CHCKSC ( 'Expression class',   
     .                  XCLASS(I),  '=',  'COL',  OK )

      END DO

C
C     Unload the EK.
C     
      CALL EKUEF ( EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

   
   
C
C --- Case 10
C
      CALL TCASE ( 'Test sorting:  select rows from scalar_2, ' //
     .             'ordering by c_col_2 (ascending).'           )
C
C     Use EK1.
C
      CALL EKLEF  ( EK1, EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the query.
C
      QUERY = 'select c_col_2 from scalar_2 ' //
     .        'where '                        //
     .        '(c_col_2 between '             //
     .        '"SEG_2_C_COL_2_ROW_10_" '      //
     .        'and '                          //
     .        '"SEG_2_C_COL_2_ROW_19" ) '     //
     .        'or '                           // 
     .        'c_col_2 like "X*" '            //
     .        'order by c_col_2'            


C
C     Issue the query.
C
      CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure there was no query resolution error.
C        
      CALL TSTMSG ( '#', 'The error message was:  #' )
      CALL TSTMSC ( ERRMSG )

      CALL CHCKSL ( 'ERROR flag', ERROR, .FALSE., OK )

      IF ( .NOT. ERROR ) THEN
C
C        Check NMROWS.  We expect to find 5 rows.
C         
         XNROWS = 5

         CALL CHCKSI ( 'NMROWS', NMROWS, '=', XNROWS, 0, OK )

C
C        Check the data.
C
         DO ROWNO = 1, XNROWS
C
C           Look up the expected column entry. Skip  over null entries.
C
            FILENO = 1
            SEGNO  = 2
            SELIDX = 1
            ELTIDX = 1

            CALL TSTENT ( FILENO,      'SCALAR_2',        SEGNO,  
     .                    'C_COL_2',   10 + 2*(ROWNO-1),  MAXVAL,
     .                    XNELTS,      CVALS,             DVALS,    
     .                    IVALS,       TVALS,             XNULL  )  

            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Fetch the actual column entry from the current row.
C
            CALL EKGC ( SELIDX, ROWNO,  ELTIDX, 
     .                  CDATA,  ISNULL, FOUND  )

C
C           Make sure no error was signaled.
C
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Make sure the element was found. 
C
            CALL CHCKSL ( 'FOUND flag', FOUND, .TRUE., OK )

C
C           Check the null flag returned by EKGC.
C
            CALL CHCKSL ( 'ISNULL', ISNULL, XNULL, OK )

         
            IF ( .NOT. ISNULL ) THEN
C  
C              Check the character string returned by EKGC.
C
               CALL CHCKSC ( 'char value from EKGC',
     .                       CDATA, '=', CVALS(ELTIDX), OK   )

            END IF

         END DO

      END IF

C
C --- Case 11
C
      CALL TCASE ( 'Test sorting:  select rows from scalar_2, ' //
     .             'ordering by c_col_2 (descending).'          )

C
C     Do the same query, but sort in descending order.
C
C     Create the query.
C
      QUERY = 'select c_col_2 from scalar_2 ' //
     .        'where '                        //
     .        '(c_col_2 between '             //
     .        '"SEG_2_C_COL_2_ROW_10_" '      //
     .        'and '                          //
     .        '"SEG_2_C_COL_2_ROW_19" ) '     //
     .        'or '                           // 
     .        'c_col_2 like "X*" '            //
     .        'order by c_col_2 desc'            


C
C     Issue the query.
C
      CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure there was no query resolution error.
C        
      CALL TSTMSG ( '#', 'The error message was:  #' )
      CALL TSTMSC ( ERRMSG )

      CALL CHCKSL ( 'ERROR flag', ERROR, .FALSE., OK )

      IF ( .NOT. ERROR ) THEN
C
C        Check NMROWS.  We expect to find 5 rows.
C         
         XNROWS = 5

         CALL CHCKSI ( 'NMROWS', NMROWS, '=', XNROWS, 0, OK )

C
C        Check the data.
C
         DO ROWNO = 1, XNROWS
C
C           Look up the expected column entry. Skip  over null entries.
C
            FILENO = 1
            SEGNO  = 2
            SELIDX = 1
            ELTIDX = 1

            CALL TSTENT ( FILENO,      'SCALAR_2',        SEGNO,  
     .                    'C_COL_2',   18 - 2*(ROWNO-1),  MAXVAL,
     .                    XNELTS,      CVALS,             DVALS,    
     .                    IVALS,       TVALS,             XNULL  )  

            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Fetch the actual column entry from the current row.
C
            CALL EKGC ( SELIDX, ROWNO,  ELTIDX, 
     .                  CDATA,  ISNULL, FOUND  )

C
C           Make sure no error was signaled.
C
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Make sure the element was found. 
C
            CALL CHCKSL ( 'FOUND flag', FOUND, .TRUE., OK )

C
C           Check the null flag returned by EKGC.
C
            CALL CHCKSL ( 'ISNULL', ISNULL, XNULL, OK )

         
            IF ( .NOT. ISNULL ) THEN
C  
C              Check the character string returned by EKGC.
C
               CALL CHCKSC ( 'char value from EKGC',
     .                       CDATA, '=', CVALS(ELTIDX), OK   )

            END IF

         END DO

      END IF

C
C     Unload the EK.
C     
      CALL EKUEF ( EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case 12
C
      CALL TCASE ( 'Test EKUEF; make sure we don''t accumulate ' //
     .             'DAS links when we reload a file repeatedly.' )


      DO I = 1, 2*NLOAD

         CALL EKLEF  ( EK1, EK1HAN )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO
 
      CALL EKUEF  ( EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASFNH ( EK1, EK1HAN )
      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHFILE)', OK )



C
C     EK time parsing tests follow.
C
C
C --- Case 13
C
      CALL TCASE ( 'Test ZZEKTCNV; convert SCLK string.' )

C
C     Create and load a leapseconds kernel.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create an SCLK kernel.  The routine we use for this purpose also
C     creates a C-kernel, which we don't need.
C
      CALL TSTCK3 ( CK, SCLK, .FALSE., .TRUE., .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Associate the name and ID of the clock.
C     
      CALL BODDEF ( CLKNAM, SCLKID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Convert an SCLK string to ET; make sure we get the same result
C     returned by SCS2E.
C
      CALL SCS2E  ( SCLKID, CLKSTR, XET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TIMSTR = '# SCLK #'
      CALL REPMC ( TIMSTR, '#', CLKNAM, TIMSTR )
      CALL REPMC ( TIMSTR, '#', CLKSTR, TIMSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Convert the time string.
C
      CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the error flag.
C
      CALL CHCKSL ( 'ERROR', ERROR, .FALSE., OK )

C
C     Check the error message.
C
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', ' ', OK   )

C
C     Check the time.
C
      CALL CHCKSD ( 'ET', ET, '=', XET, 0.D0, OK )


C
C --- Case 14
C
C
C     Now use a name that doesn't contain the substring SCLK.
C
      CALL TCASE ( 'SCLK name doesn''t contain the substring SCLK.' )

      CALL BODDEF ( CLKNM2, SCLKID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCS2E  ( SCLKID, CLKSTR, XET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TIMSTR = '# SCLK #'
      CALL REPMC ( TIMSTR, '#', CLKNAM, TIMSTR )
      CALL REPMC ( TIMSTR, '#', CLKSTR, TIMSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Convert the time string.
C
      CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the error flag.
C
      CALL CHCKSL ( 'ERROR', ERROR, .FALSE., OK )

C
C     Check the error message.
C
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', ' ', OK   )

C
C     Check the time.
C
      CALL CHCKSD ( 'ET', ET, '=', XET, 0.D0, OK )




C
C --- Case 15
C
C
C     Now attempt conversion using an SCLK name that doesn't map to
C     an ID code.
C
      CALL TCASE ( 'Error: SCLK does not map to ID code.' )

      TIMSTR = '# SCLK #'
      CALL REPMC ( TIMSTR, '#', CLKUDF, TIMSTR )
      CALL REPMC ( TIMSTR, '#', CLKSTR, TIMSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Convert the time string.
C
      CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the error flag.
C
      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

C
C     Check the error message.
C
      XMSG = 'Time conversion failed; SCLK type <#> ' //
     .       'was not recognized.'

      CALL REPMC ( XMSG, '#', CLKUDF, XMSG )

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )



C
C --- Case 16
C
C
C     Now attempt conversion using an SCLK string with no clock name.
C
      CALL TCASE ( 'Error: SCLK string lacks SCLK name.' )

      TIMSTR = ' SCLK #'
      CALL REPMC ( TIMSTR, '#', CLKSTR, TIMSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Convert the time string.
C
      CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the error flag.
C
      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

C
C     Check the error message.
C
      XMSG = 'Time conversion failed; SCLK name ' //
     .       'was not supplied.'

      CALL REPMC ( XMSG, '#', CLKUDF, XMSG )

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )

C
C --- Case 17
C
C
C     Try the conversion without an SCLK kernel loaded.
C
      CALL TCASE ( 'Error: SCLK kernel is not loaded.' )

      CALL ZZEKTCNV ( 'GLL SCLK 1/1000:00:0:0', ET, ERROR, ERRMSG )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


C
C --- Case 18
C
C
C     Try the conversion using a string having invalid syntax. 
C
      CALL TCASE ( 'SCLK string has invalid syntax.' )

      TIMSTR = '# SCLK #'
      CALL REPMC ( TIMSTR, '#', CLKNM2, TIMSTR )
      CALL REPMC ( TIMSTR, '#', BADCLK, TIMSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the error flag.
C
      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

C
C     The error message is going to be complex; don't attempt
C     to match it.  Just make sure the error message is non-blank.
C
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '!=', ' ', OK )



C
C --- Case 19
C
C
C     Try a conversion without having SCLK coefficients loaded. This is
C     intended to trigger an SCS2E SPICE error.
C
      CALL TCASE ( 'SCLK coefficients are not loaded.' )

      CALL DVPOOL ( 'SCLK01_COEFFICIENTS_9' )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TIMSTR = '# SCLK #'
      CALL REPMC ( TIMSTR, '#', CLKNAM, TIMSTR )
      CALL REPMC ( TIMSTR, '#', CLKSTR, TIMSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Convert the time string.
C
      CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


C
C --- Case 20
C
      CALL TCASE ( 'Conversion using a normal UTC string.' )


C
C     Create an SCLK kernel.  The routine we use for this purpose also
C     creates a C-kernel, which we don't need.
C
      CALL TSTCK3 ( CK, SCLK, .FALSE., .TRUE., .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Associate the name and ID of the clock.
C     
      CALL BODDEF ( CLKNAM, SCLKID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCS2E  ( SCLKID, CLKSTR, XET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TIMSTR =  UTCSTR
C
C     Convert the UTC string to ET; make sure we get the same result
C     returned by STR2ET.
C
      CALL STR2ET ( TIMSTR, XET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Convert the time string.
C
      CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the error flag.
C
      CALL CHCKSL ( 'ERROR', ERROR, .FALSE., OK )

C
C     Check the error message.
C
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', ' ', OK   )

C
C     Check the time.
C
      CALL CHCKSD ( 'ET', ET, '=', XET, 0.D0, OK )



C
C --- Case 21
C
      CALL TCASE ( 'Conversion using an invalid UTC string.' )


      TIMSTR =  BADUTC

C
C     Convert the time string.
C
      CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the error flag.
C
      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

C
C     The error message is going to be complex; don't attempt
C     to match it.  Just make sure the error message is non-blank.
C
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '!=', ' ', OK )


C
C --- Case 22
C
C
C     Unload the EKs.  The TSPICE system will delete the EK1 file.
C
      CALL TCASE  ( 'Unload EKs from query system.' )

      CALL EKUEF ( EK1HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EKUEF ( EK2HAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Delete the EK2 file.
C
      CALL DELFIL ( EK2 )

C
C     EK1 wil be deleted by the test system.
C

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END
