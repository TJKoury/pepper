C$Procedure F_NNEK04 ( EK tests, subset 4 )
 
      SUBROUTINE F_NNEK04 ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB EK routines, subset 4. This routine
C     operates on EK files having non-native binary format.
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
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'eksegdsc.inc'
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
C     This routine tests the EK query constraint checking routine 
C   
C        ZZEKSCMP
C
C     This routine operates on non-native EK files.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     This test family must be extended to comprehensively test its
C     target source code.
C
C     Full tests must include
C
C        Test every combination of column type and data type.
C        Test every combination of null and null-null data.
C        Test use of vector column entries.
C        Test use of segments from different files.
C
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 12-FEB-2015 (NJB)
C
C        Based on F_EK04 TSPICE Version 1.0.0, 29-MAY-2010 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               ZZEKSCMP

C
C     Local Parameters
C
      CHARACTER*(*)         EK1
      PARAMETER           ( EK1    = 'test1nn.ek' )

      CHARACTER*(*)         EK1NAT
      PARAMETER           ( EK1NAT = 'test1.ek' )

      INTEGER               DECLEN
      PARAMETER           ( DECLEN = 200 )

      INTEGER               MAXROW
      PARAMETER           ( MAXROW = 20 )

      INTEGER               CVALSZ
      PARAMETER           ( CVALSZ = 2*MAXSTR )

C
C     Local Variables
C
      CHARACTER*(CVALSZ)    CDATA  ( MAXROW )
      CHARACTER*(CVALSZ)    CVAL 
      CHARACTER*(CNAMSZ)    CNAMES ( MXCLSG )
      CHARACTER*(DECLEN)    DECLS  ( MXCLSG )
      CHARACTER*(TNAMSZ)    TABNAM

      DOUBLE PRECISION      DDATA ( MAXROW )
      DOUBLE PRECISION      DVAL
      DOUBLE PRECISION      TDATA ( MAXROW )

      INTEGER               CLDSC1 ( CDSCSZ )
      INTEGER               CLDSC3 ( CDSCSZ )
      INTEGER               CLDSC4 ( CDSCSZ )
      INTEGER               CLDSC5 ( CDSCSZ )
      INTEGER               DTYPE
      INTEGER               ELTIDX
      INTEGER               ENTSZS ( MAXROW )
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IDATA  ( MAXROW )
      INTEGER               IVAL
      INTEGER               NATBFF
      INTEGER               NCOLS
      INTEGER               NNBFF
      INTEGER               NROWS
      INTEGER               RCPTRS ( MAXROW )
      INTEGER               RECPTR
      INTEGER               ROW
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               SEGNO
      INTEGER               WKINDX ( MAXROW )

      LOGICAL               MATCH
      LOGICAL               NLFLGS ( MAXROW )
      LOGICAL               NULL

C
C     Saved variables
C      
C
C     To avoid portability problems, make all data static.
C
      SAVE


C
C     Open the test family.
C
      CALL TOPEN ( 'F_NNEK04' )

C
C --- Case: ------------------------------------------------------
C
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
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup:  create a custom EK.' )
C
C     We need an EK having columns containing scalar strings
C     longer than MAXSTR.
C
C     Open a new EK.
C
      CALL EKOPN ( EK1NAT, EK1NAT, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Since many EK routines don't return upon entry,
C     exit here if we couldn't open the file.
C
      IF ( .NOT. OK ) THEN
         CALL T_SUCCESS( OK )
         RETURN
      END IF
 

C
C     Start building a segment by creating a schema for an EK table.
C
      TABNAM    = 'TAB1'

      NCOLS     = 5
      NROWS     = 5

      CNAMES(1) = 'CCOL1'
      CNAMES(2) = 'CCOL2'
      CNAMES(3) = 'ICOL1'
      CNAMES(4) = 'DCOL1'
      CNAMES(5) = 'TCOL1'
      


      DECLS(1)  = 'DATATYPE = CHARACTER*(*), '
     .//          'INDEXED  = TRUE, '
     .//          'NULLS_OK = TRUE'

      DECLS(2)  = DECLS(1)

      DECLS(3)  = 'DATATYPE = INTEGER, '
     .//          'INDEXED  = TRUE, '
     .//          'NULLS_OK = TRUE'

      DECLS(4)  = 'DATATYPE = DOUBLE PRECISION, '
     .//          'INDEXED  = TRUE, '
     .//          'NULLS_OK = TRUE'

      DECLS(5)  = 'DATATYPE = TIME, '
     .//          'INDEXED  = TRUE, '
     .//          'NULLS_OK = TRUE'

C
C     Initiate a fast load.
C
      CALL EKIFLD ( HANDLE, TABNAM, NCOLS,  NROWS,
     .              CNAMES, DECLS,  SEGNO,  RCPTRS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the data for the first column. We need strings
C     longer than MAXSTR, and we want both strings that
C     are identical up to their first MAXSTR characters, and
C     strings that differ in their first MAXSTR characters.
C
      DO I = 1, NROWS
         CDATA(I) = ' '
      END DO

      DO I = 1, MAXSTR-1
         
         CDATA(1)(I:I) = 'X'
         CDATA(2)(I:I) = 'X'
         CDATA(3)(I:I) = 'X'
         CDATA(4)(I:I) = 'X'
         
      END DO

      CDATA(1) (MAXSTR:MAXSTR+2) = 'ZAB'
      CDATA(2) (MAXSTR:MAXSTR+2) = 'ZAC'
      CDATA(3) (MAXSTR:MAXSTR+2) = 'WAB'
      CDATA(4) (MAXSTR:MAXSTR+2) = 'VAB'
      CDATA(5) (MAXSTR:MAXSTR+2) = 'VAB'

C
C     Add the first column to the segment.
C
      CALL FILLI ( 1, MAXROW, ENTSZS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, MAXROW
         NLFLGS(I) = .FALSE.
      END DO
         
C     
C     Make the last entry null.
C
      NLFLGS(NROWS) = .TRUE.

      CALL EKACLC ( HANDLE, SEGNO,  CNAMES(1), CDATA,
     .              ENTSZS, NLFLGS, RCPTRS,    WKINDX )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Give the second column the same data as the first.
C
      CALL EKACLC ( HANDLE, SEGNO,  CNAMES(2), CDATA,
     .              ENTSZS, NLFLGS, RCPTRS,    WKINDX )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create data for and load the integer column.
C
      DO I = 1, NROWS
         IDATA(I) = 100 * I
      END DO

      CALL EKACLI ( HANDLE, SEGNO,  CNAMES(3), IDATA,
     .              ENTSZS, NLFLGS, RCPTRS,    WKINDX )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create data for and load the d.p. column.
C
      DO I = 1, NROWS
         DDATA(I) = DBLE( 100 * I )
      END DO

      CALL EKACLD ( HANDLE, SEGNO,  CNAMES(4), DDATA,
     .              ENTSZS, NLFLGS, RCPTRS,    WKINDX )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create data for and load the time column.
C
      DO I = 1, NROWS
         TDATA(I) = I * 86400.D0
      END DO

      CALL EKACLD ( HANDLE, SEGNO,  CNAMES(5), TDATA,
     .              ENTSZS, NLFLGS, RCPTRS,    WKINDX )

      CALL CHCKXC ( .FALSE., ' ', OK )



C
C     Now that we've finished adding columns to the segment,
C     finish the load.
C
      CALL EKFFLD ( HANDLE, SEGNO, RCPTRS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the EK.
C
      CALL EKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create non-native EK from native version.' )

C
C     Replace the EK with a non-native version.
C
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
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Load non-native EK for read access using EKLEF.' )

C
C     Load the EK for read access. We'll use the handle, so
C     it's easier to use the load entry point from the query
C     manager.
C
      CALL EKLEF  ( EK1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C

C
C     Test operation of ZZEKSCMP on long strings.
C 
      CALL TCASE ( 'ZZEKSCMP: compare strings that are identical up '
     .//           'to length MAXSTR.'                               )

C
C     Get the segment descriptor for the first segment of EK1.
C
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the column descriptor for the first column.
C
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(1), CLDSC1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Compare the first entry of column 1 to the second entry.
C     Since these entries agree up to the first MAXSTR characters,
C     we expect equality.
C
      ELTIDX = 1
      DTYPE  = CHR
      CVAL   = CDATA(2)
      DVAL   = 0.D0
      IVAL   = 0
      NULL   = .FALSE.
      ROW    = 1
C
C     For class three columns, the "row" input required by ZZEKSCMP
C     is actually a pointer to the base of a data structure called an
C     "EK record pointer." (See usage in ZZEKRD03 for an example;
C     also see ekrecptr.inc.) We need to obtain this record pointer
C     for the row of interest: row 1.
C
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )
      
      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (0)', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (1)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (0)', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (1)', MATCH, .FALSE., OK )




      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .TRUE., OK )



C
C --- Case: ------------------------------------------------------
C

C
C     Test operation of ZZEKSCMP on long strings.
C 
      CALL TCASE ( 'ZZEKSCMP: compare a string element than is '
     .//           'less than a given scalar value.'             )

C
C     Get the segment descriptor for the first segment of EK1.
C
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the column descriptor for the first column.
C
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(1), CLDSC1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Compare the first entry of column 1 to the second entry.
C     Since these entries agree up to the first MAXSTR characters,
C     we expect equality.
C
      ELTIDX = 1
      DTYPE  = CHR
      CVAL   = CDATA(1)
      DVAL   = 0.D0
      IVAL   = 0
      NULL   = .FALSE.
      ROW    = 4
C
C     For class three columns, the "row" input required by ZZEKSCMP
C     is actually a pointer to the base of a data structure called an
C     "EK record pointer." (See usage in ZZEKRD03 for an example;
C     also see ekrecptr.inc.) We need to obtain this record pointer
C     for the row of interest: row 4.
C
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )
      
      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*XV',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (0)', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (1)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (0)', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*XV',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (1)', MATCH, .FALSE., OK )




      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .TRUE., OK )



C
C --- Case: ------------------------------------------------------
C

C
C     Test operation of ZZEKSCMP on long strings.
C 
      CALL TCASE ( 'ZZEKSCMP: compare a string element than is '
     .//           'greater than a given scalar value.'           )

C
C     Get the segment descriptor for the first segment of EK1.
C
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the column descriptor for the first column.
C
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(1), CLDSC1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Compare the first entry of column 1 to the second entry.
C     Since these entries agree up to the first MAXSTR characters,
C     we expect equality.
C
      ELTIDX = 1
      DTYPE  = CHR
      CVAL   = CDATA(4)
      DVAL   = 0.D0
      IVAL   = 0
      NULL   = .FALSE.
      ROW    = 1
C
C     For class three columns, the "row" input required by ZZEKSCMP
C     is actually a pointer to the base of a data structure called an
C     "EK record pointer." (See usage in ZZEKRD03 for an example;
C     also see ekrecptr.inc.) We need to obtain this record pointer
C     for the row of interest: row 1.
C
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )
      
      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .TRUE., OK )



      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (0)', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (1)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (0)', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (1)', MATCH, .FALSE., OK )




      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .TRUE., OK )


C
C --- Case: ------------------------------------------------------
C

C
C     Test operation of ZZEKSCMP on long strings.
C 
      CALL TCASE ( 'ZZEKSCMP: compare a non-null string element to a '
     .//           'null value.'                                      )

C
C     Get the segment descriptor for the first segment of EK1.
C
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the column descriptor for the first column.
C
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(1), CLDSC1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Compare the first entry of column 1 to the second entry.
C     Since these entries agree up to the first MAXSTR characters,
C     we expect equality.
C
      ELTIDX = 1
      DTYPE  = CHR
      CVAL   = CDATA(4)
      DVAL   = 0.D0
      IVAL   = 0
      NULL   = .TRUE.
      ROW    = 1
C
C     For class three columns, the "row" input required by ZZEKSCMP
C     is actually a pointer to the base of a data structure called an
C     "EK record pointer." (See usage in ZZEKRD03 for an example;
C     also see ekrecptr.inc.) We need to obtain this record pointer
C     for the row of interest: row 1.
C
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )
      
      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .TRUE., OK )



      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (0)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (1)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (0)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (1)', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .TRUE., OK )




C
C --- Case: ------------------------------------------------------
C

C
C     Test operation of ZZEKSCMP on long strings.
C 
      CALL TCASE ( 'ZZEKSCMP: compare a null string element to a '
     .//           'null value.'                                 )

C
C     Get the segment descriptor for the first segment of EK1.
C
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the column descriptor for the first column.
C
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(1), CLDSC1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Compare the first entry of column 1 to the second entry.
C     Since these entries agree up to the first MAXSTR characters,
C     we expect equality.
C
      ELTIDX = 1
      DTYPE  = CHR
      CVAL   = CDATA(5)
      DVAL   = 0.D0
      IVAL   = 0
      NULL   = .TRUE.
      ROW    = 5
C
C     For class three columns, the "row" input required by ZZEKSCMP
C     is actually a pointer to the base of a data structure called an
C     "EK record pointer." (See usage in ZZEKRD03 for an example;
C     also see ekrecptr.inc.) We need to obtain this record pointer
C     for the row of interest: row 5.
C
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )
      
      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (0)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (1)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (0)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (1)', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .FALSE., OK )




C
C --- Case: ------------------------------------------------------
C

C
C     Test operation of ZZEKSCMP on long strings.
C 
      CALL TCASE ( 'ZZEKSCMP: compare a null string element to a '
     .//           'non-null value.'                               )

C
C     Get the segment descriptor for the first segment of EK1.
C
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the column descriptor for the first column.
C
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(1), CLDSC1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Compare the first entry of column 1 to the second entry.
C     Since these entries agree up to the first MAXSTR characters,
C     we expect equality.
C
      ELTIDX = 1
      DTYPE  = CHR
      CVAL   = CDATA(4)
      DVAL   = 0.D0
      IVAL   = 0
      NULL   = .FALSE.
      ROW    = 5
C
C     For class three columns, the "row" input required by ZZEKSCMP
C     is actually a pointer to the base of a data structure called an
C     "EK record pointer." (See usage in ZZEKRD03 for an example;
C     also see ekrecptr.inc.) We need to obtain this record pointer
C     for the row of interest: row 5.
C
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )
      
      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (0)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (1)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (0)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (1)', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC1, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .FALSE., OK )








C
C --- Case: ------------------------------------------------------
C

C
C     Test operation of ZZEKSCMP on integers.
C 
      CALL TCASE ( 'ZZEKSCMP: compare matching integers.' )

C
C     Get the segment descriptor for the first segment of EK1.
C
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the column descriptor for the third column.
C
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(3), CLDSC3 )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Compare the first entry of column 1 to the first entry.
C     We expect equality.
C
      ELTIDX = 1
      DTYPE  = INT
      CVAL   = ' '
      DVAL   = 0.D0
      IVAL   = IDATA(1)
      NULL   = .FALSE.
      ROW    = 1
C
C     For class one columns, the "row" input required by ZZEKSCMP
C     is actually a pointer to the base of a data structure called an
C     "EK record pointer." (See usage in ZZEKRD01 for an example;
C     also see ekrecptr.inc.) We need to obtain this record pointer
C     for the row of interest: row 1.
C
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )
      
      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC3, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC3, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC3, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC3, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC3, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .TRUE., OK )






C
C --- Case: ------------------------------------------------------
C

C
C     Test operation of ZZEKSCMP on d.p numbers.
C 
      CALL TCASE ( 'ZZEKSCMP: compare matching d.p. numbers.' )

C
C     Get the segment descriptor for the first segment of EK1.
C
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the column descriptor for the fourth column.
C
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(4), CLDSC4 )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Compare the first entry of column 4 to the first entry.
C     We expect equality.
C
      ELTIDX = 1
      DTYPE  = DP
      CVAL   = ' '
      DVAL   = DDATA(1)
      IVAL   = 0
      NULL   = .FALSE.
      ROW    = 1
C
C     For class two columns, the "row" input required by ZZEKSCMP
C     is actually a pointer to the base of a data structure called an
C     "EK record pointer." (See usage in ZZEKRD02 for an example;
C     also see ekrecptr.inc.) We need to obtain this record pointer
C     for the row of interest: row 1.
C
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )
      
      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC4, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC4, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC4, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC4, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC4, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .TRUE., OK )




C --- Case: ------------------------------------------------------
C

C
C     Test operation of ZZEKSCMP on times.
C 
      CALL TCASE ( 'ZZEKSCMP: compare matching times.' )

C
C     Get the segment descriptor for the first segment of EK1.
C
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the column descriptor for the fifth column.
C
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(5), CLDSC5 )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Compare the first entry of column 5 to the first entry.
C     We expect equality.
C
      ELTIDX = 1
      DTYPE  = TIME
      CVAL   = ' '
      DVAL   = TDATA(1)
      IVAL   = 0
      NULL   = .FALSE.
      ROW    = 1
C
C     For class two columns, the "row" input required by ZZEKSCMP
C     is actually a pointer to the base of a data structure called an
C     "EK record pointer." (See usage in ZZEKRD02 for an example;
C     also see ekrecptr.inc.) We need to obtain this record pointer
C     for the row of interest: row 1.
C
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )
      
      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC5, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC5, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC5, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC5, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC5, RECPTR,  
     .                   ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .TRUE., OK )



C
C --- Case: ------------------------------------------------------
C

C
C     Unload the EKs and delete the files.
C
      CALL TCASE  ( 'Unload EKs from query system.' )

      CALL EKUEF  ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( EK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


