!$Procedure F_EK04 ( EK tests, subset 4 )

      SUBROUTINE F_EK04 ( OK )

! $ Abstract
!
!     Exercise the SPICELIB EK routines, subset 4.
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
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektype.inc'
      INCLUDE 'errhnd.inc'

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
!     This routine tests the EK query constraint checking routine
!
!        ZZEKSCMP
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     This test family must be extended to comprehensively test its
!     target source code.
!
!     Full tests must include
!
!        Test every combination of column type and data type.
!        Test every combination of null and null-null data.
!        Test use of vector column entries.
!        Test use of segments from different files.
!
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 29-MAY-2010 (NJB)
!
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               ZZEKSCMP

!
!     Local Parameters
!
      CHARACTER*(*)         EK1
      PARAMETER           ( EK1    = 'test1.ek' )

!      CHARACTER*(*)         EK2
!      PARAMETER           ( EK2    = 'test2.ek' )

      INTEGER               DECLEN
      PARAMETER           ( DECLEN = 200 )

      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXENT
      PARAMETER           ( MAXENT = 20 )

      INTEGER               MAXROW
      PARAMETER           ( MAXROW = 20 )

      INTEGER               CVALSZ
      PARAMETER           ( CVALSZ = 2*MAXSTR )

      INTEGER               NTABS
      PARAMETER           ( NTABS  = 6 )

      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL = MAXENT )

!
!     Local Variables
!
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
      INTEGER               NCOLS
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

!
!     Saved variables
!
!
!     To avoid portability problems, make all data static.
!
      SAVE


!
!     Open the test family.
!
      CALL TOPEN ( 'F_EK04' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup:  create a custom EK.' )
!
!     We need an EK having columns containing scalar strings
!     longer than MAXSTR.
!
!     Open a new EK.
!
      CALL EKOPN ( EK1, EK1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Since many EK routines don't return upon entry,
!     exit here if we couldn't open the file.
!
      IF ( .NOT. OK ) THEN
         CALL T_SUCCESS( OK )
         RETURN
      END IF


!
!     Start building a segment by creating a schema for an EK table.
!
      TABNAM    = 'TAB1'

      NCOLS     = 5
      NROWS     = 5

      CNAMES(1) = 'CCOL1'
      CNAMES(2) = 'CCOL2'
      CNAMES(3) = 'ICOL1'
      CNAMES(4) = 'DCOL1'
      CNAMES(5) = 'TCOL1'



      DECLS(1)  = 'DATATYPE = CHARACTER*(*), ' &
      //          'INDEXED  = TRUE, ' &
      //          'NULLS_OK = TRUE'

      DECLS(2)  = DECLS(1)

      DECLS(3)  = 'DATATYPE = INTEGER, ' &
      //          'INDEXED  = TRUE, ' &
      //          'NULLS_OK = TRUE'

      DECLS(4)  = 'DATATYPE = DOUBLE PRECISION, ' &
      //          'INDEXED  = TRUE, ' &
      //          'NULLS_OK = TRUE'

      DECLS(5)  = 'DATATYPE = TIME, ' &
      //          'INDEXED  = TRUE, ' &
      //          'NULLS_OK = TRUE'

!
!     Initiate a fast load.
!
      CALL EKIFLD ( HANDLE, TABNAM, NCOLS,  NROWS, &
                    CNAMES, DECLS,  SEGNO,  RCPTRS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the data for the first column. We need strings
!     longer than MAXSTR, and we want both strings that
!     are identical up to their first MAXSTR characters, and
!     strings that differ in their first MAXSTR characters.
!
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

!
!     Add the first column to the segment.
!
      CALL FILLI ( 1, MAXROW, ENTSZS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, MAXROW
         NLFLGS(I) = .FALSE.
      END DO

!
!     Make the last entry null.
!
      NLFLGS(NROWS) = .TRUE.

      CALL EKACLC ( HANDLE, SEGNO,  CNAMES(1), CDATA, &
                    ENTSZS, NLFLGS, RCPTRS,    WKINDX )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Give the second column the same data as the first.
!
      CALL EKACLC ( HANDLE, SEGNO,  CNAMES(2), CDATA, &
                    ENTSZS, NLFLGS, RCPTRS,    WKINDX )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create data for and load the integer column.
!
      DO I = 1, NROWS
         IDATA(I) = 100 * I
      END DO

      CALL EKACLI ( HANDLE, SEGNO,  CNAMES(3), IDATA, &
                    ENTSZS, NLFLGS, RCPTRS,    WKINDX )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create data for and load the d.p. column.
!
      DO I = 1, NROWS
         DDATA(I) = DBLE( 100 * I )
      END DO

      CALL EKACLD ( HANDLE, SEGNO,  CNAMES(4), DDATA, &
                    ENTSZS, NLFLGS, RCPTRS,    WKINDX )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create data for and load the time column.
!
      DO I = 1, NROWS
         TDATA(I) = I * 86400.D0
      END DO

      CALL EKACLD ( HANDLE, SEGNO,  CNAMES(5), TDATA, &
                    ENTSZS, NLFLGS, RCPTRS,    WKINDX )

      CALL CHCKXC ( .FALSE., ' ', OK )



!
!     Now that we've finished adding columns to the segment,
!     finish the load.
!
      CALL EKFFLD ( HANDLE, SEGNO, RCPTRS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the EK.
!
      CALL EKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load the EK for read access. We'll use the handle, so
!     it's easier to use the load entry point from the query
!     manager.
!
      CALL EKLEF  ( EK1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!

!
!     Test operation of ZZEKSCMP on long strings.
!
      CALL TCASE ( 'ZZEKSCMP: compare strings that are identical up ' &
      //           'to length MAXSTR.'                               )

!
!     Get the segment descriptor for the first segment of EK1.
!
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the column descriptor for the first column.
!
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(1), CLDSC1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Compare the first entry of column 1 to the second entry.
!     Since these entries agree up to the first MAXSTR characters,
!     we expect equality.
!
      ELTIDX = 1
      DTYPE  = CHR
      CVAL   = CDATA(2)
      DVAL   = 0.D0
      IVAL   = 0
      NULL   = .FALSE.
      ROW    = 1
!
!     For class three columns, the "row" input required by ZZEKSCMP
!     is actually a pointer to the base of a data structure called an
!     "EK record pointer." (See usage in ZZEKRD03 for an example;
!     also see ekrecptr.inc.) We need to obtain this record pointer
!     for the row of interest: row 1.
!
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (0)', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (1)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (0)', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (1)', MATCH, .FALSE., OK )




      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .TRUE., OK )



!
! --- Case: ------------------------------------------------------
!

!
!     Test operation of ZZEKSCMP on long strings.
!
      CALL TCASE ( 'ZZEKSCMP: compare a string element than is ' &
      //           'less than a given scalar value.'             )

!
!     Get the segment descriptor for the first segment of EK1.
!
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the column descriptor for the first column.
!
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(1), CLDSC1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Compare the first entry of column 1 to the second entry.
!     Since these entries agree up to the first MAXSTR characters,
!     we expect equality.
!
      ELTIDX = 1
      DTYPE  = CHR
      CVAL   = CDATA(1)
      DVAL   = 0.D0
      IVAL   = 0
      NULL   = .FALSE.
      ROW    = 4
!
!     For class three columns, the "row" input required by ZZEKSCMP
!     is actually a pointer to the base of a data structure called an
!     "EK record pointer." (See usage in ZZEKRD03 for an example;
!     also see ekrecptr.inc.) We need to obtain this record pointer
!     for the row of interest: row 4.
!
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*XV',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (0)', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (1)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (0)', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*XV',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (1)', MATCH, .FALSE., OK )




      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .TRUE., OK )



!
! --- Case: ------------------------------------------------------
!

!
!     Test operation of ZZEKSCMP on long strings.
!
      CALL TCASE ( 'ZZEKSCMP: compare a string element than is ' &
      //           'greater than a given scalar value.'           )

!
!     Get the segment descriptor for the first segment of EK1.
!
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the column descriptor for the first column.
!
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(1), CLDSC1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Compare the first entry of column 1 to the second entry.
!     Since these entries agree up to the first MAXSTR characters,
!     we expect equality.
!
      ELTIDX = 1
      DTYPE  = CHR
      CVAL   = CDATA(4)
      DVAL   = 0.D0
      IVAL   = 0
      NULL   = .FALSE.
      ROW    = 1
!
!     For class three columns, the "row" input required by ZZEKSCMP
!     is actually a pointer to the base of a data structure called an
!     "EK record pointer." (See usage in ZZEKRD03 for an example;
!     also see ekrecptr.inc.) We need to obtain this record pointer
!     for the row of interest: row 1.
!
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .TRUE., OK )



      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (0)', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (1)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (0)', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (1)', MATCH, .FALSE., OK )




      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .TRUE., OK )


!
! --- Case: ------------------------------------------------------
!

!
!     Test operation of ZZEKSCMP on long strings.
!
      CALL TCASE ( 'ZZEKSCMP: compare a non-null string element to a ' &
      //           'null value.'                                      )

!
!     Get the segment descriptor for the first segment of EK1.
!
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the column descriptor for the first column.
!
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(1), CLDSC1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Compare the first entry of column 1 to the second entry.
!     Since these entries agree up to the first MAXSTR characters,
!     we expect equality.
!
      ELTIDX = 1
      DTYPE  = CHR
      CVAL   = CDATA(4)
      DVAL   = 0.D0
      IVAL   = 0
      NULL   = .TRUE.
      ROW    = 1
!
!     For class three columns, the "row" input required by ZZEKSCMP
!     is actually a pointer to the base of a data structure called an
!     "EK record pointer." (See usage in ZZEKRD03 for an example;
!     also see ekrecptr.inc.) We need to obtain this record pointer
!     for the row of interest: row 1.
!
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .TRUE., OK )



      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (0)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (1)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (0)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (1)', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .TRUE., OK )




!
! --- Case: ------------------------------------------------------
!

!
!     Test operation of ZZEKSCMP on long strings.
!
      CALL TCASE ( 'ZZEKSCMP: compare a null string element to a ' &
      //           'null value.'                                 )

!
!     Get the segment descriptor for the first segment of EK1.
!
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the column descriptor for the first column.
!
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(1), CLDSC1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Compare the first entry of column 1 to the second entry.
!     Since these entries agree up to the first MAXSTR characters,
!     we expect equality.
!
      ELTIDX = 1
      DTYPE  = CHR
      CVAL   = CDATA(5)
      DVAL   = 0.D0
      IVAL   = 0
      NULL   = .TRUE.
      ROW    = 5
!
!     For class three columns, the "row" input required by ZZEKSCMP
!     is actually a pointer to the base of a data structure called an
!     "EK record pointer." (See usage in ZZEKRD03 for an example;
!     also see ekrecptr.inc.) We need to obtain this record pointer
!     for the row of interest: row 5.
!
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (0)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (1)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (0)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (1)', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .FALSE., OK )




!
! --- Case: ------------------------------------------------------
!

!
!     Test operation of ZZEKSCMP on long strings.
!
      CALL TCASE ( 'ZZEKSCMP: compare a null string element to a ' &
      //           'non-null value.'                               )

!
!     Get the segment descriptor for the first segment of EK1.
!
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the column descriptor for the first column.
!
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(1), CLDSC1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Compare the first entry of column 1 to the second entry.
!     Since these entries agree up to the first MAXSTR characters,
!     we expect equality.
!
      ELTIDX = 1
      DTYPE  = CHR
      CVAL   = CDATA(4)
      DVAL   = 0.D0
      IVAL   = 0
      NULL   = .FALSE.
      ROW    = 5
!
!     For class three columns, the "row" input required by ZZEKSCMP
!     is actually a pointer to the base of a data structure called an
!     "EK record pointer." (See usage in ZZEKRD03 for an example;
!     also see ekrecptr.inc.) We need to obtain this record pointer
!     for the row of interest: row 5.
!
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (0)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( LIKE,   HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LIKE MATCH (1)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*X',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (0)', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( UNLIKE, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  '*XZ',  DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOT LIKE MATCH (1)', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC1, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .FALSE., OK )








!
! --- Case: ------------------------------------------------------
!

!
!     Test operation of ZZEKSCMP on integers.
!
      CALL TCASE ( 'ZZEKSCMP: compare matching integers.' )

!
!     Get the segment descriptor for the first segment of EK1.
!
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the column descriptor for the third column.
!
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(3), CLDSC3 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Compare the first entry of column 1 to the first entry.
!     We expect equality.
!
      ELTIDX = 1
      DTYPE  = INT
      CVAL   = ' '
      DVAL   = 0.D0
      IVAL   = IDATA(1)
      NULL   = .FALSE.
      ROW    = 1
!
!     For class one columns, the "row" input required by ZZEKSCMP
!     is actually a pointer to the base of a data structure called an
!     "EK record pointer." (See usage in ZZEKRD01 for an example;
!     also see ekrecptr.inc.) We need to obtain this record pointer
!     for the row of interest: row 1.
!
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC3, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC3, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC3, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC3, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC3, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .TRUE., OK )






!
! --- Case: ------------------------------------------------------
!

!
!     Test operation of ZZEKSCMP on d.p numbers.
!
      CALL TCASE ( 'ZZEKSCMP: compare matching d.p. numbers.' )

!
!     Get the segment descriptor for the first segment of EK1.
!
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the column descriptor for the fourth column.
!
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(4), CLDSC4 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Compare the first entry of column 4 to the first entry.
!     We expect equality.
!
      ELTIDX = 1
      DTYPE  = DP
      CVAL   = ' '
      DVAL   = DDATA(1)
      IVAL   = 0
      NULL   = .FALSE.
      ROW    = 1
!
!     For class two columns, the "row" input required by ZZEKSCMP
!     is actually a pointer to the base of a data structure called an
!     "EK record pointer." (See usage in ZZEKRD02 for an example;
!     also see ekrecptr.inc.) We need to obtain this record pointer
!     for the row of interest: row 1.
!
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC4, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC4, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC4, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC4, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC4, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .TRUE., OK )




! --- Case: ------------------------------------------------------
!

!
!     Test operation of ZZEKSCMP on times.
!
      CALL TCASE ( 'ZZEKSCMP: compare matching times.' )

!
!     Get the segment descriptor for the first segment of EK1.
!
      CALL ZZEKSDSC ( HANDLE, 1, SEGDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the column descriptor for the fifth column.
!
      CALL ZZEKCDSC ( HANDLE, SEGDSC, CNAMES(5), CLDSC5 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Compare the first entry of column 5 to the first entry.
!     We expect equality.
!
      ELTIDX = 1
      DTYPE  = TIME
      CVAL   = ' '
      DVAL   = TDATA(1)
      IVAL   = 0
      NULL   = .FALSE.
      ROW    = 1
!
!     For class two columns, the "row" input required by ZZEKSCMP
!     is actually a pointer to the base of a data structure called an
!     "EK record pointer." (See usage in ZZEKRD02 for an example;
!     also see ekrecptr.inc.) We need to obtain this record pointer
!     for the row of interest: row 1.
!
      CALL ZZEKTRDP (  HANDLE, SEGDSC(RTIDX), ROW, RECPTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      MATCH = ZZEKSCMP ( EQ,     HANDLE, SEGDSC, CLDSC5, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'EQ MATCH', MATCH, .TRUE., OK )


      MATCH = ZZEKSCMP ( LT,     HANDLE, SEGDSC, CLDSC5, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'LT MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( GT,     HANDLE, SEGDSC, CLDSC5, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'GT MATCH', MATCH, .FALSE., OK )



      MATCH = ZZEKSCMP ( ISNULL, HANDLE, SEGDSC, CLDSC5, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISNULL MATCH', MATCH, .FALSE., OK )


      MATCH = ZZEKSCMP ( NOTNUL, HANDLE, SEGDSC, CLDSC5, RECPTR, &
                         ELTIDX, DTYPE,  CVAL,   DVAL,   IVAL,  NULL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'NOTNUL MATCH', MATCH, .TRUE., OK )



!
! --- Case: ------------------------------------------------------
!

!
!     Unload the EKs and delete the files.
!
      CALL TCASE  ( 'Unload EKs from query system.' )

      CALL EKUEF  ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( EK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


