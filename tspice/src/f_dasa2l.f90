!$Procedure      F_DASA2L ( Test DAS address calculation routine )

      SUBROUTINE F_DASA2L ( OK )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Test the SPICELIB DAS address calculation routine DASA2L.
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

      INCLUDE 'f_dasa2l.inc'

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
!     This routine tests the routine DASA2L.
!
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
!
! $ Version
!
!-    TSPICE Version 1.0.0, 25-APR-2014 (NJB)
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB functions
!

!
!     Other functions
!
      DOUBLE PRECISION      T_RANDD

!
!     Local parameters
!
      INTEGER               CHR
      PARAMETER           ( CHR    =  1  )

      INTEGER               DP
      PARAMETER           ( DP     =  2  )

      INTEGER               INT
      PARAMETER           ( INT    =  3  )



!
!     Words per data record, for each data type:
!
      INTEGER               NWC
      PARAMETER           ( NWC = 1024 )

      INTEGER               NWD
      PARAMETER           ( NWD =  128 )

      INTEGER               NWI
      PARAMETER           ( NWI =  256 )

!
!     Directory forward pointer location
!

!      INTEGER               FWDLOC
!      PARAMETER           ( FWDLOC = 2 )

!
!     Directory address range locations
!

      INTEGER               CHRRNG
      PARAMETER           ( CHRRNG =          3 )

      INTEGER               DPRNG
      PARAMETER           ( DPRNG  = CHRRNG + 2 )

      INTEGER               INTRNG
      PARAMETER           ( INTRNG = DPRNG  + 2 )

!
!     Index of highest address in a `range array':
!
!      INTEGER               HIGH
!      PARAMETER           ( HIGH = 2 )

!
!     Location of first type descriptor
!
!      INTEGER               BEGDSC
!      PARAMETER           ( BEGDSC = 9 )

!
!     File table size
!
!      INTEGER               MAXFIL
!      PARAMETER           ( MAXFIL = 20 )

!
!     Number of cluster descriptors in cluster directory:
!
      INTEGER               CLRCSZ
      PARAMETER           ( CLRCSZ = 247 )


      INTEGER               MXCLST
      PARAMETER           ( MXCLST = 100000 )


!
!     Data word numbering scheme codes:
!
!     SCHADD indicates that each numeric word contains
!     its own address. Character words contain their
!     addresses mod 128.
!
      INTEGER               SCHADD
      PARAMETER           ( SCHADD = 1 )

!
!     SCHCOD indicates that each numeric word contains
!     a code based on the file number, record, and address.
!
!      INTEGER               SCHCOD
!      PARAMETER           ( SCHCOD = 2 )

!
!     Additional parameters
!
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

!      INTEGER               TOTFIL
!      PARAMETER           ( TOTFIL = 30 )

!      INTEGER               MAXLOD
!      PARAMETER           ( MAXLOD = 20 )

      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 4 )

      INTEGER               ACCLEN
      PARAMETER           ( ACCLEN = 10 )

      INTEGER               NFAST
      PARAMETER           ( NFAST = 10 )

      INTEGER               NRDNSG
      PARAMETER           ( NRDNSG = 10 )

      INTEGER               NWRIT
      PARAMETER           ( NWRIT  = 10 )

!
!     DASA2L file buffer size:
!
!      INTEGER               A2LBSZ
!      PARAMETER           ( A2LBSZ = 20 )


      INTEGER               PTHDEP
      PARAMETER           ( PTHDEP = 30 )

      INTEGER               PTHWID
      PARAMETER           ( PTHWID = 50 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

!
!     Local Variables
!
      CHARACTER*(ACCLEN)    ACCESS
      CHARACTER*(1)         CVAL
      CHARACTER*(FILSIZ)    DAS0
      CHARACTER*(FILSIZ)    DAS1
      CHARACTER*(FILSIZ)    ERRDAS
      CHARACTER*(PTHWID)    EXPPTH ( LBCELL : PTHDEP )
      CHARACTER*(FILSIZ)    FAST   ( NFAST )
      CHARACTER*(TYPLEN)    FTYPE
      CHARACTER*(PTHWID)    PATH   ( LBCELL : PTHDEP )
      CHARACTER*(FILSIZ)    RDNSEG ( NRDNSG )
      CHARACTER*(FILSIZ)    WRTABL ( NWRIT  )

      DOUBLE PRECISION      DVAL
      DOUBLE PRECISION DVAL_A1 ( 1 )
      EQUIVALENCE ( DVAL, DVAL_A1 )

      INTEGER               ADDRSS
      INTEGER               BIGHAN
      INTEGER               CLBASE
      INTEGER               CLNWDS ( MXCLST )
      INTEGER               CLSIZE
      INTEGER               CLTYPS ( MXCLST )
      INTEGER               DTYPE
      INTEGER               FILENO
      INTEGER               FREE
      INTEGER               HANDLE
      INTEGER               HFAST  ( NFAST  )
      INTEGER               HRDNSG ( NRDNSG )
      INTEGER               HWRIT  ( NWRIT  )
      INTEGER               I
      INTEGER               IVAL
      INTEGER IVAL_A1 ( 1 )
      EQUIVALENCE ( IVAL, IVAL_A1 )
      INTEGER               J
      INTEGER               K
      INTEGER               LASTC
      INTEGER               LASTD
      INTEGER               LASTI
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
      INTEGER               N
      INTEGER               NCOMRC
      INTEGER               NCOMCH
      INTEGER               NCLUST
      INTEGER               NEXT   ( 3 )
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NSCHEM
      INTEGER               PREV   ( 3 )
      INTEGER               RECNO
      INTEGER               RECSZS ( 3 )
      INTEGER               SEED
      INTEGER               WORDNO
      INTEGER               XCLBAS
      INTEGER               XCLSIZ
      INTEGER               XRECNO
      INTEGER               XWRDNO

      LOGICAL               FCLOSE
      LOGICAL               SGRGAT


!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                  NEXT   / DP,  INT, CHR /
      DATA                  PREV   / INT, CHR, DP  /
      DATA                  RECSZS / NWC, NWD, NWI /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_DASA2L' )

!**********************************************************************
!
!     DASA2L error cases:
!
!**********************************************************************


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Bad data type' )

      ERRDAS = 'error.das'

      IF ( EXISTS(ERRDAS) ) THEN
         CALL DELFIL( ERRDAS )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL DASONW ( ERRDAS, 'TEST', ERRDAS, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DTYPE  = -1
      ADDRSS = 1

      CALL DASA2L ( HANDLE, DTYPE,  ADDRSS, &
                    CLBASE, CLSIZE, RECNO,  WORDNO )
      CALL CHCKXC ( .TRUE., 'SPICE(DASINVALIDTYPE)', OK )

!
!     Close DAS without segregating: write buffered records
!     (mandatory!) and execute low-level close.
!
      CALL DASWBR ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DASLLC ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DP address out of range' )

      ERRDAS = 'error.das'

      IF ( EXISTS(ERRDAS) ) THEN
         CALL DELFIL( ERRDAS )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL DASONW ( ERRDAS, 'TEST', ERRDAS, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N = 10

      DO I = 1, N
         CALL DASADD ( HANDLE, 1, [DBLE(I)] )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      DO I = 1, N
         CALL DASADI ( HANDLE, 1, [I] )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      DO I = 1, N
         CALL DASADC ( HANDLE, 1, 1, 1, CHAR(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO



      DTYPE  = DP
      ADDRSS = N+1

      CALL DASA2L ( HANDLE, DTYPE,  ADDRSS, &
                    CLBASE, CLSIZE, RECNO,  WORDNO )
      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHADDRESS)', OK )

      ADDRSS = 0
      CALL DASA2L ( HANDLE, DTYPE,  ADDRSS, &
                    CLBASE, CLSIZE, RECNO,  WORDNO )
      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHADDRESS)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'INT address out of range' )

      DTYPE  = INT
      ADDRSS = N+1

      CALL DASA2L ( HANDLE, DTYPE,  ADDRSS, &
                    CLBASE, CLSIZE, RECNO,  WORDNO )
      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHADDRESS)', OK )

      ADDRSS = 0
      CALL DASA2L ( HANDLE, DTYPE,  ADDRSS, &
                    CLBASE, CLSIZE, RECNO,  WORDNO )
      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHADDRESS)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'CHR address out of range' )

      DTYPE  = CHR
      ADDRSS = N+1

      CALL DASA2L ( HANDLE, DTYPE,  ADDRSS, &
                    CLBASE, CLSIZE, RECNO,  WORDNO )
      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHADDRESS)', OK )

      ADDRSS = 0
      CALL DASA2L ( HANDLE, DTYPE,  ADDRSS, &
                    CLBASE, CLSIZE, RECNO,  WORDNO )
      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHADDRESS)', OK )




!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'FAILED is TRUE after DASHFS; new file case' )

      CALL SIGERR ( 'HFS setup' )

      DTYPE  = 1
      ADDRSS = 1

      CALL DASA2L ( HANDLE, DTYPE,  ADDRSS, &
                    CLBASE, CLSIZE, RECNO,  WORDNO )
      CALL CHCKXC ( .TRUE., 'HFS setup', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'FAILED is TRUE after DASHFS; known file case' )


      DTYPE  = 1
      ADDRSS = 1


      CALL DASA2L ( HANDLE, DTYPE,  ADDRSS, &
                    CLBASE, CLSIZE, RECNO,  WORDNO )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SIGERR ( 'HFS setup' )

      CALL DASA2L ( HANDLE, DTYPE,  ADDRSS, &
                    CLBASE, CLSIZE, RECNO,  WORDNO )
      CALL CHCKXC ( .TRUE., 'HFS setup', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DASRRI failure: attempt to read non-existent ' &
      //           'record.'                                      )

!
!     Set the maximum integer address in the file summary
!     high enough so that DASA2L will look beyond the last
!     directory.
!
      IVAL = 1000000
!
      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMRC, &
                    NCOMCH, FREE,   LASTLA, LASTRC, LASTWD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      LASTLA ( INT ) = IVAL

      CALL DASUFS ( HANDLE, NRESVR, NRESVC, NCOMRC, &
                    NCOMCH, FREE,   LASTLA, LASTRC, LASTWD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASA2L ( HANDLE, INT, IVAL, CLBASE, CLSIZE, RECNO, WORDNO )
      CALL CHCKXC ( .TRUE., 'SPICE(DASFILEREADFAILED)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Bad directory record: promises more data than ' &
      //           'are really available.'                         )

!
!     Assign a big value to the upper integer address of the first
!     directory record.
!
      IVAL = 1000000
      CALL DASURI ( HANDLE, 2, INTRNG+1, INTRNG+1, [IVAL] )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get DASFM in on the conspiracy: update the file's summary to
!     agree with the lie told by the directory record's integer address
!     range.
!
      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMRC, &
                    NCOMCH, FREE,   LASTLA, LASTRC, LASTWD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      LASTLA ( INT ) = IVAL

      CALL DASUFS ( HANDLE, NRESVR, NRESVC, NCOMRC, &
                    NCOMCH, FREE,   LASTLA, LASTRC, LASTWD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASA2L ( HANDLE, INT, IVAL, CLBASE, CLSIZE, RECNO, WORDNO )
      CALL CHCKXC ( .TRUE., 'SPICE(BADDASDIRECTORY)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Clean up error test file.' )

!
!     Close DAS without segregating: write buffered records
!     (mandatory!) and execute low-level close.
!
      CALL DASWBR ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DASLLC ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL( ERRDAS )
      CALL CHCKXC ( .FALSE., ' ', OK )



!**********************************************************************
!
!     DASA2L normal cases:
!
!**********************************************************************

!
!     The following set of tests relies on the DAS reader routines
!     to exercise DASA2L.
!

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DAS is writable; contains only INT data.' )

      DAS0 = 'test0.das'

      IF ( EXISTS(DAS0) ) THEN
         CALL DELFIL( DAS0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      FTYPE     = 'TEST'
      NCOMRC    = 0
      FILENO    = 1
      NCLUST    = 1
      CLTYPS(1) = INT
      CLNWDS(1) = 3000
      SGRGAT    = .FALSE.
      FCLOSE    = .FALSE.
      NSCHEM    = SCHADD

      CALL TSTDAS ( DAS0,   FTYPE,  NCOMRC, FILENO, &
                    NCLUST, CLTYPS, CLNWDS, SGRGAT, &
                    FCLOSE, NSCHEM, HANDLE         )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify write access.
!
      CALL DASHAM ( HANDLE, ACCESS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'ACCESS', ACCESS, '=', 'WRITE', OK )

!
!     Check data.
!
      DO I = 1, CLNWDS(1)

         CALL DASRDI ( HANDLE, I, I, IVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( IVAL .NE. I ) THEN
            CALL CHCKSI ( 'IVAL', IVAL, '=', I, 0, OK )
         END IF

      END DO


!
!     Close DAS without segregating: write buffered records
!     (mandatory!) and execute low-level close.
!
      CALL DASWBR ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DASLLC ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DAS is read-only; contains only INT data.' )

!
!     Open file for read access; check data.
!
      CALL DASOPR ( DAS0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, CLNWDS(1)

         CALL DASRDI ( HANDLE, I, I, IVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( IVAL .NE. I ) THEN
            CALL CHCKSI ( 'IVAL', IVAL, '=', I, 0, OK )
         END IF

      END DO

!
!     Close DAS.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DAS is writable; contains only DP data.' )

      DAS0 = 'test0.das'

      IF ( EXISTS(DAS0) ) THEN
         CALL DELFIL( DAS0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      FTYPE     = 'TEST'
      NCOMRC    = 0
      FILENO    = 1
      NCLUST    = 1
      CLTYPS(1) = DP
      CLNWDS(1) = 1500
      SGRGAT    = .FALSE.
      FCLOSE    = .FALSE.
      NSCHEM    = SCHADD

      CALL TSTDAS ( DAS0,   FTYPE,  NCOMRC, FILENO, &
                    NCLUST, CLTYPS, CLNWDS, SGRGAT, &
                    FCLOSE, NSCHEM, HANDLE         )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify write access.
!
      CALL DASHAM ( HANDLE, ACCESS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'ACCESS', ACCESS, '=', 'WRITE', OK )

!
!     Check data.
!
      DO I = 1, CLNWDS(1)

         CALL DASRDD ( HANDLE, I, I, DVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( DVAL .NE. DBLE(I) ) THEN
            CALL CHCKSD ( 'DVAL', DVAL, '=', DBLE(I), 0.D0, OK )
         END IF

      END DO

!
!     Close DAS without segregating: write buffered records
!     (mandatory!) and execute low-level close.
!
      CALL DASWBR ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DASLLC ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DAS is read-only; contains only DP data.' )

!
!     Check data.
!
      CALL DASOPR ( DAS0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, CLNWDS(1)

         CALL DASRDD ( HANDLE, I, I, DVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( DVAL .NE. DBLE(I) ) THEN
            CALL CHCKSD ( 'DVAL', DVAL, '=', DBLE(I), 0.D0, OK )
         END IF

      END DO

      CALL DASCLS( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DAS is writable; contains only CHR data.' )

      DAS0 = 'test0.das'

      IF ( EXISTS(DAS0) ) THEN
         CALL DELFIL( DAS0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      FTYPE     = 'TEST'
      NCOMRC    = 0
      FILENO    = 1
      NCLUST    = 1
      CLTYPS(1) = CHR
      CLNWDS(1) = 5000
      SGRGAT    = .FALSE.
      FCLOSE    = .FALSE.
      NSCHEM    = SCHADD

      CALL TSTDAS ( DAS0,   FTYPE,  NCOMRC, FILENO, &
                    NCLUST, CLTYPS, CLNWDS, SGRGAT, &
                    FCLOSE, NSCHEM, HANDLE         )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify write access.
!
      CALL DASHAM ( HANDLE, ACCESS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'ACCESS', ACCESS, '=', 'WRITE', OK )

!
!     Check data.
!
      DO I = 1, CLNWDS(1)

         CALL DASRDC ( HANDLE, I, I, 1, 1, CVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         J = MOD(I,128)

         IF ( ICHAR(CVAL) .NE. J ) THEN
            CALL CHCKSC ( 'CVAL', CVAL, '=', CHAR(J), OK )
         END IF

      END DO

      CALL DASCLS( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DAS is read-only; contains only CHR data.' )

!
!     Check data.
!
      CALL DASOPR ( DAS0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, CLNWDS(1)

         CALL DASRDC ( HANDLE, I, I, 1, 1, CVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         J = MOD(I,128)

         IF ( ICHAR(CVAL) .NE. J ) THEN
            CALL CHCKSC ( 'CVAL', CVAL, '=', CHAR(J), OK )
         END IF

      END DO

      CALL DASCLS( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DAS is writable; contains only INT and DP data.' )

      DAS0 = 'test0.das'

      IF ( EXISTS(DAS0) ) THEN
         CALL DELFIL( DAS0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      FTYPE     = 'TEST'
      NCOMRC    = 0
      FILENO    = 1
      NCLUST    = 2
      CLTYPS(1) = INT
      CLNWDS(1) = 3000
      CLTYPS(2) = DP
      CLNWDS(2) = 1500
      SGRGAT    = .FALSE.
      FCLOSE    = .FALSE.
      NSCHEM    = SCHADD

      CALL TSTDAS ( DAS0,   FTYPE,  NCOMRC, FILENO, &
                    NCLUST, CLTYPS, CLNWDS, SGRGAT, &
                    FCLOSE, NSCHEM, HANDLE         )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify write access.
!
      CALL DASHAM ( HANDLE, ACCESS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'ACCESS', ACCESS, '=', 'WRITE', OK )

!
!     Check data.
!
      DO I = 1, CLNWDS(1)

         CALL DASRDI ( HANDLE, I, I, IVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( IVAL .NE. I ) THEN
            CALL CHCKSI ( 'IVAL', IVAL, '=', I, 0, OK )
         END IF

      END DO

      DO I = 1, CLNWDS(2)

         CALL DASRDD ( HANDLE, I, I, DVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( DVAL .NE. DBLE(I) ) THEN
            CALL CHCKSD ( 'DVAL', DVAL, '=', DBLE(I), 0.D0, OK )
         END IF

      END DO

!
!     Close file so as to leave it unsegregated.
!
!     This is a special case: we need to create a read-only,
!     unsegregated file having the data types of its first
!     two clusters in descending numeric order (DP < INT).
!     This file is used to test a branch of the segregation
!     detection logic in DASA2L.
!
      CALL DASWBR( HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL DASLLC( HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DAS is read-only and unsegregated; contains only ' &
      //           'INT and DP data.' )

!
!     Check data.
!
      CALL DASOPR ( DAS0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, CLNWDS(1)

         CALL DASRDI ( HANDLE, I, I, IVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( IVAL .NE. I ) THEN
            CALL CHCKSI ( 'IVAL', IVAL, '=', I, 0, OK )
         END IF

      END DO

      DO I = 1, CLNWDS(2)

         CALL DASRDD ( HANDLE, I, I, DVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( DVAL .NE. DBLE(I) ) THEN
            CALL CHCKSD ( 'DVAL', DVAL, '=', DBLE(I), 0.D0, OK )
         END IF

      END DO

      CALL DASCLS( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DAS is read-only and segregated; contains only ' &
      //           'INT and DP data.' )


!
!     Segregate the file.
!
      CALL DASOPW ( DAS0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check data.
!
      CALL DASOPR ( DAS0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, CLNWDS(1)

         CALL DASRDI ( HANDLE, I, I, IVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( IVAL .NE. I ) THEN
            CALL CHCKSI ( 'IVAL', IVAL, '=', I, 0, OK )
         END IF

      END DO

      DO I = 1, CLNWDS(2)

         CALL DASRDD ( HANDLE, I, I, DVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( DVAL .NE. DBLE(I) ) THEN
            CALL CHCKSD ( 'DVAL', DVAL, '=', DBLE(I), 0.D0, OK )
         END IF

      END DO

      CALL DASCLS( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DAS is writable; contains only INT and CHAR data.' )

      DAS0 = 'test0.das'

      IF ( EXISTS(DAS0) ) THEN
         CALL DELFIL( DAS0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      FTYPE     = 'TEST'
      NCOMRC    = 0
      FILENO    = 1
      NCLUST    = 2
      CLTYPS(1) = INT
      CLNWDS(1) = 5000
      CLTYPS(2) = CHR
      CLNWDS(2) = 2500
      SGRGAT    = .FALSE.
      FCLOSE    = .FALSE.
      NSCHEM    = SCHADD

      CALL TSTDAS ( DAS0,   FTYPE,  NCOMRC, FILENO, &
                    NCLUST, CLTYPS, CLNWDS, SGRGAT, &
                    FCLOSE, NSCHEM, HANDLE         )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify write access.
!
      CALL DASHAM ( HANDLE, ACCESS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'ACCESS', ACCESS, '=', 'WRITE', OK )
!
!     Check data.
!
      DO I = 1, CLNWDS(1)

         CALL DASRDI ( HANDLE, I, I, IVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( IVAL .NE. I ) THEN
            CALL CHCKSI ( 'IVAL', IVAL, '=', I, 0, OK )
         END IF

      END DO

      DO I = 1, CLNWDS(2)

         CALL DASRDC ( HANDLE, I, I, 1, 1, CVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         J = MOD(I,128)

         IF ( ICHAR(CVAL) .NE. J ) THEN
            CALL CHCKSC ( 'CVAL', CVAL, '=', CHAR(J), OK )
         END IF

      END DO

!
!     Close DAS without segregating: write buffered records
!     (mandatory!) and execute low-level close.
!
      CALL DASWBR ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DASLLC ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE( 'DAS is read-only; contains only INT and CHAR data.' )


!
!     Check data.
!
      CALL DASOPR ( DAS0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, CLNWDS(1)

         CALL DASRDI ( HANDLE, I, I, IVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( IVAL .NE. I ) THEN
            CALL CHCKSI ( 'IVAL', IVAL, '=', I, 0, OK )
         END IF

      END DO

      DO I = 1, CLNWDS(2)

         CALL DASRDC ( HANDLE, I, I, 1, 1, CVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         J = MOD(I,128)

         IF ( ICHAR(CVAL) .NE. J ) THEN
            CALL CHCKSC ( 'CVAL', CVAL, '=', CHAR(J), OK )
         END IF

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DAS is writable; contains only CHAR and DP data.' )

      DAS0 = 'test0.das'

      IF ( EXISTS(DAS0) ) THEN
         CALL DELFIL( DAS0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      FTYPE     = 'TEST'
      NCOMRC    = 0
      FILENO    = 1
      NCLUST    = 2
      CLTYPS(1) = CHR
      CLNWDS(1) = 5000
      CLTYPS(2) = DP
      CLNWDS(2) = 2500
      SGRGAT    = .FALSE.
      FCLOSE    = .FALSE.
      NSCHEM    = SCHADD

      CALL TSTDAS ( DAS0,   FTYPE,  NCOMRC, FILENO, &
                    NCLUST, CLTYPS, CLNWDS, SGRGAT, &
                    FCLOSE, NSCHEM, HANDLE         )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify write access.
!
      CALL DASHAM ( HANDLE, ACCESS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'ACCESS', ACCESS, '=', 'WRITE', OK )
!
!     Check data.
!
      DO I = 1, CLNWDS(1)

         CALL DASRDC ( HANDLE, I, I, 1, 1, CVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         J = MOD(I,128)

         IF ( ICHAR(CVAL) .NE. J ) THEN
            CALL CHCKSC ( 'CVAL', CVAL, '=', CHAR(J), OK )
         END IF

      END DO



      DO I = 1, CLNWDS(2)

         CALL DASRDD ( HANDLE, I, I, DVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( DVAL .NE. DBLE(I) ) THEN
            CALL CHCKSD ( 'DVAL', DVAL, '=', DBLE(I), 0.D0, OK )
         END IF

      END DO

!
!     Close DAS without segregating: write buffered records
!     (mandatory!) and execute low-level close.
!
      CALL DASWBR ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DASLLC ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DAS is read-only; contains only CHAR and DP data.' )

      CALL DASOPR ( DAS0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check data.
!
      DO I = 1, CLNWDS(1)

         CALL DASRDC ( HANDLE, I, I, 1, 1, CVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         J = MOD(I,128)

         IF ( ICHAR(CVAL) .NE. J ) THEN
            CALL CHCKSC ( 'CVAL', CVAL, '=', CHAR(J), OK )
         END IF

      END DO


      DO I = 1, CLNWDS(2)

         CALL DASRDD ( HANDLE, I, I, DVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( DVAL .NE. DBLE(I) ) THEN
            CALL CHCKSD ( 'DVAL', DVAL, '=', DBLE(I), 0.D0, OK )
         END IF

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DAS is writable; contains INT, CHAR and DP data.' )

      DAS0 = 'test0.das'

      IF ( EXISTS(DAS0) ) THEN
         CALL DELFIL( DAS0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      FTYPE     = 'TEST'
      NCOMRC    = 0
      FILENO    = 1
      NCLUST    = 3
      CLTYPS(1) = INT
      CLNWDS(1) = 1500
      CLTYPS(2) = CHR
      CLNWDS(2) = 5000
      CLTYPS(3) = DP
      CLNWDS(3) = 2500
      SGRGAT    = .FALSE.
      FCLOSE    = .FALSE.
      NSCHEM    = SCHADD

      CALL TSTDAS ( DAS0,   FTYPE,  NCOMRC, FILENO, &
                    NCLUST, CLTYPS, CLNWDS, SGRGAT, &
                    FCLOSE, NSCHEM, HANDLE         )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify write access.
!
      CALL DASHAM ( HANDLE, ACCESS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'ACCESS', ACCESS, '=', 'WRITE', OK )

!
!     Check data.
!
      DO I = 1, CLNWDS(1)

         CALL DASRDI ( HANDLE, I, I, IVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( IVAL .NE. I ) THEN
            CALL CHCKSI ( 'IVAL', IVAL, '=', I, 0, OK )
         END IF

      END DO

      DO I = 1, CLNWDS(2)

         CALL DASRDC ( HANDLE, I, I, 1, 1, CVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         J = MOD(I,128)

         IF ( ICHAR(CVAL) .NE. J ) THEN
            CALL CHCKSC ( 'CVAL', CVAL, '=', CHAR(J), OK )
         END IF

      END DO

      DO I = 1, CLNWDS(3)

         CALL DASRDD ( HANDLE, I, I, DVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( DVAL .NE. DBLE(I) ) THEN
            CALL CHCKSD ( 'DVAL', DVAL, '=', DBLE(I), 0.D0, OK )
         END IF

      END DO

!
!     Close DAS without segregating: write buffered records
!     (mandatory!) and execute low-level close.
!
      CALL DASWBR ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DASLLC ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DAS is read-only; contains INT, CHAR and DP data.' )

!     Check data.
!
      CALL DASOPR ( DAS0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, CLNWDS(1)

         CALL DASRDI ( HANDLE, I, I, IVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( IVAL .NE. I ) THEN
            CALL CHCKSI ( 'IVAL', IVAL, '=', I, 0, OK )
         END IF

      END DO

      DO I = 1, CLNWDS(2)

         CALL DASRDC ( HANDLE, I, I, 1, 1, CVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         J = MOD(I,128)

         IF ( ICHAR(CVAL) .NE. J ) THEN
            CALL CHCKSC ( 'CVAL', CVAL, '=', CHAR(J), OK )
         END IF

      END DO

      DO I = 1, CLNWDS(3)

         CALL DASRDD ( HANDLE, I, I, DVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( DVAL .NE. DBLE(I) ) THEN
            CALL CHCKSD ( 'DVAL', DVAL, '=', DBLE(I), 0.D0, OK )
         END IF

      END DO

!
!     Close DAS.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE( 'DAS is segregated; contains INT, CHAR and DP data.' )

!
!     Open existing DAS for write access; close after segregation.
!
      CALL DASOPW ( DAS0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check data.
!
      CALL DASOPR ( DAS0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, CLNWDS(1)

         CALL DASRDI ( HANDLE, I, I, IVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( IVAL .NE. I ) THEN
            CALL CHCKSI ( 'IVAL', IVAL, '=', I, 0, OK )
         END IF

      END DO

      DO I = 1, CLNWDS(2)

         CALL DASRDC ( HANDLE, I, I, 1, 1, CVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         J = MOD(I,128)

         IF ( ICHAR(CVAL) .NE. J ) THEN
            CALL CHCKSC ( 'CVAL', CVAL, '=', CHAR(J), OK )
         END IF

      END DO

      DO I = 1, CLNWDS(3)

         CALL DASRDD ( HANDLE, I, I, DVAL_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( DVAL .NE. DBLE(I) ) THEN
            CALL CHCKSD ( 'DVAL', DVAL, '=', DBLE(I), 0.D0, OK )
         END IF

      END DO

!
!     Close DAS.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE( 'DAS is segregated; contains INT, CHAR and DP data. ' &
      //          'Check logic path for first d.p. lookup. Path case 3 ' &
      //          ': FAST, UNKNOWN FILE.'                            )

      CALL DASOPR ( DAS0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create expected path.
!
      CALL SSIZEC ( PTHDEP, EXPPTH )

      CALL APPNDC ( TYPCHK, EXPPTH )
      CALL APPNDC ( NOTSAM, EXPPTH )
      CALL APPNDC ( UNKNWN, EXPPTH )
      CALL APPNDC ( BUFSHF, EXPPTH )
      CALL APPNDC ( BUFINS, EXPPTH )
      CALL APPNDC ( SETTBF, EXPPTH )
      CALL APPNDC ( GETACC, EXPPTH )
      CALL APPNDC ( GFSUM0, EXPPTH )
      CALL APPNDC ( SEGCHK, EXPPTH )
      CALL APPNDC ( SETTBF, EXPPTH )
      CALL APPNDC ( ADDCHK, EXPPTH )
      CALL APPNDC ( FASTST, EXPPTH )
      CALL APPNDC ( CALC,   EXPPTH )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Execute instrumented version of DASA2L.
!
      CALL T_PTHNEW
      CALL P_DASA2L ( HANDLE, DP,    1, CLBASE, &
                      CLSIZE, RECNO,    WORDNO )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check actual path.
!
      CALL SSIZEC ( PTHDEP, PATH )

      CALL T_PTHGET ( PATH )
      CALL T_PTHCMP ( PATH, EXPPTH )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE( 'DAS is segregated; contains INT, CHAR and DP data. ' &
      //          'Check logic path for second lookup (type = INT). ' &
      //          'Path case 1: FAST, SAME FILE.'                     )


      CALL P_DASA2L ( HANDLE, DP,   1, CLBASE, &
                      CLSIZE, RECNO,    WORDNO )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create expected path.
!
      CALL SSIZEC ( PTHDEP, EXPPTH )

      CALL APPNDC ( TYPCHK, EXPPTH )
      CALL APPNDC ( ADDCHK, EXPPTH )
      CALL APPNDC ( FASTST, EXPPTH )
      CALL APPNDC ( CALC,   EXPPTH )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Execute instrumented version of DASA2L.
!
      CALL T_PTHNEW
      CALL P_DASA2L ( HANDLE, INT,   1, CLBASE, &
                      CLSIZE, RECNO,    WORDNO )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check actual path.
!
      CALL SSIZEC ( PTHDEP, PATH )

      CALL T_PTHGET ( PATH )
      CALL T_PTHCMP ( PATH, EXPPTH )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case --------------------------------------------------------
!

!
!     Test address calculation for a non-trivial, unsegregated file.
!
!
!     This file contains multiple cluster directories. All possible
!     combinations of data types of adjacent clusters occur.
!
      CALL TCASE( 'Create non-trivial, unsegregated file.' )

!
!     Set cluster data types.
!
      NCLUST = 0

      DO I = 1, 2

         DO J = 1, 3

            NCLUST = NCLUST + 1
!
!           Set the first data type of the record.
!
            CLTYPS( NCLUST ) = J

            IF ( I .EQ. 1 ) THEN
!
!              Insert one "previous" type at cluster position 2.
!              This exercises a branch of the segregation detection
!              logic in DASA2L.
!
               NCLUST           = NCLUST + 1
               CLTYPS( NCLUST ) = PREV( CLTYPS(NCLUST-1) )


               DO K = 3, CLRCSZ

                  NCLUST           = NCLUST + 1
                  CLTYPS( NCLUST ) = NEXT( CLTYPS(NCLUST-1) )

               END DO

            ELSE

               DO K = 2, CLRCSZ

                  NCLUST           = NCLUST + 1
                  CLTYPS( NCLUST ) = PREV( CLTYPS(NCLUST-1) )

               END DO

            END IF

         END DO

      END DO

!
!     Set cluster record counts.
!
      SEED = -1

      DO I = 1, NCLUST

         J = NINT(  T_RANDD( 1.D0, 3.D0, SEED )  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLNWDS(I) = J * RECSZS( CLTYPS(I) )

      END DO

!
!  @@@
!
      DAS1 = 'big_nonseg.das'

      IF ( EXISTS(DAS1) ) THEN
         CALL DELFIL( DAS1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      FTYPE     = 'TEST'
      NCOMRC    = 0
      FILENO    = 1
      SGRGAT    = .FALSE.
      FCLOSE    = .FALSE.
      NSCHEM    = SCHADD

      CALL TSTDAS ( DAS1,      FTYPE,  NCOMRC,  FILENO, &
                    NCLUST,    CLTYPS, CLNWDS,  SGRGAT, &
                    FCLOSE,    NSCHEM, BIGHAN          )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Read from the big, non-segregated, writable DAS' )

!
!     Verify write access.
!
      CALL DASHAM ( BIGHAN, ACCESS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'ACCESS', ACCESS, '=', 'WRITE', OK )

!
!     Get maximum addresses of each data type.
!
      CALL DASLLA ( BIGHAN, LASTC, LASTD, LASTI )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check integer data.
!
      DO I = 1, LASTI

         CALL DASA2L  ( BIGHAN, INT, I, CLBASE, CLSIZE, RECNO,  WORDNO )
         CALL T_DASA2L( BIGHAN, INT, I, XCLBAS, XCLSIZ, XRECNO, XWRDNO )
!
!        We're not going to call CHCKSI for each item, because that
!        call is way too slow. Check for a mismatched output first.
!
         IF (     ( RECNO  .NE. XRECNO ) &
             .OR. ( WORDNO .NE. XWRDNO ) &
             .OR. ( CLBASE .NE. XCLBAS ) &
             .OR. ( CLSIZE .NE. XCLSIZ ) ) THEN

            CALL CHCKSI ( 'RECNO',  RECNO,  '=', XRECNO, 0, OK )
            CALL CHCKSI ( 'WORDNO', WORDNO, '=', XWRDNO, 0, OK )
            CALL CHCKSI ( 'CLBASE', CLBASE, '=', XCLBAS, 0, OK )
            CALL CHCKSI ( 'CLSIZE', CLSIZE, '=', XCLSIZ, 0, OK )

         END IF

      END DO

!
!     Check d.p. data.
!
      DO I = 1, LASTD

         CALL DASA2L  ( BIGHAN, DP, I, CLBASE, CLSIZE, RECNO,  WORDNO )
         CALL T_DASA2L( BIGHAN, DP, I, XCLBAS, XCLSIZ, XRECNO, XWRDNO )

         IF (     ( RECNO  .NE. XRECNO ) &
             .OR. ( WORDNO .NE. XWRDNO ) &
             .OR. ( CLBASE .NE. XCLBAS ) &
             .OR. ( CLSIZE .NE. XCLSIZ ) ) THEN

            CALL CHCKSI ( 'RECNO',  RECNO,  '=', XRECNO, 0, OK )
            CALL CHCKSI ( 'WORDNO', WORDNO, '=', XWRDNO, 0, OK )
            CALL CHCKSI ( 'CLBASE', CLBASE, '=', XCLBAS, 0, OK )
            CALL CHCKSI ( 'CLSIZE', CLSIZE, '=', XCLSIZ, 0, OK )

         END IF

      END DO


!
!     Check character data.
!
      DO I = 1, LASTC

         CALL DASA2L  ( BIGHAN, CHR, I, CLBASE, CLSIZE, RECNO,  WORDNO )
         CALL T_DASA2L( BIGHAN, CHR, I, XCLBAS, XCLSIZ, XRECNO, XWRDNO )

         IF (     ( RECNO  .NE. XRECNO ) &
             .OR. ( WORDNO .NE. XWRDNO ) &
             .OR. ( CLBASE .NE. XCLBAS ) &
             .OR. ( CLSIZE .NE. XCLSIZ ) ) THEN

            CALL CHCKSI ( 'RECNO',  RECNO,  '=', XRECNO, 0, OK )
            CALL CHCKSI ( 'WORDNO', WORDNO, '=', XWRDNO, 0, OK )
            CALL CHCKSI ( 'CLBASE', CLBASE, '=', XCLBAS, 0, OK )
            CALL CHCKSI ( 'CLSIZE', CLSIZE, '=', XCLSIZ, 0, OK )

         END IF

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Read from the big, non-segregated, read-only DAS' )

!
!     Close DAS without segregating: write buffered records
!     (mandatory!) and execute low-level close.
!
      CALL DASWBR ( BIGHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DASLLC ( BIGHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get maximum addresses of each data type.
!
      CALL DASOPR ( DAS1, BIGHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASLLA ( BIGHAN, LASTC, LASTD, LASTI )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We're going to use a skip value of 20 in the following tests
!     in order to speed them up. The file's cluster structure
!     should be the same as that of the writable file; all we've
!     done is change the file's status to read-only.
!

!
!     Check integer data.
!
      DO I = 1, LASTI, 20

         CALL DASA2L  ( BIGHAN, INT, I, CLBASE, CLSIZE, RECNO,  WORDNO )
         CALL T_DASA2L( BIGHAN, INT, I, XCLBAS, XCLSIZ, XRECNO, XWRDNO )
!
!        We're not going to call CHCKSI for each item, because that
!        call is way too slow. Check for a mismatched output first.
!
         IF (     ( RECNO  .NE. XRECNO ) &
             .OR. ( WORDNO .NE. XWRDNO ) &
             .OR. ( CLBASE .NE. XCLBAS ) &
             .OR. ( CLSIZE .NE. XCLSIZ ) ) THEN

            CALL CHCKSI ( 'RECNO',  RECNO,  '=', XRECNO, 0, OK )
            CALL CHCKSI ( 'WORDNO', WORDNO, '=', XWRDNO, 0, OK )
            CALL CHCKSI ( 'CLBASE', CLBASE, '=', XCLBAS, 0, OK )
            CALL CHCKSI ( 'CLSIZE', CLSIZE, '=', XCLSIZ, 0, OK )

         END IF

      END DO

!
!     Check d.p. data.
!
      DO I = 1, LASTD, 20

         CALL DASA2L  ( BIGHAN, DP, I, CLBASE, CLSIZE, RECNO,  WORDNO )
         CALL T_DASA2L( BIGHAN, DP, I, XCLBAS, XCLSIZ, XRECNO, XWRDNO )

         IF (     ( RECNO  .NE. XRECNO ) &
             .OR. ( WORDNO .NE. XWRDNO ) &
             .OR. ( CLBASE .NE. XCLBAS ) &
             .OR. ( CLSIZE .NE. XCLSIZ ) ) THEN

            CALL CHCKSI ( 'RECNO',  RECNO,  '=', XRECNO, 0, OK )
            CALL CHCKSI ( 'WORDNO', WORDNO, '=', XWRDNO, 0, OK )
            CALL CHCKSI ( 'CLBASE', CLBASE, '=', XCLBAS, 0, OK )
            CALL CHCKSI ( 'CLSIZE', CLSIZE, '=', XCLSIZ, 0, OK )

         END IF

      END DO


!
!     Check character data.
!
      DO I = 1, LASTC, 20

         CALL DASA2L  ( BIGHAN, CHR, I, CLBASE, CLSIZE, RECNO,  WORDNO )
         CALL T_DASA2L( BIGHAN, CHR, I, XCLBAS, XCLSIZ, XRECNO, XWRDNO )

         IF (     ( RECNO  .NE. XRECNO ) &
             .OR. ( WORDNO .NE. XWRDNO ) &
             .OR. ( CLBASE .NE. XCLBAS ) &
             .OR. ( CLSIZE .NE. XCLSIZ ) ) THEN

            CALL CHCKSI ( 'RECNO',  RECNO,  '=', XRECNO, 0, OK )
            CALL CHCKSI ( 'WORDNO', WORDNO, '=', XWRDNO, 0, OK )
            CALL CHCKSI ( 'CLBASE', CLBASE, '=', XCLBAS, 0, OK )
            CALL CHCKSI ( 'CLSIZE', CLSIZE, '=', XCLSIZ, 0, OK )

         END IF

      END DO

!
!     Make the big file writable for the following tests.
!
      CALL DASCLS ( BIGHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPW ( DAS1, BIGHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case --------------------------------------------------------
!

!
!     Buffering tests follow.
!
!     We'll use sets of files with the following attributes:
!
!        "fast": readonly and segregated
!        read-only, non-segregated
!        writable
!
      CALL TCASE ( 'Create a set of writable DAS files.' )

      DO I = 1, NWRIT

         WRTABL(I) = 'writable#.das'
         CALL REPMI ( WRTABL(I), '#', I-1, WRTABL(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( EXISTS( WRTABL(I) ) ) THEN
            CALL DELFIL( WRTABL(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )
         END IF

         FTYPE     = 'TEST'
         NCOMRC    = 0
         FILENO    = 1
         NCLUST    = 4
         CLTYPS(1) = INT
         CLNWDS(1) = 768
         CLTYPS(2) = CHR
         CLNWDS(2) = 3000
         CLTYPS(3) = DP
         CLNWDS(3) = 400
         CLTYPS(4) = INT
         CLNWDS(4) = 1
         SGRGAT    = .FALSE.
         FCLOSE    = .FALSE.
         NSCHEM    = SCHADD

         CALL TSTDAS ( WRTABL(I), FTYPE,  NCOMRC,  FILENO, &
                       NCLUST,    CLTYPS, CLNWDS,  SGRGAT, &
                       FCLOSE,    NSCHEM, HWRIT(I)         )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        The files are left open.
!
      END DO


!
!     Create a set of read-only, non-segregated DAS files.
!
      DO I = 1, NWRIT

         RDNSEG(I) = 'readnonseg#.das'
         CALL REPMI ( RDNSEG(I), '#', I-1, RDNSEG(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( EXISTS( RDNSEG(I) ) ) THEN
            CALL DELFIL( RDNSEG(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )
         END IF

         FTYPE     = 'TEST'
         NCOMRC    = 0
         FILENO    = 1
         NCLUST    = 4
         CLTYPS(1) = INT
         CLNWDS(1) = 768
         CLTYPS(2) = CHR
         CLNWDS(2) = 3000
         CLTYPS(3) = DP
         CLNWDS(3) = 400
         CLTYPS(4) = INT
         CLNWDS(4) = 1
         SGRGAT    = .FALSE.
         FCLOSE    = .FALSE.
         NSCHEM    = SCHADD

         CALL TSTDAS ( RDNSEG(I), FTYPE,  NCOMRC,  FILENO, &
                       NCLUST,    CLTYPS, CLNWDS,  SGRGAT, &
                       FCLOSE,    NSCHEM, HRDNSG(I)         )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        The files are closed without being segregated.
!
         CALL DASWBR ( HRDNSG(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DASLLC ( HRDNSG(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
!     Create a set of fast DAS files.
!
      DO I = 1, NWRIT

         FAST(I) = 'fast#.das'
         CALL REPMI ( FAST(I), '#', I-1, FAST(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( EXISTS( FAST(I) ) ) THEN
            CALL DELFIL( FAST(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )
         END IF

         FTYPE     = 'TEST'
         NCOMRC    = 0
         FILENO    = 1
         NCLUST    = 4
         CLTYPS(1) = INT
         CLNWDS(1) = 768
         CLTYPS(2) = CHR
         CLNWDS(2) = 3000
         CLTYPS(3) = DP
         CLNWDS(3) = 400
         CLTYPS(4) = INT
         CLNWDS(4) = 1

         SGRGAT    = .FALSE.
         FCLOSE    = .FALSE.
         NSCHEM    = SCHADD

         CALL TSTDAS ( FAST(I),   FTYPE,  NCOMRC,  FILENO, &
                       NCLUST,    CLTYPS, CLNWDS,  SGRGAT, &
                       FCLOSE,    NSCHEM, HFAST(I)         )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        The files are segregated and closed.
!
         CALL DASCLS ( HFAST(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Read from writable files in interleaved order' )


      DO I = 1, CLNWDS(1)

         DO K = 1, NWRIT

            HANDLE = HWRIT(K)

            CALL DASRDI ( HANDLE, I, I, IVAL_A1 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( IVAL .NE. I ) THEN
               CALL CHCKSI ( 'IVAL', IVAL, '=', I, 0, OK )
            END IF

!
!           Include a lookup in a complex, unsegregated, read-only
!           file.
!
            CALL DASA2L ( BIGHAN, INT,   1,     CLBASE, &
                          CLSIZE, RECNO, WORDNO        )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

      DO I = 1, CLNWDS(2)

         DO K = 1, NWRIT

            HANDLE = HWRIT(K)

            CALL DASRDC ( HANDLE, I, I, 1, 1, CVAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            J = MOD(I,128)

            IF ( ICHAR(CVAL) .NE. J ) THEN
               CALL CHCKSC ( 'CVAL', CVAL, '=', CHAR(J), OK )
            END IF

            CALL DASA2L ( BIGHAN, INT,   1,     CLBASE, &
                          CLSIZE, RECNO, WORDNO        )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

      DO I = 1, CLNWDS(3)

         DO K = 1, NWRIT

            HANDLE = HWRIT(K)

            CALL DASRDD ( HANDLE, I, I, DVAL_A1 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( DVAL .NE. DBLE(I) ) THEN
               CALL CHCKSD ( 'DVAL', DVAL, '=', DBLE(I), 0.D0, OK )
            END IF

            CALL DASA2L ( BIGHAN, INT,   1,     CLBASE, &
                          CLSIZE, RECNO, WORDNO        )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO


      DO I = 1, CLNWDS(4)

         DO K = 1, NWRIT

            HANDLE = HWRIT(K)

            CALL DASRDI ( HANDLE, I, I, IVAL_A1 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( IVAL .NE. I ) THEN
               CALL CHCKSI ( 'IVAL', IVAL, '=', I, 0, OK )
            END IF

!
!           Include a lookup in a complex, unsegregated, read-only
!           file.
!
            CALL DASA2L ( BIGHAN, INT,   1,     CLBASE, &
                          CLSIZE, RECNO, WORDNO        )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Read from writable files; ' &
      //           'path check. Path case 7: WRITABLE, SAME FILE.' )



      DO I = 1, NWRIT

         HANDLE = HWRIT( I )

!
!        Do one read to get the file's attributes buffered.
!
         CALL P_DASA2L ( HANDLE, DP,    1, CLBASE, &
                         CLSIZE, RECNO,    WORDNO )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Create expected path.
!
         CALL SSIZEC ( PTHDEP, EXPPTH )

         CALL APPNDC ( TYPCHK, EXPPTH )
         CALL APPNDC ( SETTBF, EXPPTH )
         CALL APPNDC ( GFSUM0, EXPPTH )
         CALL APPNDC ( ADDCHK, EXPPTH )
         CALL APPNDC ( SLOWSR, EXPPTH )
         CALL APPNDC ( SLOWST, EXPPTH )
         CALL APPNDC ( CALC,   EXPPTH )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Execute instrumented version of DASA2L.
!
!        The file associated with HANDLE is now the last one read.
!
         CALL T_PTHNEW
         CALL P_DASA2L ( HANDLE, DP,    1, CLBASE, &
                         CLSIZE, RECNO,    WORDNO )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check actual path.
!
         CALL SSIZEC ( PTHDEP, PATH )

         CALL T_PTHGET ( PATH )
         CALL T_PTHCMP ( PATH, EXPPTH )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Read from writable files; ' &
      //           'path check. Path case 8: WRITABLE, KNOWN FILE.' )



!
!     Do one read of the last file so the next file read
!     will be a different one.
!
      HANDLE = HWRIT( NWRIT )

      CALL P_DASA2L ( HANDLE, DP,    1, CLBASE, &
                      CLSIZE, RECNO,    WORDNO )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NWRIT

         HANDLE = HWRIT( I )
!
!        Create expected path.
!
         CALL SSIZEC ( PTHDEP, EXPPTH )

         CALL APPNDC ( TYPCHK, EXPPTH )
         CALL APPNDC ( NOTSAM, EXPPTH )
         CALL APPNDC ( SETTBF, EXPPTH )
         CALL APPNDC ( GFSUM0, EXPPTH )
         CALL APPNDC ( ADDCHK, EXPPTH )
         CALL APPNDC ( SLOWSR, EXPPTH )
         CALL APPNDC ( SLOWST, EXPPTH )
         CALL APPNDC ( CALC,   EXPPTH )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Execute instrumented version of DASA2L.
!
!        The file associated with HANDLE is now the last one read.
!
         CALL T_PTHNEW
         CALL P_DASA2L ( HANDLE, DP,    1, CLBASE, &
                         CLSIZE, RECNO,    WORDNO )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check actual path.
!
         CALL SSIZEC ( PTHDEP, PATH )

         CALL T_PTHGET ( PATH )
         CALL T_PTHCMP ( PATH, EXPPTH )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Read from writable files; ' &
      //           'path check. Path case 9: WRITABLE, UNKNOWN FILE.' )


!
!     Close the files and re-open them for write access.
!
      DO I = 1, NWRIT
!
!        Close the file without segregating it.
!
         CALL DASWBR ( HWRIT(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL DASLLC ( HWRIT(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NWRIT

         CALL DASOPW ( WRTABL(I), HWRIT(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      DO I = 1, NWRIT

         HANDLE = HWRIT( I )
!
!        Create expected path.
!
         CALL SSIZEC ( PTHDEP, EXPPTH )

         CALL APPNDC ( TYPCHK, EXPPTH )
         CALL APPNDC ( NOTSAM, EXPPTH )
         CALL APPNDC ( UNKNWN, EXPPTH )
         CALL APPNDC ( BUFSHF, EXPPTH )
         CALL APPNDC ( BUFINS, EXPPTH )
         CALL APPNDC ( SETTBF, EXPPTH )
         CALL APPNDC ( GETACC, EXPPTH )
         CALL APPNDC ( GFSUM0, EXPPTH )
         CALL APPNDC ( ADDCHK, EXPPTH )
         CALL APPNDC ( SLOWSR, EXPPTH )
         CALL APPNDC ( SLOWST, EXPPTH )
         CALL APPNDC ( CALC,   EXPPTH )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Execute instrumented version of DASA2L.
!
!        The file associated with HANDLE is now the last one read.
!
         CALL T_PTHNEW
         CALL P_DASA2L ( HANDLE, DP,    1, CLBASE, &
                         CLSIZE, RECNO,    WORDNO )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check actual path.
!
         CALL SSIZEC ( PTHDEP, PATH )

         CALL T_PTHGET ( PATH )
         CALL T_PTHCMP ( PATH, EXPPTH )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Read from read-only, unsegregated files in ' &
      //           'interleaved order' )

      DO K = 1, NRDNSG
         CALL DASOPR ( RDNSEG(K), HRDNSG(K) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      DO I = 1, CLNWDS(1)

         DO K = 1, NRDNSG

            HANDLE = HRDNSG(K)


            CALL DASRDI ( HANDLE, I, I, IVAL_A1 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( IVAL .NE. I ) THEN
               CALL CHCKSI ( 'IVAL', IVAL, '=', I, 0, OK )
            END IF

            CALL DASA2L ( BIGHAN, INT,   257,  CLBASE, &
                          CLSIZE, RECNO, WORDNO        )
            CALL CHCKXC ( .FALSE., ' ', OK )


         END DO

      END DO

      DO I = 1, CLNWDS(2)

         DO K = 1, NRDNSG

            HANDLE = HRDNSG(K)

            CALL DASRDC ( HANDLE, I, I, 1, 1, CVAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            J = MOD(I,128)

            IF ( ICHAR(CVAL) .NE. J ) THEN
               CALL CHCKSC ( 'CVAL', CVAL, '=', CHAR(J), OK )
            END IF

            CALL DASA2L ( BIGHAN, INT,   257,   CLBASE, &
                          CLSIZE, RECNO, WORDNO        )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

      DO I = 1, CLNWDS(3)

         DO K = 1, NRDNSG

            HANDLE = HRDNSG(K)

            CALL DASRDD ( HANDLE, I, I, DVAL_A1 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( DVAL .NE. DBLE(I) ) THEN
               CALL CHCKSD ( 'DVAL', DVAL, '=', DBLE(I), 0.D0, OK )
            END IF

            CALL DASA2L ( BIGHAN, INT,   257,   CLBASE, &
                          CLSIZE, RECNO, WORDNO        )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO


      DO I = 1, CLNWDS(4)

         DO K = 1, NRDNSG

            HANDLE = HRDNSG(K)


            CALL DASRDI ( HANDLE, I, I, IVAL_A1 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( IVAL .NE. I ) THEN
               CALL CHCKSI ( 'IVAL', IVAL, '=', I, 0, OK )
            END IF

            CALL DASA2L ( BIGHAN, INT,   257,  CLBASE, &
                          CLSIZE, RECNO, WORDNO        )
            CALL CHCKXC ( .FALSE., ' ', OK )


         END DO

      END DO

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Read from read-only, unsegregated files twice; ' &
      //           'path check. Path case 4: READONLY, SAME FILE.'  )


      DO I = 1, NRDNSG
!
!        Do one read from the current file.
!
         HANDLE = HRDNSG(I)

         CALL P_DASA2L ( HANDLE, DP,    1, CLBASE, &
                         CLSIZE, RECNO,    WORDNO )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Create expected path.
!
         CALL SSIZEC ( PTHDEP, EXPPTH )

         CALL APPNDC ( TYPCHK, EXPPTH )
         CALL APPNDC ( SETTBF, EXPPTH )
         CALL APPNDC ( ADDCHK, EXPPTH )
         CALL APPNDC ( SLOWSR, EXPPTH )
         CALL APPNDC ( SLOWST, EXPPTH )
         CALL APPNDC ( CALC,   EXPPTH )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Execute instrumented version of DASA2L.
!
!        The file associated with HANDLE is now the last one read.
!
         CALL T_PTHNEW
         CALL P_DASA2L ( HANDLE, DP,    1, CLBASE, &
                         CLSIZE, RECNO,    WORDNO )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check actual path.
!
         CALL SSIZEC ( PTHDEP, PATH )

         CALL T_PTHGET ( PATH )
         CALL T_PTHCMP ( PATH, EXPPTH )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Read from read-only, unsegregated files; ' &
      //           'path check. Path case 5: READONLY, KNOWN FILE.'  )


!
!     Do one read from the last file. This ensures that the
!     first file read in the loop below is not the same
!     as that previously read.
!
      HANDLE = HRDNSG( NRDNSG )

      CALL P_DASA2L ( HANDLE, DP,    1, CLBASE, &
                      CLSIZE, RECNO,    WORDNO )
      CALL CHCKXC ( .FALSE., ' ', OK )



      DO I = 1, NRDNSG

         HANDLE = HRDNSG( I )

!
!        Create expected path.
!
         CALL SSIZEC ( PTHDEP, EXPPTH )

         CALL APPNDC ( TYPCHK, EXPPTH )
         CALL APPNDC ( NOTSAM, EXPPTH )
         CALL APPNDC ( SETTBF, EXPPTH )
         CALL APPNDC ( ADDCHK, EXPPTH )
         CALL APPNDC ( SLOWSR, EXPPTH )
         CALL APPNDC ( SLOWST, EXPPTH )
         CALL APPNDC ( CALC,   EXPPTH )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Execute instrumented version of DASA2L.
!
!        The file associated with HANDLE is now the last one read.
!
         CALL T_PTHNEW
         CALL P_DASA2L ( HANDLE, DP,    1, CLBASE, &
                         CLSIZE, RECNO,    WORDNO )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check actual path.
!
         CALL SSIZEC ( PTHDEP, PATH )

         CALL T_PTHGET ( PATH )
         CALL T_PTHCMP ( PATH, EXPPTH )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Read from read-only, unsegregated files; ' &
      //           'path check. Path case 6: READONLY, UNKNOWN FILE.' )


!
!     Close the files and re-open them for read access. All files
!     will be unknown.
!
      DO I = 1, NRDNSG
         CALL DASCLS ( HRDNSG(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      DO I = 1, NRDNSG
         CALL DASOPR ( RDNSEG(I), HRDNSG(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO


      DO I = 1, NRDNSG

         HANDLE = HRDNSG( I )

!
!        Create expected path.
!
         CALL SSIZEC ( PTHDEP, EXPPTH )

         CALL APPNDC ( TYPCHK, EXPPTH )
         CALL APPNDC ( NOTSAM, EXPPTH )
         CALL APPNDC ( UNKNWN, EXPPTH )
         CALL APPNDC ( BUFSHF, EXPPTH )
         CALL APPNDC ( BUFINS, EXPPTH )
         CALL APPNDC ( SETTBF, EXPPTH )
         CALL APPNDC ( GETACC, EXPPTH )
         CALL APPNDC ( GFSUM0, EXPPTH )
         CALL APPNDC ( SEGCHK, EXPPTH )
         CALL APPNDC ( SETTBF, EXPPTH )
         CALL APPNDC ( ADDCHK, EXPPTH )
         CALL APPNDC ( SLOWSR, EXPPTH )
         CALL APPNDC ( SLOWST, EXPPTH )
         CALL APPNDC ( CALC,   EXPPTH )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Execute instrumented version of DASA2L.
!
!        The file associated with HANDLE is now the last one read.
!
         CALL T_PTHNEW
         CALL P_DASA2L ( HANDLE, DP,    1, CLBASE, &
                         CLSIZE, RECNO,    WORDNO )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check actual path.
!
         CALL SSIZEC ( PTHDEP, PATH )

         CALL T_PTHGET ( PATH )
         CALL T_PTHCMP ( PATH, EXPPTH )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Read from fast files in interleaved order' )

!
!     Close the read-only, unsegregated files.
!
      DO K = 1, NRDNSG
         CALL DASCLS ( HRDNSG(K) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO


      DO K = 1, NFAST
         CALL DASOPR ( FAST(K), HFAST(K) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      DO I = 1, CLNWDS(1)

         DO K = 1, NFAST

            HANDLE = HFAST(K)


            CALL DASRDI ( HANDLE, I, I, IVAL_A1 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( IVAL .NE. I ) THEN
               CALL CHCKSI ( 'IVAL', IVAL, '=', I, 0, OK )
            END IF

!
!           Include a lookup in a complex, unsegregated, read-only
!           file.
!
            CALL DASA2L ( BIGHAN, INT,   257,   CLBASE, &
                          CLSIZE, RECNO, WORDNO        )
            CALL CHCKXC ( .FALSE., ' ', OK )


         END DO

      END DO

      DO I = 1, CLNWDS(2)

         DO K = 1, NFAST

            HANDLE = HFAST(K)

            CALL DASRDC ( HANDLE, I, I, 1, 1, CVAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            J = MOD(I,128)

            IF ( ICHAR(CVAL) .NE. J ) THEN
               CALL CHCKSC ( 'CVAL', CVAL, '=', CHAR(J), OK )
            END IF

            CALL DASA2L ( BIGHAN, INT,   257,   CLBASE, &
                          CLSIZE, RECNO, WORDNO        )
            CALL CHCKXC ( .FALSE., ' ', OK )


         END DO

      END DO

      DO I = 1, CLNWDS(3)

         DO K = 1, NFAST

            HANDLE = HFAST(K)

            CALL DASRDD ( HANDLE, I, I, DVAL_A1 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( DVAL .NE. DBLE(I) ) THEN
               CALL CHCKSD ( 'DVAL', DVAL, '=', DBLE(I), 0.D0, OK )
            END IF

            CALL DASA2L ( BIGHAN, INT,   257,     CLBASE, &
                          CLSIZE, RECNO, WORDNO        )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

      DO I = 1, CLNWDS(4)

         DO K = 1, NFAST

            HANDLE = HFAST(K)


            CALL DASRDI ( HANDLE, I, I, IVAL_A1 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( IVAL .NE. I ) THEN
               CALL CHCKSI ( 'IVAL', IVAL, '=', I, 0, OK )
            END IF

!
!           Include a lookup in a complex, unsegregated, read-only
!           file.
!
            CALL DASA2L ( BIGHAN, INT,   257,   CLBASE, &
                          CLSIZE, RECNO, WORDNO        )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Read from fast files twice; ' &
      //           'path check. Path case 1: FAST, SAME FILE.'  )


      DO I = 1, NFAST
!
!        Do one read from the current file.
!
         HANDLE = HFAST(I)

         CALL P_DASA2L ( HANDLE, DP,    1, CLBASE, &
                      CLSIZE, RECNO,    WORDNO )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Create expected path.
!
         CALL SSIZEC ( PTHDEP, EXPPTH )

         CALL APPNDC ( TYPCHK, EXPPTH )
         CALL APPNDC ( ADDCHK, EXPPTH )
         CALL APPNDC ( FASTST, EXPPTH )
         CALL APPNDC ( CALC,   EXPPTH )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Execute instrumented version of DASA2L.
!
!        The file associated with HANDLE is now the last one read.
!
         CALL T_PTHNEW
         CALL P_DASA2L ( HANDLE, DP,    1, CLBASE, &
                      CLSIZE, RECNO,    WORDNO )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check actual path.
!
         CALL SSIZEC ( PTHDEP, PATH )

         CALL T_PTHGET ( PATH )
         CALL T_PTHCMP ( PATH, EXPPTH )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Read from fast files in interleaved order: ' &
      //           'path check. Path case 2: FAST, KNOWN FILE.'  )

!
!     Prep the P_DASA2L buffer by reading from each
!     fast file.
!
      DO I = 1, NFAST

         HANDLE = HFAST(I)

         CALL T_PTHNEW
         CALL P_DASA2L ( HANDLE, DP,    1, CLBASE, &
                         CLSIZE, RECNO,    WORDNO )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      DO I = 1, NFAST
!
!        This file is not the last one read.
!
         HANDLE = HFAST(I)

!
!        Create expected path.
!
         CALL SSIZEC ( PTHDEP, EXPPTH )

         CALL APPNDC ( TYPCHK, EXPPTH )
         CALL APPNDC ( NOTSAM, EXPPTH )
         CALL APPNDC ( SETTBF, EXPPTH )
         CALL APPNDC ( ADDCHK, EXPPTH )
         CALL APPNDC ( FASTST, EXPPTH )
         CALL APPNDC ( CALC,   EXPPTH )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Execute instrumented version of DASA2L.
!
         CALL T_PTHNEW
         CALL P_DASA2L ( HANDLE, DP,    1, CLBASE, &
                      CLSIZE, RECNO,    WORDNO )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check actual path.
!
         CALL SSIZEC ( PTHDEP, PATH )

         CALL T_PTHGET ( PATH )
         CALL T_PTHCMP ( PATH, EXPPTH )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!     CALL T_PTHDSP ( PATH )



! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Read from newly opened fast files; ' &
      //           'path check. Path case 3: FAST, UNKNOWN FILE.'  )


      DO I = 1, NFAST
         CALL DASCLS ( HFAST(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      DO I = 1, NFAST

         CALL DASOPR ( FAST(I), HFAST(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Create expected path.
!
         CALL SSIZEC ( PTHDEP, EXPPTH )

         CALL APPNDC ( TYPCHK, EXPPTH )
         CALL APPNDC ( NOTSAM, EXPPTH )
         CALL APPNDC ( UNKNWN, EXPPTH )
         CALL APPNDC ( BUFSHF, EXPPTH )
         CALL APPNDC ( BUFINS, EXPPTH )
         CALL APPNDC ( SETTBF, EXPPTH )
         CALL APPNDC ( GETACC, EXPPTH )
         CALL APPNDC ( GFSUM0, EXPPTH )
         CALL APPNDC ( SEGCHK, EXPPTH )
         CALL APPNDC ( SETTBF, EXPPTH )
         CALL APPNDC ( ADDCHK, EXPPTH )
         CALL APPNDC ( FASTST, EXPPTH )
         CALL APPNDC ( CALC,   EXPPTH )

         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Do one read from the current file.
!
         CALL T_PTHNEW

         HANDLE = HFAST(I)

         CALL P_DASA2L ( HANDLE, DP,    1, CLBASE, &
                      CLSIZE, RECNO,    WORDNO )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check actual path.
!
         CALL SSIZEC ( PTHDEP, PATH )

         CALL T_PTHGET ( PATH )
         CALL T_PTHCMP ( PATH, EXPPTH )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Clean up' )

      CALL DELFIL ( DAS0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASWBR ( BIGHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DASLLC ( BIGHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DAS1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NWRIT

         CALL DASWBR ( HWRIT(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DASLLC ( HWRIT(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( WRTABL(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NRDNSG

         CALL DASCLS ( HRDNSG(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( RDNSEG(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NFAST

         CALL DASCLS ( HFAST(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( FAST(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL T_SUCCESS ( OK )
      RETURN
      END


!**********************************************************************
!
!     P A T H   T E S T   U T I L I T I E S
!
!**********************************************************************



!
!     Utilities for execution path checking:
!
      SUBROUTINE T_PATH ( STR, PATH )
      use pepper_legacy
      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         STR
      CHARACTER*(*)         PATH ( LBCELL : * )


      INTEGER               WIDTH
      PARAMETER           ( WIDTH = 50 )

      INTEGER               DEPTH
      PARAMETER           ( DEPTH = 30 )


      CHARACTER*(WIDTH)     SVPATH ( LBCELL : DEPTH )

      SAVE

      RETURN


!
!     Initialize path.
!
      ENTRY T_PTHNEW

      CALL SSIZEC ( DEPTH, SVPATH )

      RETURN


!
!     Append action to path.
!
      ENTRY T_PTHAPP ( STR )

      CALL APPNDC ( STR, SVPATH )

      RETURN

!
!     Get stored path.
!
      ENTRY T_PTHGET ( PATH )


      CALL COPYC ( SVPATH, PATH )

      END

!
!     Detect path mismatch.
!
      SUBROUTINE T_PTHCMP ( PATH, EXPPTH )
      use pepper_legacy
      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         PATH   ( LBCELL : * )
      CHARACTER*(*)         EXPPTH ( LBCELL : * )


      INTEGER               CP
      INTEGER               CX
      INTEGER               I
      INTEGER               N
      INTEGER               NM


      CALL CHKIN ( 'T_PTHCMP' )

      CP = CARDC( PATH   )
      CX = CARDC( EXPPTH )

      NM = MIN ( CP, CX )

      N  = 0

      DO I = 1, NM

         IF ( PATH(I) .EQ. EXPPTH(I) ) THEN
            N = N + 1
         END IF

      END DO


      IF ( CP .NE. CX ) THEN

         CALL SETMSG ( 'Path was expected to have length ' &
         //            '# but had length #. Last element ' &
         //            'of actual path was <#>. Last element ' &
         //            'common to both paths was <#>.'        )
         CALL ERRINT ( '#', CX                                )
         CALL ERRINT ( '#', CP                                )

         IF ( CP .GT. 0 ) THEN
            CALL ERRCH ( '#', PATH  ( CP ) )
         ELSE
            CALL ERRCH  ( '#', '<empty>'   )
         END IF

         IF ( N .GT. 0 ) THEN
            CALL ERRCH  ( '#', EXPPTH( N ) )
         ELSE
            CALL ERRCH  ( '#', '<empty>'   )
         END IF

         CALL SIGERR ( 'SPICE(BADPATHLENGTH)' )
         CALL CHKOUT ( 'T_PTHCMP'             )
         RETURN

      END IF


      I = 1

      DO WHILE ( I .LE. CARDC(EXPPTH) )

         IF ( PATH(I) .NE. EXPPTH(I) ) THEN

            CALL SETMSG ( 'Path element # was expected to ' &
            //            'be <#> but was <#>.'            )
            CALL ERRINT ( '#', I                           )
            CALL ERRCH  ( '#', EXPPTH(I)                   )
            CALL ERRCH  ( '#', PATH(I)                     )
            CALL SIGERR ( 'SPICE(BADPATH)'                 )
            CALL CHKOUT ( 'T_PTHCMP'                       )
            RETURN

         END IF

         I = I + 1

      END DO

      CALL CHKOUT ( 'T_PTHCMP' )
      RETURN

!
!     Display path.
!
      ENTRY T_PTHDSP ( PATH )


      CALL CHKIN ( 'T_PTHDSP' )

      DO I = 1, CARDC( PATH )
         CALL TOSTDO( PATH(I) )
      END DO

      CALL CHKOUT ( 'T_PTHDSP' )
      RETURN

      END
