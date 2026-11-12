!$Procedure F_DAS ( Test subset of DAS routines )

      SUBROUTINE F_DAS ( OK )
      use pepper_legacy
      use pepper_inc_das

! $ Abstract
!
!     Perform tests on a subset of the SPICELIB DAS routines.
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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests a subset of the SPICELIB DAS routines.
!
!     The subset is:
!
!        DASADC
!        DASADD
!        DASADI
!        DASHAM
!        DASHFS
!        DASLLA
!        DASLLC
!        DASONW
!        DASOPW
!        DASRDC
!        DASRDD
!        DASRDI
!        DASUDC
!        DASUDD
!        DASUDI
!        DASWBR
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
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 21-JUL-2021 (JDR) (BVS) (NJB)
!
!-&


! $ Index_Entries
!
!     test subset of DAS routines
!
!-&


!
!     Local Parameters
!
      CHARACTER*(*)         DASC1
      PARAMETER           ( DASC1   = 'test_char.das'    )

      CHARACTER*(*)         DASD1
      PARAMETER           ( DASD1   = 'test_dp.das'      )

      CHARACTER*(*)         DASTE1
      PARAMETER           ( DASTE1  = 'test_corrupt.das' )

      CHARACTER*(*)         DASI1
      PARAMETER           ( DASI1   = 'test_int.das'     )

      CHARACTER*(*)         WBRTST
      PARAMETER           ( WBRTST  = 'test_daswbr.das'  )

      INTEGER               CHRLEN
      PARAMETER           ( CHRLEN = 50   )

      INTEGER               CSTRLN
      PARAMETER           ( CSTRLN = 2048 )

      INTEGER               DBUFSZ
      PARAMETER           ( DBUFSZ = 30   )

      INTEGER               FTYPLN
      PARAMETER           ( FTYPLN = 3    )

      INTEGER               IBUFSZ
      PARAMETER           ( IBUFSZ = 20   )

      INTEGER               MNSUBS
      PARAMETER           ( MNSUBS = 10   )

      INTEGER               MXNDAT
      PARAMETER           ( MXNDAT = 100  )

!
!     Local Variables
!
      CHARACTER*(5)         ACCESS
      CHARACTER*(CSTRLN)    CDATA  ( MNSUBS )
      CHARACTER*(CHRLEN)    CHRBUF
      CHARACTER*(FTYPLN)    FTYPE
      CHARACTER*(CSTRLN)    RCDATA ( MNSUBS )
      CHARACTER*(CSTRLN)    UCDATA ( MNSUBS )
      CHARACTER*(CHRLEN)    XCHRBF

      DOUBLE PRECISION      DDATA  ( MXNDAT )
      DOUBLE PRECISION      DPBUF  ( DBUFSZ )
      DOUBLE PRECISION      RDDATA ( MXNDAT )
      DOUBLE PRECISION      UDDATA ( MXNDAT )
      DOUBLE PRECISION      XDPBUF ( DBUFSZ )

      INTEGER               BPOS
      INTEGER               EPOS
      INTEGER               FIRST
      INTEGER               FIRSTC
      INTEGER               FIRSTD
      INTEGER               FIRSTI
      INTEGER               FREE
      INTEGER               HANDLE
      INTEGER               HANDL1
      INTEGER               I
      INTEGER               IDATA  ( MXNDAT )
      INTEGER               INTBUF ( IBUFSZ )
      INTEGER               J
      INTEGER               LAST
      INTEGER               LASTC
      INTEGER               LASTD
      INTEGER               LASTI
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
      INTEGER               N
      INTEGER               NCALL
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NPASS
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               PASSNO
      INTEGER               RIDATA ( MXNDAT )
      INTEGER               UIDATA ( MXNDAT )
      INTEGER               XINTBF ( IBUFSZ )
      INTEGER               XLSTLA ( 3 )
      INTEGER               XLSTRC ( 3 )
      INTEGER               XLSTWD ( 3 )

!
!     Saved variables.
!
      SAVE                  FTYPE
      SAVE                  NCALL
      SAVE                  NCOMR
      SAVE                  NPASS
      SAVE                  XLSTLA
      SAVE                  XLSTRC
      SAVE                  XLSTWD

!
!     Initial values, used in the DASWBR/DASLLC test
!
      DATA                  FTYPE  / 'ANG' /
      DATA                  NCALL  / 1000  /
      DATA                  NCOMR  / 10    /
      DATA                  NPASS  / 3     /

!
!     Expected values for DASHFS test
!
      DATA                  XLSTLA /   4,   5,  10 /
      DATA                  XLSTRC / 202, 202, 202 /
      DATA                  XLSTWD /  10,  11,  12 /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_DAS' )

!
!     Start the test family from a clean state. Remove all
!     existing test das files.
!
      CALL KILFIL( DASD1  )
      CALL KILFIL( DASTE1 )
      CALL KILFIL( DASI1  )
      CALL KILFIL( WBRTST )

!
!     *****************************************************************
!
!     DASHAM error cases: try negative and zero handles when no DAS
!     file has yet been opened. Other error cases will be handled
!     while testing other routines.
!
!     *****************************************************************
!
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASHAM error case 1: Verify that DASHAM returns ' &
      //           'an error if we try to get the access type of a ' &
      //           'non existing (negative) handle.' )

      CALL DASHAM ( -1, ACCESS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )

!
!     --- Case: -------------------------------------------------------
!
!     This is a special error case, as the value Zero is used in the
!     initialization of the internal pointers and pools.
!
      CALL TCASE ( 'DASHAM error case 2: Verify that DASHAM returns ' &
      //           'an error if we try to get the access type of a ' &
      //           'non existing (zero) handle.' )

      CALL DASHAM ( 0, ACCESS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )

!
!     *****************************************************************
!
!     DASADC normal cases:
!
!     *****************************************************************
!
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASADC normal case 1: Number of characters to ' // &
                   'add to DAS file is negative.' )

      CALL KILFIL ( DASC1 )

      CALL DASONW ( DASC1, 'TEST', DASC1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N        =  -1
      BPOS     =  10
      EPOS     =  20

      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file and open it again for reading.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the file summary. Before calling DASADC, we have
!
!        FREE   = 3
!        LASTLA = 0
!        LASTRC = 0
!        LASTWD = 0
!
!     Since the DASADC has written no characters words, the
!     result should be the same.
!
      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC, &
                    FREE,   LASTLA, LASTRC, LASTWD       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FREE',   FREE,           '=', 3, 0, OK )
      CALL CHCKSI ( 'LASTLA', LASTLA(CHARDT), '=', 0, 0, OK )
      CALL CHCKSI ( 'LASTRC', LASTRC(CHARDT), '=', 0, 0, OK )
      CALL CHCKSI ( 'LASTWD', LASTWD(CHARDT), '=', 0, 0, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASONW+DASHAM normal case: Verify that DASHAM ' &
      //           'returns the right access mode: READ' )

      CALL DASHAM ( HANDLE, ACCESS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'ACCESS', ACCESS, '=', 'READ', OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASHAM error case 3: Verify that DASHAM returns an ' &
      //           'error if we try to get the access type of a file ' &
      //           'that has been closed.' )

      CALL DASHAM ( HANDLE, ACCESS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )

!
!     --- Case: -------------------------------------------------------
!
!     Create a new DAS file, and write less than 1024 characters
!     from a single string.
!
      CALL TCASE ('DASADC normal case 1: write less than 1024 ' // &
                  'characters from a single string.' )

      CALL KILFIL ( DASC1 )

      CALL DASONW ( DASC1, 'TEST', DASC1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CDATA(1) = '0123456789012345678901234567890123456789'
      CDATA(1)(1023:1023) = '-'

      N    = 1023
      BPOS = 1
      EPOS = N

!
!     Write the character data to the DAS file.
!
      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file and open it again for reading.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the file summary. Before calling DASADC, we have
!
!        FREE   = 3
!        LASTLA = 0
!
!     After calling DASADC, we should have
!
!        FREE   = 4
!        LASTLA = 1023
!
      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC, &
                    FREE,   LASTLA, LASTRC, LASTWD       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FREE',   FREE,           '=', 4,    0, OK )
      CALL CHCKSI ( 'LASTLA', LASTLA(CHARDT), '=', 1023, 0, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     --- Case: -------------------------------------------------------
!
!     Create a new DAS file, and write exactly 1024 characters
!     from a single string.
!
      CALL TCASE ('DASADC normal case 2: write 1024 characters ' // &
                  'from a single string.' )

      CALL KILFIL ( DASC1 )

      CALL DASONW ( DASC1, 'TEST', DASC1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CDATA(1) = '0123456789012345678901234567890123456789'
      CDATA(1)(1023:1024) = '--'

      N    = 1024
      BPOS = 1
      EPOS = N

!
!     Write the character data to the DAS file.
!
      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file and open it again for reading.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the file summary. Before calling DASADC, we have
!
!        FREE   = 3
!        LASTLA = 0
!
!     After calling DASADC, we should have
!
!        FREE   = 4
!        LASTLA = 1024
!
      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC, &
                    FREE,   LASTLA, LASTRC, LASTWD       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FREE',   FREE,           '=', 4,    0, OK )
      CALL CHCKSI ( 'LASTLA', LASTLA(CHARDT), '=', 1024, 0, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     --- Case: -------------------------------------------------------
!
!     Create a new DAS file, and write more than 1024 characters
!     from a single string.
!
      CALL TCASE ('DASADC normal case 2: write more than 1024 ' // &
                  'characters from a single string.' )

      CALL KILFIL ( DASC1 )

      CALL DASONW ( DASC1, 'TEST', DASC1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CDATA(1) = '0123456789012345678901234567890123456789'
      CDATA(1)(1023:1025) = '---'

      N    = 1025
      BPOS = 1
      EPOS = N

!
!     Write the character data to the DAS file.
!
      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file and open it again for reading.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the file summary. Before calling DASADC, we have
!
!        FREE   = 3
!        LASTLA = 0
!
!     After calling DASADC, we should have
!
!        FREE   = 5
!        LASTLA = 1025
!
      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC, &
                    FREE,   LASTLA, LASTRC, LASTWD       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FREE',   FREE,           '=',    5, 0, OK )
      CALL CHCKSI ( 'LASTLA', LASTLA(CHARDT), '=', 1025, 0, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!     DASADC and DASRDC normal cases:
!
!     *****************************************************************
!
!
!     --- Case: -------------------------------------------------------
!
!     Create a new DAS file, and write 121 characters to it. Read the
!     newly created file and check that everything is as expected.
!
      CALL TCASE ('DASADC and DASRDC normal case 1: write 121 '   // &
                  'characters from 40-characters substrings and ' // &
                  'read them.' )

!
!     Open a new DAS file.
!
      CALL KILFIL( DASC1 )

      CALL DASONW ( DASC1, 'TEST', DASC1, 200, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CDATA(1) = '0123456789012345678901234567890123456789'
      CDATA(2) = '0123456789012345678901234567890123456789'
      CDATA(3) = '0123456789012345678901234567890123456789'
      CDATA(4) = '9999999999999999999999999999999999999999'
      N        = 121
      BPOS     = 1
      EPOS     = 40
      FIRST    = 1
      LAST     = N

!
!     Write the character data to the DAS file.
!
      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file and open it again for reading.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Read the data from the DAS file. First clear the RCDATA
!     and update the CDATA(4) string to the expected one.
!
      CALL CLEARC ( 4, RCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CDATA(4) = '9'

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, RCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'RCDATA', RCDATA, '=', CDATA, 4, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     --- Case: -------------------------------------------------------
!
!     Create a new DAS file, and write 120 characters to it. Read the
!     newly created file and check that everything is as expected.
!
      CALL TCASE ('DASADC and DASRDC normal case 2: write 120 ' // &
                  'characters and read them.' )

!
!     Open a new DAS file.
!
      CALL KILFIL( DASC1 )

      CALL DASONW ( DASC1, 'TEST', DASC1, 200, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CDATA(1) = '0123456789012345678901234567890123456789'
      CDATA(2) = '0123456789012345678901234567890123456789'
      CDATA(3) = '0123456789012345678901234567890123456789'
      N        = 120
      BPOS     = 1
      EPOS     = 40
      FIRST    = 1
      LAST     = N

!
!     Write the character data to the DAS file.
!
      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file and open it again for reading.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Read the data from the DAS file. First clear the RCDATA
!
      CALL CLEARC ( 3, RCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, RCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'RCDATA', RCDATA, '=', CDATA, 3, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!     DASUDC normal cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     Update an existing DAS.
!
      CALL TCASE ('DASUDC normal case 1: update existing DAS ' // &
                  'character strings.' )

!
!     Open the DAS file created in the normal case for DASADC/DASRDC
!     to update some of the character addresses. Set the expected
!     updated character string in CDATA(1)
!
      CALL DASOPW ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      UCDATA(1) = '11111'
      UCDATA(2) = '22222'
      CDATA(1)  = '0123456789111112222201234567890123456789'
      FIRST     =  11
      LAST      =  20
      BPOS      =  1
      EPOS      =  5

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, UCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file and open it again for reading.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Read the data from the DAS file. First clear the RCDATA
!
      CALL CLEARC ( 3, RCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      FIRST    = 1
      LAST     = N
      BPOS     = 1
      EPOS     = 40

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, RCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'RCDATA', RCDATA, '=', CDATA, 3, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     --- Case: -------------------------------------------------------
!
!     Update an existing DAS.
!
      CALL TCASE ('DASUDC normal case 2: update existing DAS ' // &
                  'character strings. Only one character.' )

!
!     Open the DAS file created in the normal case for DASADC/DASRDC
!     to update some of the character addresses. Set the expected
!     updated character string in CDATA(1)
!
      CALL DASOPW ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      UCDATA(1) = '19'
      UCDATA(2) = '22222'
      CDATA(1)  = '0923456789111112222201234567890123456789'
      FIRST     =  2
      LAST      =  2
      BPOS      =  2
      EPOS      =  2

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, UCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file and open it again for reading.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Read the data from the DAS file. First clear the RCDATA
!
      CALL CLEARC ( 3, RCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      FIRST    = 1
      LAST     = N
      BPOS     = 1
      EPOS     = 40

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, RCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'RCDATA', RCDATA, '=', CDATA, 3, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     *****************************************************************
!
!     DASADD normal cases:
!
!     *****************************************************************
!
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASADD normal case 1: Number of double ' // &
                   'precision numbers to add to DAS file is ' // &
                   'negative.' )

      CALL KILFIL ( DASD1 )

      CALL DASONW ( DASD1, 'TEST', DASD1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N        =  -1

      CALL DASADD ( HANDLE, N, DDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file and open it again for reading.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASD1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the file summary. Before calling DASADD, we have
!
!        FREE   = 3
!        LASTLA = 0
!        LASTRC = 0
!        LASTWD = 0
!
!     Since the DASADD has written no double precision words, the
!     result should be the same.
!
      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC, &
                    FREE,   LASTLA, LASTRC, LASTWD       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FREE',   FREE,         '=', 3, 0, OK )
      CALL CHCKSI ( 'LASTLA', LASTLA(DPDT), '=', 0, 0, OK )
      CALL CHCKSI ( 'LASTRC', LASTRC(DPDT), '=', 0, 0, OK )
      CALL CHCKSI ( 'LASTWD', LASTWD(DPDT), '=', 0, 0, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!     DASADD and DASRDD normal cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     Create a new DAS file, and write 100 double precision numbers to
!     it. Read the newly created file and check that everything is as
!     expected.
!
      CALL TCASE ('DASADD and DASRDD normal case: write 100 ' // &
                  'double precision numbers and read them.' )

!
!     Open a new DAS file.
!
      CALL KILFIL( DASD1 )

      CALL DASONW ( DASD1, 'TEST', DASD1, 200, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N     = 100
      FIRST = 1
      LAST  = N

      DO I = 1, N
         DDATA(I) = DBLE(I)
      END DO

!
!     Write the double precision data to the DAS file.
!
      CALL DASADD ( HANDLE, N, DDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file and open it again for reading.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASD1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Read the data from the DAS file.
!
      CALL DASRDD ( HANDLE, FIRST, LAST, RDDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'RDDATA', RDDATA, '=', DDATA, N, 0.D0, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!     DASUDD normal cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     Update an existing DAS.
!
      CALL TCASE ('DASUDD normal case: update existing DAS ' // &
                  'double precision numbers.' )

!
!     Open the DAS file created in the normal case for DASADD/DASRDD
!     to update some of the double precision addresses. Set the
!     expected updated sequence of double precision data in DDATA.
!
      CALL DASOPW ( DASD1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      FIRST     =  11
      LAST      =  20

      DO I = 1, 10
         UDDATA(I)   = -99.D0
         DDATA(10+I) = UDDATA(I)
      END DO


      CALL DASUDD ( HANDLE, FIRST, LAST, UDDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file and open it again for reading.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASD1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Read the data from the DAS file.
!
      FIRST    = 1
      LAST     = 100

      CALL DASRDD ( HANDLE, FIRST, LAST, RDDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'RDDATA', RDDATA, '=', DDATA, N, 0.D0, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!     DASRDD normal case:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ('DASRDD normal case: read the newly created DAS ' &
      //          'file with FIRST greater than LAST. DATA shall not ' &
      //          'be changed.' )

!
!     Open the file again for reading.
!
      CALL DASOPR ( DASD1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Clear both DDATA and RDDATA.
!
      CALL CLEARD ( MXNDAT, DDATA  )
      CALL CLEARD ( MXNDAT, RDDATA )

      FIRST = 10
      LAST  = 9

      CALL DASRDD ( HANDLE, FIRST, LAST, RDDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'RDDATA', RDDATA, '=', DDATA, MXNDAT, 0.D0, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!     DASADI normal cases:
!
!     *****************************************************************
!
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASADI normal case 1: Number of integer ' // &
                   'numbers to add to DAS file is negative.' )

      CALL KILFIL ( DASI1 )

      CALL DASONW ( DASI1, 'TEST', DASI1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N        =  -1

      CALL DASADI ( HANDLE, N, IDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file and open it again for reading.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASI1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the file summary. Before calling DASADI, we have
!
!        FREE   = 3
!        LASTLA = 0
!        LASTRC = 0
!        LASTWD = 0
!
!     Since the DASADI has written no integer words, the
!     result should be the same.
!
      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC, &
                    FREE,   LASTLA, LASTRC, LASTWD       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FREE',   FREE,          '=', 3, 0, OK )
      CALL CHCKSI ( 'LASTLA', LASTLA(INTDT), '=', 0, 0, OK )
      CALL CHCKSI ( 'LASTRC', LASTRC(INTDT), '=', 0, 0, OK )
      CALL CHCKSI ( 'LASTWD', LASTWD(INTDT), '=', 0, 0, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!     DASADI and DASRDI normal cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     Create a new DAS file, and write 100 integer numbers to it.
!     Read the newly created file and check that everything is as
!     expected.
!
      CALL TCASE ('DASADI and DASRDI normal case: write 100 ' // &
                  'integer numbers and read them.' )

!
!     Open a new DAS file.
!
      CALL KILFIL( DASI1 )

      CALL DASONW ( DASI1, 'TEST', DASI1, 200, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N     = 100
      FIRST = 1
      LAST  = N

      DO I = 1, N
         IDATA(I) = I
      END DO

!
!     Write the integer data to the DAS file.
!
      CALL DASADI ( HANDLE, N, IDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file and open it again for reading.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASI1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Read the data from the DAS file.
!
      CALL DASRDI ( HANDLE, FIRST, LAST, RIDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAI ( 'RIDATA', RIDATA, '=', IDATA, N, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!     DASUDI normal cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     Update an existing DAS.
!
      CALL TCASE ('DASUDI normal case: update existing DAS ' // &
                  'integer numbers.' )

!
!     Open the DAS file created in the normal case for DASADI/DASRDI
!     to update some of the integer addresses. Set the expected
!     updated sequence of integer data in IDATA.
!
      CALL DASOPW ( DASI1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      FIRST     =  11
      LAST      =  20

      DO I = 1, 10
         UIDATA(I)   = -99
         IDATA(10+I) = UIDATA(I)
      END DO


      CALL DASUDI ( HANDLE, FIRST, LAST, UIDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file and open it again for reading.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASI1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Read the data from the DAS file.
!
      FIRST    = 1
      LAST     = 100

      CALL DASRDI ( HANDLE, FIRST, LAST, RIDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAI ( 'RIDATA', RIDATA, '=', IDATA, N, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!     DASRDI normal case:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ('DASRDI normal case: read the newly created DAS ' &
      //          'file with FIRST greater than LAST. DATA shall not ' &
      //          'be changed.' )

!
!     Open the file again for reading.
!
      CALL DASOPR ( DASI1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Clear both IDATA and RIDATA.
!
      CALL CLEARI ( MXNDAT, IDATA  )
      CALL CLEARI ( MXNDAT, RIDATA )

      FIRST = 10
      LAST  = 9

      CALL DASRDI ( HANDLE, FIRST, LAST, RIDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAI ( 'RIDATA', RIDATA, '=', IDATA, MXNDAT, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!     DASHFS normal cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ('DASHFS normal case: add character, integer, and '  // &
                  'double precision data and check the updated file ' // &
                  'summary.' )

!
!     Open a new DAS file.
!
      CALL KILFIL ( WBRTST )

      CALL DASONW ( WBRTST, FTYPE, WBRTST, 200, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, 10
         CALL DASADI ( HANDLE, 1, [I] )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      DO I = 1, 5
         CALL DASADD ( HANDLE, 1, [DBLE(I)] )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      CALL DASADC ( HANDLE, 4, 1, 4, 'SPUD' )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file and open it for reading.
!
      CALL DASCLS ( HANDLE         )
      CALL DASOPR ( WBRTST, HANDLE )

!
!     Obtain the file summary.
!
      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC, &
                    FREE,   LASTLA, LASTRC, LASTWD       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NRESVR', NRESVR, '=',      0, 0, OK )
      CALL CHCKSI ( 'NRESVC', NRESVC, '=',      0, 0, OK )
      CALL CHCKSI ( 'NCOMR',  NCOMR,  '=',    200, 0, OK )
      CALL CHCKSI ( 'NCOMC',  NCOMC,  '=',      0, 0, OK )
      CALL CHCKSI ( 'FREE',   FREE,   '=',    206, 0, OK )
      CALL CHCKAI ( 'LASTLA', LASTLA, '=', XLSTLA, 3, OK )
      CALL CHCKAI ( 'LASTRC', LASTRC, '=', XLSTRC, 3, OK )
      CALL CHCKAI ( 'LASTWD', LASTWD, '=', XLSTWD, 3, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!     DASLLA normal cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ('DASLLA normal case: add character, integer, and ' // &
                  'double precision data and check last logical '    // &
                  'addresses for each data type.' )

!
!     Open a new DAS file.
!
      CALL KILFIL ( WBRTST )

      CALL DASONW ( WBRTST, FTYPE, WBRTST, NCOMR, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, 10
         CALL DASADI ( HANDLE, 1, [I] )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      DO I = 1, 5
         CALL DASADD ( HANDLE, 1, [DBLE(I)] )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      CALL DASADC ( HANDLE, 4, 1, 4, 'SPUD' )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now check the logical address ranges.
!
      CALL DASLLA ( HANDLE, LASTC, LASTD, LASTI )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'LASTC', LASTC, '=',  4, 0, OK )
      CALL CHCKSI ( 'LASTD', LASTD, '=',  5, 0, OK )
      CALL CHCKSI ( 'LASTI', LASTI, '=', 10, 0, OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     *****************************************************************
!
!     DASLLC and DASWBR normal cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ('DASLLC and DASWBR normal case: adding '         // &
                  'character, integer, and double precision data ' // &
                  'in interleaved fashion.' )

!
!     Open a new DAS file.
!
      CALL KILFIL ( WBRTST )

      CALL DASONW ( WBRTST, FTYPE, WBRTST, NCOMR, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add data of character, integer, and double precision
!     types to the file in interleaved fashion. We'll add to
!     the file over NPASS "passes," in each of which we close
!     the file after writing.
!
      DO PASSNO = 1, NPASS

         IF ( PASSNO .GT. 1 ) THEN

            CALL DASOPW ( WBRTST, HANDLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         DO I = 1, NCALL
!
!           Add string data to the file.
!
            CHRBUF = 'Character value #'
            CALL REPMI  ( CHRBUF, '#', I, CHRBUF )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CHRBUF(CHRLEN-3:CHRLEN) = '--->'

            CALL DASADC ( HANDLE, CHRLEN, 1, CHRLEN, CHRBUF )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Add double precision data to the file.
!
            DO J = 1, DBUFSZ
               DPBUF(J) = DBLE( 100000000*PASSNO  +  100*I  +  J )
            END DO

            CALL DASADD ( HANDLE, DBUFSZ, DPBUF )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Add integer data to the file.
!
            DO J = 1, IBUFSZ
               INTBUF(J) = 100000000*PASSNO  +  100 * I  +  J
            END DO

            CALL DASADI ( HANDLE, IBUFSZ, INTBUF )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

!
!        Write buffered data to the file.
!
         CALL DASWBR ( HANDLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Close the file without segregating it.
!
         CALL DASLLC ( HANDLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check file contents.
!
      CALL DASOPR ( WBRTST, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Read data from the file; compare to expected values.
!
!     Initialize end addresses.
!
      LASTC = 0
      LASTD = 0
      LASTI = 0

      DO PASSNO = 1, NPASS

         DO I = 1, NCALL
!
!           Check string data.
!
            XCHRBF = 'Character value #'
            CALL REPMI( XCHRBF, '#', I, XCHRBF )
            CALL CHCKXC ( .FALSE., ' ', OK )

            XCHRBF(CHRLEN-3:CHRLEN) = '--->'

            FIRSTC = LASTC + 1
            LASTC  = LASTC + CHRLEN

            CALL DASRDC ( HANDLE, FIRSTC, LASTC, 1, CHRLEN, CHRBUF )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSC ( XCHRBF, CHRBUF, '=', XCHRBF, OK )

!
!           Check double precision data.
!
            DO J = 1, DBUFSZ
               XDPBUF(J) = DBLE( 100000000*PASSNO  +  100*I  +  J )
            END DO

            FIRSTD = LASTD + 1
            LASTD  = LASTD + DBUFSZ

            CALL DASRDD ( HANDLE, FIRSTD, LASTD, DPBUF )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( 'XDPBUF', DPBUF,  '=', &
                           XDPBUF,  DBUFSZ, 0.D0, OK )

!
!           Check integer data.
!
            DO J = 1, IBUFSZ
               XINTBF(J) = 100000000*PASSNO  +  100 * I  +  J
            END DO

            FIRSTI = LASTI + 1
            LASTI  = LASTI + IBUFSZ

            CALL DASRDI ( HANDLE, FIRSTI, LASTI, INTBUF )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAI ( 'XINTBF', INTBUF, '=', XINTBF, IBUFSZ, OK )

         END DO

      END DO

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     *****************************************************************
!
!     DASADC error cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASADC error case #1: begin position of ' // &
                   'substring less than 1.' )

!
!     Open a new DAS file for testing the error cases.
!
      CALL DASONW ( DASTE1, 'TEST', DASTE1, 200, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CDATA(1) = '012345678901234567890123456789'
      N        =  30
      BPOS     =  0
      EPOS     =  20

      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASADC error case #2: end position of ' // &
                   'substring less than 1.' )

      CDATA(1) = '012345678901234567890123456789'
      N        =  30
      BPOS     =  1
      EPOS     =  0

      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASADC error case #3: begin position of ' // &
                   'substring greater than length of substring.' )

      CDATA(1) = '012345678901234567890123456789'
      N        =  30
      BPOS     =  CSTRLN + 1
      EPOS     =  30

      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASADC error case #4: end position of ' // &
                   'substring greater than length of substring.' )

      CDATA(1) = '012345678901234567890123456789'
      N        =  30
      BPOS     =  1
      EPOS     =  CSTRLN + 1

      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASADC error case #5: begin position greater ' // &
                   'than end position of substring.' )

      CDATA(1) = '012345678901234567890123456789'
      N        =  30
      BPOS     =  25
      EPOS     =  20

      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASADC error case #6: Trying to add characters ' // &
                   'to a DAS file that has not been open.' )

      N        =  1
      BPOS     =  10
      EPOS     =  20

      CALL DASADC ( 1000, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHHANDLE)', OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!     DASADD error cases: None.
!
!     *****************************************************************
!
!     *****************************************************************
!
!     DASADI error cases: None.
!
!     *****************************************************************
!
!     *****************************************************************
!
!     DASRDC error cases: None.
!
!     *****************************************************************
!
!     *****************************************************************
!
!     DASHFS error cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASHFS error case #1: DAS file is not open.' )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC, &
                    FREE,   LASTLA, LASTRC, LASTWD       )
      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHHANDLE)', OK )

!
!     *****************************************************************
!
!     DASLLA error cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASLLA error case #1: DAS file is not open.' )

      CALL DASLLA ( HANDLE, LASTC, LASTD, LASTI )
      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHHANDLE)', OK )

!
!     *****************************************************************
!
!     DASONW error cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASONW error case #1: blank filename.' )

      CALL DASONW ( '    ', 'TEST', DASD1, 200, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKFILENAME)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASONW error case #2: blank file type.' )

      CALL DASONW ( DASD1, '    ', DASD1, 200, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKFILETYPE)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASONW error case #3: nonprinting character ' &
              //   'case #1.' )

      CALL DASONW ( DASD1, CHAR(31), DASD1, 200, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(ILLEGALCHARACTER)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASONW error case #4: nonprinting character ' &
              //   'case #2.' )

      CALL DASONW ( DASD1, CHAR(127), DASD1, 200, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(ILLEGALCHARACTER)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASONW error case #5: negative number of comment ' &
              //   'records allocated.' )

      CALL DASONW ( DASD1, 'TEST', DASD1, -1, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

!
!     --- Case: -------------------------------------------------------
!
!     Testing of SPICE(DASFTFULL)
!
!
!     *****************************************************************
!
!     DASOPW error cases:
!
!     *****************************************************************
!
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASOPW error case #1: blank filename.' )

      CALL DASOPW ( ' ', HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKFILENAME)', OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASOPW error case #2: specified file does not ' &
              //   'exist.' )

      CALL DASOPW ( 'UNKNOWN.TXT', HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(FILENOTFOUND)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASOPW error case #3: specified file already ' &
              //   'opened.' )

      CALL DASOPR ( DASD1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPW ( DASD1, HANDL1 )
      CALL CHCKXC ( .TRUE., 'SPICE(FILEOPENCONFLICT)', OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     --- Case: -------------------------------------------------------
!
!     Testing of SPICE(DASFTFULL)
!
!     *****************************************************************
!
!     DASRDC error cases:
!
!     *****************************************************************
!
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASRDC error case #1: begin position of ' // &
                   'substring less than 1.' )

!
!     Open the test DAS file created during the DASADC case
!     for testing the error cases.
!
      CALL DASOPR ( DASTE1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      FIRST    =  1
      LAST     =  30
      BPOS     =  0
      EPOS     =  20

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASRDC error case #2: end position of ' // &
                   'substring less than 1.' )

      FIRST    =  1
      LAST     =  30
      BPOS     =  1
      EPOS     =  0

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASRDC error case #3: begin position of ' // &
                   'substring greater than length of substring.' )

      FIRST    =  1
      LAST     =  30
      BPOS     =  CSTRLN + 1
      EPOS     =  30

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASRDC error case #4: end position of ' // &
                   'substring greater than length of substring.' )

      FIRST    =  1
      LAST     =  30
      BPOS     =  1
      EPOS     =  CSTRLN + 1

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASRDC error case #5: begin position greater ' // &
                   'than end position of substring.' )

      FIRST    =  1
      LAST     =  30
      BPOS     =  25
      EPOS     =  20

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!     DASRDD error cases: None.
!
!     *****************************************************************
!
!     *****************************************************************
!
!     DASRDI error cases: None.
!
!     *****************************************************************
!
!     *****************************************************************
!
!     DASUDC error cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASUDC error case #1: first logical address of ' // &
                   'DAS less than 1.' )

!
!     Open the test DAS file created during the DASADC normal
!     case #1. Note that 120 characters are already in the DAS file,
!     therefore addresses 1:120 are in use.
!
      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      FIRST    =  0
      LAST     =  30
      BPOS     =  1
      EPOS     =  40

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDADDRESS)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASUDC error case #2: last logical address of ' // &
                   'DAS less than 1.' )

      FIRST    =  1
      LAST     =  0
      BPOS     =  1
      EPOS     =  40

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDADDRESS)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASUDC error case #3: first logical address of ' // &
                   'DAS greater than last character logical address.' )

      FIRST    =  121
      LAST     =  30
      BPOS     =  1
      EPOS     =  40

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDADDRESS)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASUDC error case #4: last logical address of ' // &
                   'DAS greater than last character logical address.' )

      FIRST    =  1
      LAST     =  121
      BPOS     =  1
      EPOS     =  40

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDADDRESS)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASUDC error case #5: string begin index ' &
      //           'non-positive.' )

      FIRST    =  1
      LAST     =  120
      BPOS     =  0
      EPOS     =  40

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASUDC error case #6: string begin index ' &
      //           'greater than string length.' )

      FIRST    =  1
      LAST     =  120
      BPOS     =  CSTRLN + 1
      EPOS     =  CSTRLN + 1

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASUDC error case #7: string end index ' &
      //           'non-positive.' )

      FIRST    =  1
      LAST     =  120
      BPOS     =  1
      EPOS     =  0

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASUDC error case #8: string end index ' &
      //           'greater than string length.' )

      FIRST    =  1
      LAST     =  120
      BPOS     =  1
      EPOS     =  CSTRLN + 1

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'DASUDC error case #9: string end index ' &
      //           'less than begin index.' )

      FIRST    =  1
      LAST     =  120
      BPOS     =  CSTRLN
      EPOS     =  1

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INDICESOUTOFORDER)', OK )

!
!     Close the file.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!     DASWBR error cases: None.
!
!     *****************************************************************
!
!     *****************************************************************
!
!     Clean up
!
!     *****************************************************************
!
!     Remove all the DAF files created during the execution of the
!     tests.
!
      CALL KILFIL( DASC1  )
      CALL KILFIL( DASTE1 )
      CALL KILFIL( DASD1  )
      CALL KILFIL( DASI1  )
      CALL KILFIL( WBRTST )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
