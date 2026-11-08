C$Procedure F_DAS ( Test subset of DAS routines )

      SUBROUTINE F_DAS ( OK )

C$ Abstract
C
C     Perform tests on a subset of the SPICELIB DAS routines.
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

      INCLUDE 'das.inc'

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
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests a subset of the SPICELIB DAS routines.
C
C     The subset is:
C
C        DASADC
C        DASADD
C        DASADI
C        DASHAM
C        DASHFS
C        DASLLA
C        DASLLC
C        DASONW
C        DASOPW
C        DASRDC
C        DASRDD
C        DASRDI
C        DASUDC
C        DASUDD
C        DASUDI
C        DASWBR
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
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 21-JUL-2021 (JDR) (BVS) (NJB)
C
C-&


C$ Index_Entries
C
C     test subset of DAS routines
C
C-&


C
C     Local Parameters
C
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

C
C     Local Variables
C
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

C
C     Saved variables.
C
      SAVE                  FTYPE
      SAVE                  NCALL
      SAVE                  NCOMR
      SAVE                  NPASS
      SAVE                  XLSTLA
      SAVE                  XLSTRC
      SAVE                  XLSTWD

C
C     Initial values, used in the DASWBR/DASLLC test
C
      DATA                  FTYPE  / 'ANG' /
      DATA                  NCALL  / 1000  /
      DATA                  NCOMR  / 10    /
      DATA                  NPASS  / 3     /

C
C     Expected values for DASHFS test
C
      DATA                  XLSTLA /   4,   5,  10 /
      DATA                  XLSTRC / 202, 202, 202 /
      DATA                  XLSTWD /  10,  11,  12 /

C
C     Open the test family.
C
      CALL TOPEN ( 'F_DAS' )

C
C     Start the test family from a clean state. Remove all
C     existing test das files.
C
      CALL KILFIL( DASD1  )
      CALL KILFIL( DASTE1 )
      CALL KILFIL( DASI1  )
      CALL KILFIL( WBRTST )

C
C     *****************************************************************
C
C     DASHAM error cases: try negative and zero handles when no DAS
C     file has yet been opened. Other error cases will be handled
C     while testing other routines.
C
C     *****************************************************************
C
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASHAM error case 1: Verify that DASHAM returns '
     .//           'an error if we try to get the access type of a '
     .//           'non existing (negative) handle.' )

      CALL DASHAM ( -1, ACCESS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )

C
C     --- Case: -------------------------------------------------------
C
C     This is a special error case, as the value Zero is used in the
C     initialization of the internal pointers and pools.
C
      CALL TCASE ( 'DASHAM error case 2: Verify that DASHAM returns '
     .//           'an error if we try to get the access type of a '
     .//           'non existing (zero) handle.' )

      CALL DASHAM ( 0, ACCESS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )

C
C     *****************************************************************
C
C     DASADC normal cases:
C
C     *****************************************************************
C
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASADC normal case 1: Number of characters to ' //
     .             'add to DAS file is negative.' )

      CALL KILFIL ( DASC1 )

      CALL DASONW ( DASC1, 'TEST', DASC1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N        =  -1
      BPOS     =  10
      EPOS     =  20

      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the file and open it again for reading.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the file summary. Before calling DASADC, we have
C
C        FREE   = 3
C        LASTLA = 0
C        LASTRC = 0
C        LASTWD = 0
C
C     Since the DASADC has written no characters words, the
C     result should be the same.
C
      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
     .              FREE,   LASTLA, LASTRC, LASTWD       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FREE',   FREE,           '=', 3, 0, OK )
      CALL CHCKSI ( 'LASTLA', LASTLA(CHARDT), '=', 0, 0, OK )
      CALL CHCKSI ( 'LASTRC', LASTRC(CHARDT), '=', 0, 0, OK )
      CALL CHCKSI ( 'LASTWD', LASTWD(CHARDT), '=', 0, 0, OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASONW+DASHAM normal case: Verify that DASHAM '
     .//           'returns the right access mode: READ' )

      CALL DASHAM ( HANDLE, ACCESS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'ACCESS', ACCESS, '=', 'READ', OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASHAM error case 3: Verify that DASHAM returns an '
     .//           'error if we try to get the access type of a file '
     .//           'that has been closed.' )

      CALL DASHAM ( HANDLE, ACCESS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )

C
C     --- Case: -------------------------------------------------------
C
C     Create a new DAS file, and write less than 1024 characters
C     from a single string.
C
      CALL TCASE ('DASADC normal case 1: write less than 1024 ' //
     .            'characters from a single string.' )

      CALL KILFIL ( DASC1 )

      CALL DASONW ( DASC1, 'TEST', DASC1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CDATA(1) = '0123456789012345678901234567890123456789'
      CDATA(1)(1023:1023) = '-'

      N    = 1023
      BPOS = 1
      EPOS = N

C
C     Write the character data to the DAS file.
C
      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the file and open it again for reading.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the file summary. Before calling DASADC, we have
C
C        FREE   = 3
C        LASTLA = 0
C
C     After calling DASADC, we should have
C
C        FREE   = 4
C        LASTLA = 1023
C
      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
     .              FREE,   LASTLA, LASTRC, LASTWD       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FREE',   FREE,           '=', 4,    0, OK )
      CALL CHCKSI ( 'LASTLA', LASTLA(CHARDT), '=', 1023, 0, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     --- Case: -------------------------------------------------------
C
C     Create a new DAS file, and write exactly 1024 characters
C     from a single string.
C
      CALL TCASE ('DASADC normal case 2: write 1024 characters ' //
     .            'from a single string.' )

      CALL KILFIL ( DASC1 )

      CALL DASONW ( DASC1, 'TEST', DASC1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CDATA(1) = '0123456789012345678901234567890123456789'
      CDATA(1)(1023:1024) = '--'

      N    = 1024
      BPOS = 1
      EPOS = N

C
C     Write the character data to the DAS file.
C
      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the file and open it again for reading.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the file summary. Before calling DASADC, we have
C
C        FREE   = 3
C        LASTLA = 0
C
C     After calling DASADC, we should have
C
C        FREE   = 4
C        LASTLA = 1024
C
      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
     .              FREE,   LASTLA, LASTRC, LASTWD       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FREE',   FREE,           '=', 4,    0, OK )
      CALL CHCKSI ( 'LASTLA', LASTLA(CHARDT), '=', 1024, 0, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     --- Case: -------------------------------------------------------
C
C     Create a new DAS file, and write more than 1024 characters
C     from a single string.
C
      CALL TCASE ('DASADC normal case 2: write more than 1024 ' //
     .            'characters from a single string.' )

      CALL KILFIL ( DASC1 )

      CALL DASONW ( DASC1, 'TEST', DASC1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CDATA(1) = '0123456789012345678901234567890123456789'
      CDATA(1)(1023:1025) = '---'

      N    = 1025
      BPOS = 1
      EPOS = N

C
C     Write the character data to the DAS file.
C
      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the file and open it again for reading.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the file summary. Before calling DASADC, we have
C
C        FREE   = 3
C        LASTLA = 0
C
C     After calling DASADC, we should have
C
C        FREE   = 5
C        LASTLA = 1025
C
      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
     .              FREE,   LASTLA, LASTRC, LASTWD       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FREE',   FREE,           '=',    5, 0, OK )
      CALL CHCKSI ( 'LASTLA', LASTLA(CHARDT), '=', 1025, 0, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     DASADC and DASRDC normal cases:
C
C     *****************************************************************
C
C
C     --- Case: -------------------------------------------------------
C
C     Create a new DAS file, and write 121 characters to it. Read the
C     newly created file and check that everything is as expected.
C
      CALL TCASE ('DASADC and DASRDC normal case 1: write 121 '   //
     .            'characters from 40-characters substrings and ' //
     .            'read them.' )

C
C     Open a new DAS file.
C
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

C
C     Write the character data to the DAS file.
C
      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the file and open it again for reading.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Read the data from the DAS file. First clear the RCDATA
C     and update the CDATA(4) string to the expected one.
C
      CALL CLEARC ( 4, RCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CDATA(4) = '9'

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, RCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'RCDATA', RCDATA, '=', CDATA, 4, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     --- Case: -------------------------------------------------------
C
C     Create a new DAS file, and write 120 characters to it. Read the
C     newly created file and check that everything is as expected.
C
      CALL TCASE ('DASADC and DASRDC normal case 2: write 120 ' //
     .            'characters and read them.' )

C
C     Open a new DAS file.
C
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

C
C     Write the character data to the DAS file.
C
      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the file and open it again for reading.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Read the data from the DAS file. First clear the RCDATA
C
      CALL CLEARC ( 3, RCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, RCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'RCDATA', RCDATA, '=', CDATA, 3, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     DASUDC normal cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     Update an existing DAS.
C
      CALL TCASE ('DASUDC normal case 1: update existing DAS ' //
     .            'character strings.' )

C
C     Open the DAS file created in the normal case for DASADC/DASRDC
C     to update some of the character addresses. Set the expected
C     updated character string in CDATA(1)
C
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

C
C     Close the file and open it again for reading.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Read the data from the DAS file. First clear the RCDATA
C
      CALL CLEARC ( 3, RCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      FIRST    = 1
      LAST     = N
      BPOS     = 1
      EPOS     = 40

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, RCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'RCDATA', RCDATA, '=', CDATA, 3, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     --- Case: -------------------------------------------------------
C
C     Update an existing DAS.
C
      CALL TCASE ('DASUDC normal case 2: update existing DAS ' //
     .            'character strings. Only one character.' )

C
C     Open the DAS file created in the normal case for DASADC/DASRDC
C     to update some of the character addresses. Set the expected
C     updated character string in CDATA(1)
C
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

C
C     Close the file and open it again for reading.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Read the data from the DAS file. First clear the RCDATA
C
      CALL CLEARC ( 3, RCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      FIRST    = 1
      LAST     = N
      BPOS     = 1
      EPOS     = 40

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, RCDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'RCDATA', RCDATA, '=', CDATA, 3, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     *****************************************************************
C
C     DASADD normal cases:
C
C     *****************************************************************
C
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASADD normal case 1: Number of double ' //
     .             'precision numbers to add to DAS file is ' //
     .             'negative.' )

      CALL KILFIL ( DASD1 )

      CALL DASONW ( DASD1, 'TEST', DASD1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N        =  -1

      CALL DASADD ( HANDLE, N, DDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the file and open it again for reading.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASD1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the file summary. Before calling DASADD, we have
C
C        FREE   = 3
C        LASTLA = 0
C        LASTRC = 0
C        LASTWD = 0
C
C     Since the DASADD has written no double precision words, the
C     result should be the same.
C
      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
     .              FREE,   LASTLA, LASTRC, LASTWD       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FREE',   FREE,         '=', 3, 0, OK )
      CALL CHCKSI ( 'LASTLA', LASTLA(DPDT), '=', 0, 0, OK )
      CALL CHCKSI ( 'LASTRC', LASTRC(DPDT), '=', 0, 0, OK )
      CALL CHCKSI ( 'LASTWD', LASTWD(DPDT), '=', 0, 0, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     DASADD and DASRDD normal cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     Create a new DAS file, and write 100 double precision numbers to
C     it. Read the newly created file and check that everything is as
C     expected.
C
      CALL TCASE ('DASADD and DASRDD normal case: write 100 ' //
     .            'double precision numbers and read them.' )

C
C     Open a new DAS file.
C
      CALL KILFIL( DASD1 )

      CALL DASONW ( DASD1, 'TEST', DASD1, 200, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N     = 100
      FIRST = 1
      LAST  = N

      DO I = 1, N
         DDATA(I) = DBLE(I)
      END DO

C
C     Write the double precision data to the DAS file.
C
      CALL DASADD ( HANDLE, N, DDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the file and open it again for reading.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASD1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Read the data from the DAS file.
C
      CALL DASRDD ( HANDLE, FIRST, LAST, RDDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'RDDATA', RDDATA, '=', DDATA, N, 0.D0, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     DASUDD normal cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     Update an existing DAS.
C
      CALL TCASE ('DASUDD normal case: update existing DAS ' //
     .            'double precision numbers.' )

C
C     Open the DAS file created in the normal case for DASADD/DASRDD
C     to update some of the double precision addresses. Set the
C     expected updated sequence of double precision data in DDATA.
C
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

C
C     Close the file and open it again for reading.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASD1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Read the data from the DAS file.
C
      FIRST    = 1
      LAST     = 100

      CALL DASRDD ( HANDLE, FIRST, LAST, RDDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'RDDATA', RDDATA, '=', DDATA, N, 0.D0, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     DASRDD normal case:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ('DASRDD normal case: read the newly created DAS '
     .//          'file with FIRST greater than LAST. DATA shall not '
     .//          'be changed.' )

C
C     Open the file again for reading.
C
      CALL DASOPR ( DASD1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Clear both DDATA and RDDATA.
C
      CALL CLEARD ( MXNDAT, DDATA  )
      CALL CLEARD ( MXNDAT, RDDATA )

      FIRST = 10
      LAST  = 9

      CALL DASRDD ( HANDLE, FIRST, LAST, RDDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'RDDATA', RDDATA, '=', DDATA, MXNDAT, 0.D0, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     DASADI normal cases:
C
C     *****************************************************************
C
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASADI normal case 1: Number of integer ' //
     .             'numbers to add to DAS file is negative.' )

      CALL KILFIL ( DASI1 )

      CALL DASONW ( DASI1, 'TEST', DASI1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N        =  -1

      CALL DASADI ( HANDLE, N, IDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the file and open it again for reading.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASI1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the file summary. Before calling DASADI, we have
C
C        FREE   = 3
C        LASTLA = 0
C        LASTRC = 0
C        LASTWD = 0
C
C     Since the DASADI has written no integer words, the
C     result should be the same.
C
      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
     .              FREE,   LASTLA, LASTRC, LASTWD       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FREE',   FREE,          '=', 3, 0, OK )
      CALL CHCKSI ( 'LASTLA', LASTLA(INTDT), '=', 0, 0, OK )
      CALL CHCKSI ( 'LASTRC', LASTRC(INTDT), '=', 0, 0, OK )
      CALL CHCKSI ( 'LASTWD', LASTWD(INTDT), '=', 0, 0, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     DASADI and DASRDI normal cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     Create a new DAS file, and write 100 integer numbers to it.
C     Read the newly created file and check that everything is as
C     expected.
C
      CALL TCASE ('DASADI and DASRDI normal case: write 100 ' //
     .            'integer numbers and read them.' )

C
C     Open a new DAS file.
C
      CALL KILFIL( DASI1 )

      CALL DASONW ( DASI1, 'TEST', DASI1, 200, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N     = 100
      FIRST = 1
      LAST  = N

      DO I = 1, N
         IDATA(I) = I
      END DO

C
C     Write the integer data to the DAS file.
C
      CALL DASADI ( HANDLE, N, IDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the file and open it again for reading.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASI1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Read the data from the DAS file.
C
      CALL DASRDI ( HANDLE, FIRST, LAST, RIDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAI ( 'RIDATA', RIDATA, '=', IDATA, N, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     DASUDI normal cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     Update an existing DAS.
C
      CALL TCASE ('DASUDI normal case: update existing DAS ' //
     .            'integer numbers.' )

C
C     Open the DAS file created in the normal case for DASADI/DASRDI
C     to update some of the integer addresses. Set the expected
C     updated sequence of integer data in IDATA.
C
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

C
C     Close the file and open it again for reading.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DASI1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Read the data from the DAS file.
C
      FIRST    = 1
      LAST     = 100

      CALL DASRDI ( HANDLE, FIRST, LAST, RIDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAI ( 'RIDATA', RIDATA, '=', IDATA, N, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     DASRDI normal case:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ('DASRDI normal case: read the newly created DAS '
     .//          'file with FIRST greater than LAST. DATA shall not '
     .//          'be changed.' )

C
C     Open the file again for reading.
C
      CALL DASOPR ( DASI1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Clear both IDATA and RIDATA.
C
      CALL CLEARI ( MXNDAT, IDATA  )
      CALL CLEARI ( MXNDAT, RIDATA )

      FIRST = 10
      LAST  = 9

      CALL DASRDI ( HANDLE, FIRST, LAST, RIDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAI ( 'RIDATA', RIDATA, '=', IDATA, MXNDAT, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     DASHFS normal cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ('DASHFS normal case: add character, integer, and '  //
     .            'double precision data and check the updated file ' //
     .            'summary.' )

C
C     Open a new DAS file.
C
      CALL KILFIL ( WBRTST )

      CALL DASONW ( WBRTST, FTYPE, WBRTST, 200, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, 10
         CALL DASADI ( HANDLE, 1, I )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      DO I = 1, 5
         CALL DASADD ( HANDLE, 1, DBLE(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      CALL DASADC ( HANDLE, 4, 1, 4, 'SPUD' )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the file and open it for reading.
C
      CALL DASCLS ( HANDLE         )
      CALL DASOPR ( WBRTST, HANDLE )

C
C     Obtain the file summary.
C
      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
     .              FREE,   LASTLA, LASTRC, LASTWD       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NRESVR', NRESVR, '=',      0, 0, OK )
      CALL CHCKSI ( 'NRESVC', NRESVC, '=',      0, 0, OK )
      CALL CHCKSI ( 'NCOMR',  NCOMR,  '=',    200, 0, OK )
      CALL CHCKSI ( 'NCOMC',  NCOMC,  '=',      0, 0, OK )
      CALL CHCKSI ( 'FREE',   FREE,   '=',    206, 0, OK )
      CALL CHCKAI ( 'LASTLA', LASTLA, '=', XLSTLA, 3, OK )
      CALL CHCKAI ( 'LASTRC', LASTRC, '=', XLSTRC, 3, OK )
      CALL CHCKAI ( 'LASTWD', LASTWD, '=', XLSTWD, 3, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     DASLLA normal cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ('DASLLA normal case: add character, integer, and ' //
     .            'double precision data and check last logical '    //
     .            'addresses for each data type.' )

C
C     Open a new DAS file.
C
      CALL KILFIL ( WBRTST )

      CALL DASONW ( WBRTST, FTYPE, WBRTST, NCOMR, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, 10
         CALL DASADI ( HANDLE, 1, I )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      DO I = 1, 5
         CALL DASADD ( HANDLE, 1, DBLE(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      CALL DASADC ( HANDLE, 4, 1, 4, 'SPUD' )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Now check the logical address ranges.
C
      CALL DASLLA ( HANDLE, LASTC, LASTD, LASTI )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'LASTC', LASTC, '=',  4, 0, OK )
      CALL CHCKSI ( 'LASTD', LASTD, '=',  5, 0, OK )
      CALL CHCKSI ( 'LASTI', LASTI, '=', 10, 0, OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     *****************************************************************
C
C     DASLLC and DASWBR normal cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ('DASLLC and DASWBR normal case: adding '         //
     .            'character, integer, and double precision data ' //
     .            'in interleaved fashion.' )

C
C     Open a new DAS file.
C
      CALL KILFIL ( WBRTST )

      CALL DASONW ( WBRTST, FTYPE, WBRTST, NCOMR, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add data of character, integer, and double precision
C     types to the file in interleaved fashion. We'll add to
C     the file over NPASS "passes," in each of which we close
C     the file after writing.
C
      DO PASSNO = 1, NPASS

         IF ( PASSNO .GT. 1 ) THEN

            CALL DASOPW ( WBRTST, HANDLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         DO I = 1, NCALL
C
C           Add string data to the file.
C
            CHRBUF = 'Character value #'
            CALL REPMI  ( CHRBUF, '#', I, CHRBUF )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CHRBUF(CHRLEN-3:CHRLEN) = '--->'

            CALL DASADC ( HANDLE, CHRLEN, 1, CHRLEN, CHRBUF )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Add double precision data to the file.
C
            DO J = 1, DBUFSZ
               DPBUF(J) = DBLE( 100000000*PASSNO  +  100*I  +  J )
            END DO

            CALL DASADD ( HANDLE, DBUFSZ, DPBUF )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Add integer data to the file.
C
            DO J = 1, IBUFSZ
               INTBUF(J) = 100000000*PASSNO  +  100 * I  +  J
            END DO

            CALL DASADI ( HANDLE, IBUFSZ, INTBUF )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

C
C        Write buffered data to the file.
C
         CALL DASWBR ( HANDLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Close the file without segregating it.
C
         CALL DASLLC ( HANDLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check file contents.
C
      CALL DASOPR ( WBRTST, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Read data from the file; compare to expected values.
C
C     Initialize end addresses.
C
      LASTC = 0
      LASTD = 0
      LASTI = 0

      DO PASSNO = 1, NPASS

         DO I = 1, NCALL
C
C           Check string data.
C
            XCHRBF = 'Character value #'
            CALL REPMI( XCHRBF, '#', I, XCHRBF )
            CALL CHCKXC ( .FALSE., ' ', OK )

            XCHRBF(CHRLEN-3:CHRLEN) = '--->'

            FIRSTC = LASTC + 1
            LASTC  = LASTC + CHRLEN

            CALL DASRDC ( HANDLE, FIRSTC, LASTC, 1, CHRLEN, CHRBUF )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSC ( XCHRBF, CHRBUF, '=', XCHRBF, OK )

C
C           Check double precision data.
C
            DO J = 1, DBUFSZ
               XDPBUF(J) = DBLE( 100000000*PASSNO  +  100*I  +  J )
            END DO

            FIRSTD = LASTD + 1
            LASTD  = LASTD + DBUFSZ

            CALL DASRDD ( HANDLE, FIRSTD, LASTD, DPBUF )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( 'XDPBUF', DPBUF,  '=',
     .                     XDPBUF,  DBUFSZ, 0.D0, OK )

C
C           Check integer data.
C
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

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     *****************************************************************
C
C     DASADC error cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASADC error case #1: begin position of ' //
     .             'substring less than 1.' )

C
C     Open a new DAS file for testing the error cases.
C
      CALL DASONW ( DASTE1, 'TEST', DASTE1, 200, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CDATA(1) = '012345678901234567890123456789'
      N        =  30
      BPOS     =  0
      EPOS     =  20

      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASADC error case #2: end position of ' //
     .             'substring less than 1.' )

      CDATA(1) = '012345678901234567890123456789'
      N        =  30
      BPOS     =  1
      EPOS     =  0

      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASADC error case #3: begin position of ' //
     .             'substring greater than length of substring.' )

      CDATA(1) = '012345678901234567890123456789'
      N        =  30
      BPOS     =  CSTRLN + 1
      EPOS     =  30

      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASADC error case #4: end position of ' //
     .             'substring greater than length of substring.' )

      CDATA(1) = '012345678901234567890123456789'
      N        =  30
      BPOS     =  1
      EPOS     =  CSTRLN + 1

      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASADC error case #5: begin position greater ' //
     .             'than end position of substring.' )

      CDATA(1) = '012345678901234567890123456789'
      N        =  30
      BPOS     =  25
      EPOS     =  20

      CALL DASADC ( HANDLE, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASADC error case #6: Trying to add characters ' //
     .             'to a DAS file that has not been open.' )

      N        =  1
      BPOS     =  10
      EPOS     =  20

      CALL DASADC ( 1000, N, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHHANDLE)', OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     DASADD error cases: None.
C
C     *****************************************************************
C
C     *****************************************************************
C
C     DASADI error cases: None.
C
C     *****************************************************************
C
C     *****************************************************************
C
C     DASRDC error cases: None.
C
C     *****************************************************************
C
C     *****************************************************************
C
C     DASHFS error cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASHFS error case #1: DAS file is not open.' )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
     .              FREE,   LASTLA, LASTRC, LASTWD       )
      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHHANDLE)', OK )

C
C     *****************************************************************
C
C     DASLLA error cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASLLA error case #1: DAS file is not open.' )

      CALL DASLLA ( HANDLE, LASTC, LASTD, LASTI )
      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHHANDLE)', OK )

C
C     *****************************************************************
C
C     DASONW error cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASONW error case #1: blank filename.' )

      CALL DASONW ( '    ', 'TEST', DASD1, 200, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKFILENAME)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASONW error case #2: blank file type.' )

      CALL DASONW ( DASD1, '    ', DASD1, 200, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKFILETYPE)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASONW error case #3: nonprinting character '
     .        //   'case #1.' )

      CALL DASONW ( DASD1, CHAR(31), DASD1, 200, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(ILLEGALCHARACTER)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASONW error case #4: nonprinting character '
     .        //   'case #2.' )

      CALL DASONW ( DASD1, CHAR(127), DASD1, 200, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(ILLEGALCHARACTER)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASONW error case #5: negative number of comment '
     .        //   'records allocated.' )

      CALL DASONW ( DASD1, 'TEST', DASD1, -1, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

C
C     --- Case: -------------------------------------------------------
C
C     Testing of SPICE(DASFTFULL)
C
C
C     *****************************************************************
C
C     DASOPW error cases:
C
C     *****************************************************************
C
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASOPW error case #1: blank filename.' )

      CALL DASOPW ( ' ', HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKFILENAME)', OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASOPW error case #2: specified file does not '
     .        //   'exist.' )

      CALL DASOPW ( 'UNKNOWN.TXT', HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(FILENOTFOUND)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASOPW error case #3: specified file already '
     .        //   'opened.' )

      CALL DASOPR ( DASD1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPW ( DASD1, HANDL1 )
      CALL CHCKXC ( .TRUE., 'SPICE(FILEOPENCONFLICT)', OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     --- Case: -------------------------------------------------------
C
C     Testing of SPICE(DASFTFULL)
C
C     *****************************************************************
C
C     DASRDC error cases:
C
C     *****************************************************************
C
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASRDC error case #1: begin position of ' //
     .             'substring less than 1.' )

C
C     Open the test DAS file created during the DASADC case
C     for testing the error cases.
C
      CALL DASOPR ( DASTE1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      FIRST    =  1
      LAST     =  30
      BPOS     =  0
      EPOS     =  20

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASRDC error case #2: end position of ' //
     .             'substring less than 1.' )

      FIRST    =  1
      LAST     =  30
      BPOS     =  1
      EPOS     =  0

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASRDC error case #3: begin position of ' //
     .             'substring greater than length of substring.' )

      FIRST    =  1
      LAST     =  30
      BPOS     =  CSTRLN + 1
      EPOS     =  30

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASRDC error case #4: end position of ' //
     .             'substring greater than length of substring.' )

      FIRST    =  1
      LAST     =  30
      BPOS     =  1
      EPOS     =  CSTRLN + 1

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASRDC error case #5: begin position greater ' //
     .             'than end position of substring.' )

      FIRST    =  1
      LAST     =  30
      BPOS     =  25
      EPOS     =  20

      CALL DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSUBSTRINGBOUNDS)', OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     DASRDD error cases: None.
C
C     *****************************************************************
C
C     *****************************************************************
C
C     DASRDI error cases: None.
C
C     *****************************************************************
C
C     *****************************************************************
C
C     DASUDC error cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASUDC error case #1: first logical address of ' //
     .             'DAS less than 1.' )

C
C     Open the test DAS file created during the DASADC normal
C     case #1. Note that 120 characters are already in the DAS file,
C     therefore addresses 1:120 are in use.
C
      CALL DASOPR ( DASC1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      FIRST    =  0
      LAST     =  30
      BPOS     =  1
      EPOS     =  40

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDADDRESS)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASUDC error case #2: last logical address of ' //
     .             'DAS less than 1.' )

      FIRST    =  1
      LAST     =  0
      BPOS     =  1
      EPOS     =  40

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDADDRESS)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASUDC error case #3: first logical address of ' //
     .             'DAS greater than last character logical address.' )

      FIRST    =  121
      LAST     =  30
      BPOS     =  1
      EPOS     =  40

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDADDRESS)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASUDC error case #4: last logical address of ' //
     .             'DAS greater than last character logical address.' )

      FIRST    =  1
      LAST     =  121
      BPOS     =  1
      EPOS     =  40

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDADDRESS)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASUDC error case #5: string begin index '
     .//           'non-positive.' )

      FIRST    =  1
      LAST     =  120
      BPOS     =  0
      EPOS     =  40

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASUDC error case #6: string begin index '
     .//           'greater than string length.' )

      FIRST    =  1
      LAST     =  120
      BPOS     =  CSTRLN + 1
      EPOS     =  CSTRLN + 1

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASUDC error case #7: string end index '
     .//           'non-positive.' )

      FIRST    =  1
      LAST     =  120
      BPOS     =  1
      EPOS     =  0

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASUDC error case #8: string end index '
     .//           'greater than string length.' )

      FIRST    =  1
      LAST     =  120
      BPOS     =  1
      EPOS     =  CSTRLN + 1

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'DASUDC error case #9: string end index '
     .//           'less than begin index.' )

      FIRST    =  1
      LAST     =  120
      BPOS     =  CSTRLN
      EPOS     =  1

      CALL DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, CDATA )
      CALL CHCKXC ( .TRUE., 'SPICE(INDICESOUTOFORDER)', OK )

C
C     Close the file.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     DASWBR error cases: None.
C
C     *****************************************************************
C
C     *****************************************************************
C
C     Clean up
C
C     *****************************************************************
C
C     Remove all the DAF files created during the execution of the
C     tests.
C
      CALL KILFIL( DASC1  )
      CALL KILFIL( DASTE1 )
      CALL KILFIL( DASD1  )
      CALL KILFIL( DASI1  )
      CALL KILFIL( WBRTST )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END
