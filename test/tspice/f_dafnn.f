C$Procedure F_DAFNN ( DAF Non-Native Test Family )
 
      SUBROUTINE F_DAFNN ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in routines that
C     operate on non-native DAF files.
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
 
      INCLUDE              'zzddhman.inc'
 
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
C$ Files
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
C$ Particulars
C
C     This routine exercises some non-native DAF logic.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    TSPICE Version 2.0.0, 06-FEB-2002 (FST)
C
C        Appended a test that modifies the NATIVE file
C        by replacing the binary file format ID word in the
C        file record with 8 nulls.  This was done to validate
C        changes in ZZDDHPPF.  See the Revisions section of
C        its header for details.
C
C-    TSPICE Version 1.0.0, 16-DEC-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               RTRIM
 
C
C     Local Parameters
C
      INTEGER               NUMCHR
      PARAMETER           ( NUMCHR = 1024 )
 
      INTEGER               NUMDP
      PARAMETER           ( NUMDP = 128 )
 
      INTEGER               NUMINT
      PARAMETER           ( NUMINT = 256 )
 
C
C     Local Variables
C
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMTMP
      CHARACTER*(FILEN)     NATIVE
      CHARACTER*(NUMCHR)    CHARS
      CHARACTER*(STRSIZ)    STRAMH ( NUMAMH )
      CHARACTER*(STRSIZ)    STRARC ( NUMARC )
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
 
      DOUBLE PRECISION      DPS    ( NUMDP )
 
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               INTS   ( NUMINT )
      INTEGER               NATBFF
      INTEGER               NATI
      INTEGER               NNBFF  ( NUMBFF )
      INTEGER               NUMNN
      INTEGER               NUMSUP
      INTEGER               SUPBFF ( NUMBFF )
      INTEGER               NUMHAN
      INTEGER               HANLST ( NUMBFF )
      INTEGER               UNIT
 
      LOGICAL               FOUND
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DAFNN' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'F_DAFNN Initialization' )
 
C
C     Retrieve the native format and other related information.
C
      CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, STRAMH, STRARC, STRBFF )
 
C
C     Check to see if this system supports multiple binary file formats.
C     If it does not, then just end the test family here.
C
      IF ( NUMSUP .EQ. 1 ) THEN
         CALL T_SUCCESS ( OK )
         RETURN
      END IF
 
C
C     Now locate the native file format in SUPBFF and construct the
C     list of non-native format codes.
C
      NATI  = 0
      NUMNN = 0
 
      DO I = 1, NUMSUP
 
         IF ( SUPBFF(I) .EQ. NATBFF ) THEN
            NATI = I
         ELSE
            NUMNN        = NUMNN + 1
            NNBFF(NUMNN) = SUPBFF(I)
         END IF
 
      END DO
 
C
C     Check the value of NATI, it should be non-zero.
C
      CALL CHCKSI ( 'NATIVE_INDEX', NATI, '!=', 0, 0, OK )
 
C
C     Check the value of NUMNN, it should be at least 1.
C
      CALL CHCKSI ( 'NUMBER_NN_BFFS', NUMNN, '>=', 1, 0, OK )
 
C
C     Construct a native file first.
C
      NATIVE = 'daf.daf'
      CALL TSTSPK ( NATIVE, .FALSE., HANDLE )
 
C
C     Setup the non-native filename template.
C
      FNMTMP = 'nndaf#.daf'
 
C
C     Construct the DAFs for each non-native file format.
C
      DO I = 1, NUMNN
 
         CALL REPMI ( FNMTMP, '#', I, FNAME )
 
C
C        Convert the native source DAF into the appropriate
C        format.
C
         CALL T_BINGO ( NATIVE, FNAME, NNBFF(I) )
 
      END DO
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DAFOPW on Native and Non-Native Formats' )
 
C
C     Attempt to open the native format for write, this should work.
C
      HANDLE = 0
      CALL DAFOPW ( NATIVE, HANDLE )
 
C
C     Check for the absence of errors.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '!=', 0, 0, OK )
 
C
C     Unload the file.
C
      CALL DAFCLS ( HANDLE )
 
C
C     Now attempt the same for each of the non-native files.
C
      DO I = 1, NUMNN
 
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL DAFOPW ( FNAME, HANDLE )
 
C
C        Check for the exception.
C
         CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDBFF)', OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
      END DO
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check DAF routines requiring write access' )
 
C
C     Clear the handle list.
C
      CALL CLEARI ( NUMBFF, HANLST )
 
C
C     Open all the non-native files for read access.
C     This should work.
C
      DO I = 1, NUMNN
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL DAFOPR ( FNAME, HANLST(I)      )
 
C
C        Check for the absence of an exception.
C
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSI ( 'HANDLE', HANLST(I), '!=', 0, 0, OK )
 
      END DO
 
C
C     Now append the native file to the handle list.
C
      NUMHAN = NUMNN + 1
 
      CALL DAFOPR ( NATIVE, HANLST(NUMHAN) )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'HANDLE', HANLST(NUMHAN), '!=', 0, 0, OK )
 
C
C     Now loop over every file in the handle list, checking each
C     of the routines and entry points that require write access
C     for DAFs.
C
      CHARS = ' '
      CALL CLEARD ( NUMDP, DPS )
 
      DO I = 1, NUMHAN
 
         HANDLE = HANLST(I)
 
         CALL DAFBFS ( HANDLE )
         CALL DAFFNA ( FOUND  )
 
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
         CALL DAFRS  ( DPS )
         CALL CHCKXC ( .TRUE., 'SPICE(DAFINVALIDACCESS)', OK )
 
         CALL DAFRN  ( 'NONE' )
         CALL CHCKXC ( .TRUE., 'SPICE(DAFINVALIDACCESS)', OK )
 
         CALL DAFWS  ( DPS )
         CALL CHCKXC ( .TRUE., 'SPICE(DAFILLEGWRITE)', OK )
 
         CALL DAFBNA ( HANDLE, DPS, CHARS )
         CALL CHCKXC ( .TRUE., 'SPICE(DAFINVALIDACCESS)', OK )
 
         CALL DAFCAD ( HANDLE )
         CALL CHCKXC ( .TRUE., 'SPICE(DAFINVALIDACCESS)', OK )
 
         CALL DAFARR ( HANDLE, 5 )
         CALL CHCKXC ( .TRUE., 'SPICE(DAFINVALIDACCESS)', OK )
 
         INTS (1) = 1
         INTS (2) = 3
         INTS (3) = 2
 
         CALL DAFRA  ( HANDLE, INTS, 3 )
         CALL CHCKXC ( .TRUE., 'SPICE(DAFILLEGWRITE)', OK )
 
         CALL DAFRRR ( HANDLE, 5 )
         CALL CHCKXC ( .TRUE., 'SPICE(DAFINVALIDACCESS)', OK )
 
         CALL DAFWDR ( HANDLE, 2, DPS )
         CALL CHCKXC ( .TRUE., 'SPICE(DAFILLEGWRITE)', OK )
 
         CALL DAFWCR ( HANDLE, 2, CHARS )
         CALL CHCKXC ( .TRUE., 'SPICE(DAFINVALIDACCESS)', OK )
 
C
C        The following routine signals different errors depending
C        on whether or not the HANDLE is associated with a native
C        or non-native file.
C
         CALL DAFWDA ( HANDLE, 128, 250, DPS )
 
         IF ( I .NE. NUMHAN ) THEN
            CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDBFF)', OK )
         ELSE
            CALL CHCKXC ( .TRUE., 'SPICE(DAFILLEGWRITE)',  OK )
         END IF
 
         CALL DAFWFR ( HANDLE, 2, 6, 'NONE', 2, 2, 1205 )
         CALL CHCKXC ( .TRUE., 'SPICE(DAFINVALIDACCESS)', OK )
 
      END DO
 
C
C     Close the files.
C
      DO I = 1, NUMHAN
         CALL DAFCLS ( HANLST(I) )
      END DO
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check obsolete DAF routines requiring native' )
 
C
C     Clear the handle list.
C
      CALL CLEARI ( NUMBFF, HANLST )
 
C
C     Open all the non-native files for read access.
C     This should work.
C
      DO I = 1, NUMNN
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL DAFOPR ( FNAME, HANLST(I)      )
 
C
C        Check for the absence of an exception.
C
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSI ( 'HANDLE', HANLST(I), '!=', 0, 0, OK )
 
      END DO
 
C
C     Now append the native file to the handle list.
C
      NUMHAN = NUMNN + 1
 
      CALL DAFOPR ( NATIVE, HANLST(NUMHAN) )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'HANDLE', HANLST(NUMHAN), '!=', 0, 0, OK )
 
C
C     Loop over all non-native files, checking for the appropriate
C     exceptions.
C
      DO I = 1, NUMNN
 
         HANDLE = HANLST(I)
 
         CALL DAFRDA ( HANDLE, 128, 250, DPS )
         CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDBFF)', OK )
 
         CALL DAFRDR ( HANDLE, 1, 1, 128, DPS, FOUND )
         CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDBFF)', OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 
      END DO
 
C
C     Verify that we can still use these routines on native files.
C
      HANDLE = HANLST ( NUMHAN )
 
      CALL DAFRDA ( HANDLE, 385, 390, DPS )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL DAFRDR ( HANDLE, 4, 2, 10, DPS, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
C
C     Close the files.
C
      DO I = 1, NUMHAN
         CALL DAFCLS ( HANLST(I) )
      END DO
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check native NULL BFFID File Records Load' )
 
C
C     Note: This test modifies the file record of the file
C     referenced by the name NATIVE.  If you need to add tests,
C     add them before this one.
C
 
      CALL GETLUN ( UNIT )
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = NATIVE(1:RTRIM(NATIVE)),
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT              )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'OPEN IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Read the file record into the CHARS buffer.
C
      READ ( UNIT=UNIT, REC=1, IOSTAT=IOSTAT ) CHARS
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'READ IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Replace the BFF ID with nulls.
C
      DO I = 89, 96
         CHARS(I:I) = CHAR(0)
      END DO
 
      WRITE ( UNIT=UNIT, REC=1, IOSTAT=IOSTAT ) CHARS
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'WRITE IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Close the file.
C
      CLOSE ( UNIT=UNIT )
 
C
C     Check for any exceptions; there should be none.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Attempt to open the file with DAFOPR.
C
      CALL DAFOPR ( NATIVE, HANDLE )
 
C
C     Check for an exception; again we expect none.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Close the file and try DAFOPW.
C
      CALL DAFCLS ( HANDLE )
 
C
C     Now try DAFOPW.
C
      CALL DAFOPW ( NATIVE, HANDLE )
 
C
C     Check for any exceptions; again we expect none.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Close the file.
C
      CALL DAFCLS ( HANDLE )
 
 
C
C     Clean up by removing the test files.
C
      DO I = 1, NUMNN
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL KILFIL ( FNAME                 )
      END DO
 
      CALL KILFIL ( NATIVE )
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
