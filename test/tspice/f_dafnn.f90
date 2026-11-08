!$Procedure F_DAFNN ( DAF Non-Native Test Family )

      SUBROUTINE F_DAFNN ( OK )

! $ Abstract
!
!     Test family to exercise the logic and code in routines that
!     operate on non-native DAF files.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      INCLUDE              'zzddhman.inc'

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
! $ Files
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
! $ Particulars
!
!     This routine exercises some non-native DAF logic.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    TSPICE Version 2.0.0, 06-FEB-2002 (FST)
!
!        Appended a test that modifies the NATIVE file
!        by replacing the binary file format ID word in the
!        file record with 8 nulls.  This was done to validate
!        changes in ZZDDHPPF.  See the Revisions section of
!        its header for details.
!
!-    TSPICE Version 1.0.0, 16-DEC-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               RTRIM

!
!     Local Parameters
!
      INTEGER               NUMCHR
      PARAMETER           ( NUMCHR = 1024 )

      INTEGER               NUMDP
      PARAMETER           ( NUMDP = 128 )

      INTEGER               NUMINT
      PARAMETER           ( NUMINT = 256 )

!
!     Local Variables
!
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

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DAFNN' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'F_DAFNN Initialization' )

!
!     Retrieve the native format and other related information.
!
      CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, STRAMH, STRARC, STRBFF )

!
!     Check to see if this system supports multiple binary file formats.
!     If it does not, then just end the test family here.
!
      IF ( NUMSUP .EQ. 1 ) THEN
         CALL T_SUCCESS ( OK )
         RETURN
      END IF

!
!     Now locate the native file format in SUPBFF and construct the
!     list of non-native format codes.
!
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

!
!     Check the value of NATI, it should be non-zero.
!
      CALL CHCKSI ( 'NATIVE_INDEX', NATI, '!=', 0, 0, OK )

!
!     Check the value of NUMNN, it should be at least 1.
!
      CALL CHCKSI ( 'NUMBER_NN_BFFS', NUMNN, '>=', 1, 0, OK )

!
!     Construct a native file first.
!
      NATIVE = 'daf.daf'
      CALL TSTSPK ( NATIVE, .FALSE., HANDLE )

!
!     Setup the non-native filename template.
!
      FNMTMP = 'nndaf#.daf'

!
!     Construct the DAFs for each non-native file format.
!
      DO I = 1, NUMNN

         CALL REPMI ( FNMTMP, '#', I, FNAME )

!
!        Convert the native source DAF into the appropriate
!        format.
!
         CALL T_BINGO ( NATIVE, FNAME, NNBFF(I) )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DAFOPW on Native and Non-Native Formats' )

!
!     Attempt to open the native format for write, this should work.
!
      HANDLE = 0
      CALL DAFOPW ( NATIVE, HANDLE )

!
!     Check for the absence of errors.
!
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '!=', 0, 0, OK )

!
!     Unload the file.
!
      CALL DAFCLS ( HANDLE )

!
!     Now attempt the same for each of the non-native files.
!
      DO I = 1, NUMNN

         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL DAFOPW ( FNAME, HANDLE )

!
!        Check for the exception.
!
         CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDBFF)', OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check DAF routines requiring write access' )

!
!     Clear the handle list.
!
      CALL CLEARI ( NUMBFF, HANLST )

!
!     Open all the non-native files for read access.
!     This should work.
!
      DO I = 1, NUMNN
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL DAFOPR ( FNAME, HANLST(I)      )

!
!        Check for the absence of an exception.
!
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSI ( 'HANDLE', HANLST(I), '!=', 0, 0, OK )

      END DO

!
!     Now append the native file to the handle list.
!
      NUMHAN = NUMNN + 1

      CALL DAFOPR ( NATIVE, HANLST(NUMHAN) )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'HANDLE', HANLST(NUMHAN), '!=', 0, 0, OK )

!
!     Now loop over every file in the handle list, checking each
!     of the routines and entry points that require write access
!     for DAFs.
!
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

!
!        The following routine signals different errors depending
!        on whether or not the HANDLE is associated with a native
!        or non-native file.
!
         CALL DAFWDA ( HANDLE, 128, 250, DPS )

         IF ( I .NE. NUMHAN ) THEN
            CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDBFF)', OK )
         ELSE
            CALL CHCKXC ( .TRUE., 'SPICE(DAFILLEGWRITE)',  OK )
         END IF

         CALL DAFWFR ( HANDLE, 2, 6, 'NONE', 2, 2, 1205 )
         CALL CHCKXC ( .TRUE., 'SPICE(DAFINVALIDACCESS)', OK )

      END DO

!
!     Close the files.
!
      DO I = 1, NUMHAN
         CALL DAFCLS ( HANLST(I) )
      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check obsolete DAF routines requiring native' )

!
!     Clear the handle list.
!
      CALL CLEARI ( NUMBFF, HANLST )

!
!     Open all the non-native files for read access.
!     This should work.
!
      DO I = 1, NUMNN
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL DAFOPR ( FNAME, HANLST(I)      )

!
!        Check for the absence of an exception.
!
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSI ( 'HANDLE', HANLST(I), '!=', 0, 0, OK )

      END DO

!
!     Now append the native file to the handle list.
!
      NUMHAN = NUMNN + 1

      CALL DAFOPR ( NATIVE, HANLST(NUMHAN) )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'HANDLE', HANLST(NUMHAN), '!=', 0, 0, OK )

!
!     Loop over all non-native files, checking for the appropriate
!     exceptions.
!
      DO I = 1, NUMNN

         HANDLE = HANLST(I)

         CALL DAFRDA ( HANDLE, 128, 250, DPS )
         CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDBFF)', OK )

         CALL DAFRDR ( HANDLE, 1, 1, 128, DPS, FOUND )
         CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDBFF)', OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      END DO

!
!     Verify that we can still use these routines on native files.
!
      HANDLE = HANLST ( NUMHAN )

      CALL DAFRDA ( HANDLE, 385, 390, DPS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFRDR ( HANDLE, 4, 2, 10, DPS, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Close the files.
!
      DO I = 1, NUMHAN
         CALL DAFCLS ( HANLST(I) )
      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check native NULL BFFID File Records Load' )

!
!     Note: This test modifies the file record of the file
!     referenced by the name NATIVE.  If you need to add tests,
!     add them before this one.
!

      CALL GETLUN ( UNIT )

      OPEN ( UNIT   = UNIT, &
             FILE   = NATIVE(1:RTRIM(NATIVE)), &
             ACCESS = 'DIRECT', &
             RECL   = RECL, &
             STATUS = 'OLD', &
             IOSTAT = IOSTAT              )

!
!     Check IOSTAT.
!
      CALL CHCKSI ( 'OPEN IOSTAT', IOSTAT, '=', 0, 0, OK )

!
!     Read the file record into the CHARS buffer.
!
      READ ( UNIT=UNIT, REC=1, IOSTAT=IOSTAT ) CHARS

!
!     Check IOSTAT.
!
      CALL CHCKSI ( 'READ IOSTAT', IOSTAT, '=', 0, 0, OK )

!
!     Replace the BFF ID with nulls.
!
      DO I = 89, 96
         CHARS(I:I) = CHAR(0)
      END DO

      WRITE ( UNIT=UNIT, REC=1, IOSTAT=IOSTAT ) CHARS

!
!     Check IOSTAT.
!
      CALL CHCKSI ( 'WRITE IOSTAT', IOSTAT, '=', 0, 0, OK )

!
!     Close the file.
!
      CLOSE ( UNIT=UNIT )

!
!     Check for any exceptions; there should be none.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Attempt to open the file with DAFOPR.
!
      CALL DAFOPR ( NATIVE, HANDLE )

!
!     Check for an exception; again we expect none.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file and try DAFOPW.
!
      CALL DAFCLS ( HANDLE )

!
!     Now try DAFOPW.
!
      CALL DAFOPW ( NATIVE, HANDLE )

!
!     Check for any exceptions; again we expect none.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the file.
!
      CALL DAFCLS ( HANDLE )


!
!     Clean up by removing the test files.
!
      DO I = 1, NUMNN
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL KILFIL ( FNAME                 )
      END DO

      CALL KILFIL ( NATIVE )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
