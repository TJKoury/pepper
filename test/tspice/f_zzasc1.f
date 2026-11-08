C$Procedure  F_ZZASC1 ( Family of tests for ZZASCII )
 
      SUBROUTINE F_ZZASC1 ( OK )
 
      IMPLICIT NONE
 
C$ Abstract
C
C     Test family for the ZZASCII routine. The tests confirm correct
C     responses when scanning differently terminated text files by
C     ZZASCII and ZZLDKER.
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
C     TBD.
C
C$ Keywords
C
C     TBD.
C
C$ Declarations
C
C     TBD.
C
C$ Brief_I/O
C
C     TBD.
C
C$ Detailed_Input
C
C     TBD.
C
C$ Detailed_Output
C
C     TBD.
C
C$ Parameters
C
C     TBD.
C
C$ Exceptions
C
C     TBD.
C
C$ Files
C
C     TBD.
C
C$ Particulars
C
C     TBD.
C
C$ Examples
C
C     TBD.
C
C$ Restrictions
C
C     TBD.
C
C$ Literature_References
C
C     TBD.
C
C$ Author_and_Institution
C
C     E.D. Wright      (JPL)
C
C$ Version
C
C-    TSPICE Version 1.26.0, 28-NOV-2021 (BVS)
C
C        Updated for MAC-OSX-M1-64BIT-CLANG_C.
C
C-    TSPICE Version 1.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    TSPICE Version 1.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    TSPICE Version 1.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    TSPICE Version 1.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    TSPICE Version 1.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    TSPICE Version 1.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    TSPICE Version 1.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    TSPICE Version 1.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    TSPICE Version 1.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    TSPICE Version 1.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    TSPICE Version 1.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    TSPICE Version 1.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    TSPICE Version 1.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    TSPICE Version 1.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    TSPICE Version 1.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    TSPICE Version 1.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    TSPICE Version 1.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    TSPICE Version 1.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    TSPICE Version 1.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    TSPICE Version 1.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    TSPICE Version 1.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    TSPICE Version 1.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    TSPICE Version 1.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    TSPICE Version 1.2.0, 03-OCT-2005 (EDW)
C
C        Updated to block-out all code when processed for C
C        environments. Due to the non-native text kernel capability
C        in CSPICE, tspice_c should not perform zzascii tests - so
C        always return a success.
C
C-    TSPICE Version 1.0.0, 11-FEB-2004 (EDW)
C
C-&
 
      LOGICAL               OK
 
 
 
 
 
      INTEGER               RECLEN
      PARAMETER           ( RECLEN   = 32 )
 
      INTEGER               SHRT
      PARAMETER           ( SHRT   = RECLEN/2 )
 
C
C     Write ascii text containing line terminators
C     for each platform, Macintosh classic, Unix,
C     and DOS.
C
      INTEGER               FAIL(2)
      INTEGER               NATIVE
      INTEGER               IOSTAT
      INTEGER               NUMBER
      INTEGER               REC
      INTEGER               I
      INTEGER               J
 
      CHARACTER*(RECLEN)    LINE (3)
      CHARACTER*(12)        FILES(3)
 
      CHARACTER*(2)         TERMIN(3)
      CHARACTER*(5)         EXPECT(3)
      CHARACTER*(5)         OUTTRM
      CHARACTER*(5)         WORD
      CHARACTER*(50)        WRKLIN
      CHARACTER*(SHRT)      SHRTLN
      CHARACTER*(2)         XSHRTL
 
C
C     SPICELIB functions.
C
      LOGICAL               EXISTS
      LOGICAL               EQSTR
 
 
C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZASC1' )
 
C
C     Define the data to write to test files. ID the
C     file as an LSK kernel, with a begin text block - thus
C     we don't risk damaging any kernel pool variable
C     assignments. The third line contains the data to write.
C     Thirty-two characters written forty times.
C
      LINE (1) = 'KPL/LSK                         '
      LINE (2) = '\begintext                      '
      LINE (3) = '12345678901234567890123456789012'
 
C
C     Initialize the file names and the corresponding line
C     terminators.
C
      FILES(1) = 'mac.ker'
      FILES(2) = 'unix.ker'
      FILES(3) = 'dos.ker'
 
      TERMIN(1)(1:2) = CHAR(13)
      TERMIN(2)(1:2) = CHAR(10)
      TERMIN(3)(1:2) = CHAR(13)//CHAR(10)
 
      EXPECT(1) = 'CR'
      EXPECT(2) = 'LF'
      EXPECT(3) = 'CR-LF'
 
 
C
C     Initialize the REC marker value. Start from the
C     beginning of the file.
C
      REC = 1
 
C
C     Loop over the three terminator types. Create
C     a test file with an appropriate name for the terminator
C     character.
C
      DO I=1, 3
 
C
C     If the test file exists, delete it.
C
         IF ( EXISTS( FILES(I) ) ) THEN
 
            CALL KILFIL( FILES(I) )
 
         END IF
 
C
C        Find a free logical unit for file access.
C
         CALL GETLUN ( NUMBER )
 
C
C        Open the file as direct access thus suppressing the print
C        of a native line terminator.
C
         OPEN ( UNIT               = NUMBER  ,
     .          FILE               = FILES(I),
     .          ACCESS             = 'DIRECT',
     .          STATUS             = 'NEW'   ,
     .          RECL               = RECLEN  ,
     .          IOSTAT             = IOSTAT  )
 
C
C        Add the terminator to the first line.
C
         LINE(1)(31:32) = TERMIN(I)
 
C
C        Write the line to record REC then increment REC.
C
         WRITE( NUMBER, REC=REC ) LINE(1)(1:32)
         REC = REC + 1
 
C
C        Add the terminator to the second line.
C
 
         LINE(2)(31:32) = TERMIN(I)
 
C
C        Write the line to record REC then increment REC.
C
         WRITE( NUMBER, REC=REC ) LINE(2)(1:32)
         REC = REC + 1
 
C
C        Add the terminator to the third data line. We
C        write this line 40 times to create a kernel file
C        larger than 1032 (the record size read by GETFAT).
C
         LINE(3)(31:32) = TERMIN(I)
 
C
C        Write the line, incrementing the record counter each
C        pass.
C
         DO J = 1, 40
            WRITE( NUMBER, REC=REC ) LINE(3)(1:32)
            REC = REC + 1
         END DO
 
C
C        Finished. Close this file, reset the REC marker for the
C        next file.
C
         CLOSE( NUMBER )
         REC = 1
 
      END DO
 
 
C
C Case 1
C
      CALL TCASE ( 'Check for correct terminator ID' )
 
C
C     Start tests. Check the correct response for each file.
C     Note, the length of WRKLIN must exceed the declared length of
C     the LINE variable for the test to succeed.
C
      DO I = 1, 3
 
         CALL ZZASCII ( FILES(I), WRKLIN, .FALSE., OUTTRM )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSC ( 'Terminator', OUTTRM, '=', EXPECT(I), OK )
 
      END DO
 
 
C
C Case 2
C
      CALL TCASE ( 'Check for failure for short line' )
 
C
C     Test that a scan fails when using a line length shorter than
C     the line length of the first line in the file of interest.
C     In this case we know the test kernels have line
C     lengths of thirty-two.
C
      DO I = 1, 3
 
         CALL ZZASCII ( FILES(I), SHRTLN, .FALSE., OUTTRM )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSC ( 'Terminator', OUTTRM, '=', '?', OK )
 
      END DO
 
 
C
C     Retrieve the proper line terminator for the platform.
C
      CALL ZZPLATFM ( 'TEXT_FORMAT', WORD )
 
C
C     Set the index for the native terminator.
C
      IF ( EQSTR ( WORD, EXPECT(1) ) ) THEN
 
         NATIVE  = 1
         FAIL(1) = 2
         FAIL(2) = 3
 
      ELSE IF ( EQSTR ( WORD, EXPECT(2) ) )THEN
 
         NATIVE  = 2
         FAIL(1) = 1
         FAIL(2) = 3
 
      ELSE IF ( EQSTR ( WORD, EXPECT(3) ) ) THEN
 
         NATIVE  = 3
         FAIL(1) = 1
         FAIL(2) = 2
 
      ELSE
 
C
C     BAD BUG! This block should never execute.
C
         CALL CHCKXC ( .TRUE., 'UNKNOWNLINETERMINATOR', OK )
 
C
C        Clean-up. Delete files.
C
         DO I = 1, 3
            CALL KILFIL( FILES(I) )
         END DO
 
C
C        Close out the test family.
C
         CALL T_SUCCESS ( OK )
         RETURN
 
      END IF
 
 
C
C Case 3
C
      CALL TCASE ('Check for correct FURNSH response, native.')
 
C
C     Check a native file passes through a FURNSH call without
C     an error signal.
C
      CALL FURNSH ( FILES(NATIVE) )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Unload the kernel so we don't mess-up another kernel
C     test family.
C
      CALL UNLOAD ( FILES(NATIVE) )
 
C
C Case 4
C
      CALL TCASE ('Check for correct FURNSH response, non-native.')
 
C
C     Check a non-native file causes an error signal when used in a
C     FURNSH call.
C
      CALL FURNSH ( FILES( FAIL(1) ) )
      CALL CHCKXC ( .TRUE., 'SPICE(INCOMPATIBLEEOL)', OK )
 
      CALL FURNSH ( FILES( FAIL(2) ) )
      CALL CHCKXC ( .TRUE., 'SPICE(INCOMPATIBLEEOL)', OK )
 
 
C
C Case 5
C
      CALL TCASE ( 'Check for ZZASCII short work line signal.' )
 
C
C     Ensure ZZASCII signals an error for work line lengths
C     below the 3 character threshold.
C
      CALL ZZASCII ( FILES(NATIVE), XSHRTL, .FALSE., OUTTRM )
      CALL CHCKXC ( .TRUE., 'SPICE(STRINGTOOSHORT)', OK )
 
C
C     Clean-up. Delete files.
C
      DO I = 1, 3
         CALL KILFIL( FILES(I) )
      END DO
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
 
 
      RETURN
      END
