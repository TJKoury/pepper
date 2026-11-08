C$Procedure  F_ZZASC2 ( Family of tests for ZZASCII )
 
      SUBROUTINE F_ZZASC2 ( OK )
 
      IMPLICIT NONE
 
C$ Abstract
C
C     Another test family for the ZZASCII routine. The tests confirm
C     correct responses when scanning differently terminated text files
C     by ZZASCII and ZZLDKER.
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
C     B.V. Semenov     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.27.0, 28-NOV-2021 (BVS)
C
C        Updated for MAC-OSX-M1-64BIT-CLANG_C.
C
C-    TSPICE Version 1.26.0, 02-APR-2014 (BVS)
C
C        Changed PC-CYGWIN-GFORTRAN and PC-CYGWIN-64BIT-GFORTRAN
C        to be like LINUX GFORTRAN environments.
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
C-    TSPICE Version 1.10.0, 01-APR-2009 (BVS)(EDW)
C
C        Update for gfortran with tags:
C
C        PC-LINUX-GFORTRAN
C        MAC-OSX-GFORTRAN
C
C-    TSPICE Version 1.9.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    TSPICE Version 1.8.0, 20-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    TSPICE Version 1.7.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    TSPICE Version 1.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    TSPICE Version 1.5.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    TSPICE Version 1.4.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    TSPICE Version 1.3.0, 03-OCT-2005 (EDW)
C
C        Updated to block-out all code when processed for C
C        environments. Due to the non-native text kernel capability
C        in CSPICE, tspice_c should not perform zzascii tests - so
C        always return a success.
C
C-    TSPICE Version 1.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    TSPICE Version 1.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    TSPICE Version 1.0.0, 17-FEB-2004 (EDW) (BVS)
C
C-&
 
      LOGICAL               OK
 
 
 
 
 
C
C     Local parameters
C
      INTEGER               LNGLEN
      PARAMETER           ( LNGLEN = 128 )
 
 
      INTEGER               MIDLEN
      PARAMETER           ( MIDLEN = LNGLEN/2 )
 
 
 
 
      INTEGER               RECLEN
      PARAMETER           ( RECLEN = LNGLEN/2 )
 
 
 
 
 
      INTEGER               NFILES
      PARAMETER           ( NFILES = 12 )
 
      INTEGER               EOLTSZ
      PARAMETER           ( EOLTSZ = 5 )
 
C
C     Local Variables.
C
      INTEGER               IOSTAT
      INTEGER               NUMBER
      INTEGER               REC
      INTEGER               I
      INTEGER               J
 
      CHARACTER*(MIDLEN)    LINE   ( 3 )
      CHARACTER*(2)         TERMIN (3)
      CHARACTER*(MIDLEN)    HLINE
 
      CHARACTER*(12)        FILES ( NFILES )
      CHARACTER*(12)        MYFILE
 
      CHARACTER*(EOLTSZ)    OUTTRM
      CHARACTER*(LNGLEN)    WRKLIN
      CHARACTER*(2)         XSHRTL
 
      LOGICAL               MYZZEF( NFILES )
      CHARACTER*(32)        MYZZEM( NFILES )
      CHARACTER*(EOLTSZ)    MYTERM( NFILES )
      LOGICAL               MYFNEF( NFILES )
      CHARACTER*(32)        MYFNEM( NFILES )
 
C
C     SPICELIB functions.
C
      LOGICAL               EXISTS
 
C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZASC2' )
 
C
C     Define the data to write to test files. ID the file as an LSK
C     kernel, with a begin text block - thus we don't risk damaging any
C     kernel pool variable assignments. The third line contains the
C     data to write.
C
      LINE (1) = 'KPL/LSK                          ' //
     .           '                               '
      LINE (2) = '\begintext                       ' //
     .           '                               '
      LINE (3) = '123456789012345678901234567890123' //
     .           '4567890123456789012345678901234'
 
C
C     Initialize the file names. The first three files will be smaller
C     than the buffer that will be used to read them. The second three
C     will have exactly same size as the buffer. The third three will
C     be larger than the buffer. The last three will be larger than the
C     buffer but will contain mixed terminators.
C
      FILES(1)  = 'macs.ker'
      FILES(2)  = 'unxs.ker'
      FILES(3)  = 'doss.ker'
      FILES(4)  = 'macm.ker'
      FILES(5)  = 'unxm.ker'
      FILES(6)  = 'dosm.ker'
      FILES(7)  = 'macl.ker'
      FILES(8)  = 'unxl.ker'
      FILES(9)  = 'dosl.ker'
      FILES(10) = 'mxd1.ker'
      FILES(11) = 'mxd2.ker'
      FILES(12) = 'mxd3.ker'
 
C
C     Initialize terminators and tags.
C
      TERMIN(1) = CHAR(13)//' '
      TERMIN(2) = CHAR(10)//' '
      TERMIN(3) = CHAR(13)//CHAR(10)
 
C
C     Generate test files. The first loop ("I" loop) controls how many
C     records will be written to the test files (up to three, the 4th
C     is for the mixed files.)
C
      DO I = 1, 4
 
C
C        The second loop ("J" loop) goes over triplets of files and
C        corresponding terminators.
C
         DO J = 1, 3
 
C
C           If the test file exists, delete it.
C
            MYFILE =  FILES((I-1)*3+J)
 
            IF ( EXISTS( MYFILE ) ) THEN
               CALL KILFIL( MYFILE )
            END IF
 
C
C           Find a free logical unit and open the file as direct access.
C
            CALL GETLUN ( NUMBER )
 
            OPEN ( UNIT               = NUMBER,
     .             FILE               = MYFILE,
     .             ACCESS             = 'DIRECT',
     .             STATUS             = 'NEW'   ,
     .             RECL               = RECLEN  ,
     .             IOSTAT             = IOSTAT  )
 
C
C           The third loop ("REC" loop) pads "I" lines with the right
C           terminators (for the first three file triplets) or with
C           mixed terminators (for the last triplet) and writes them to
C           the file.
C
            DO REC = 1, I
 
C
C              Copy line to temporary buffer, add the terminator and
C              write it to the file. If REC is 4 or greater, use
C              incorrect terminator to get all flavors of
C              "contaminated" files.
C
               IF ( REC .LE. 3 ) THEN
 
                  HLINE = LINE(REC)
                  HLINE( MIDLEN-1:MIDLEN ) = TERMIN(J)
                  WRITE( NUMBER, REC=REC ) HLINE(1:MIDLEN)
 
               ELSE
 
                  HLINE = LINE(1)
                  IF      ( J .EQ. 1 ) THEN
                     HLINE( MIDLEN-1:MIDLEN ) = TERMIN(2)
                  ELSE IF ( J .EQ. 2 ) THEN
                     HLINE( MIDLEN-1:MIDLEN ) = TERMIN(3)
                  ELSE IF ( J .EQ. 3 ) THEN
                     HLINE( MIDLEN-1:MIDLEN ) = TERMIN(1)
                  END IF
                  WRITE( NUMBER, REC=1   ) HLINE(1:MIDLEN)
 
               END IF
 
            END DO
 
C
C           Close the file.
C
            CLOSE( NUMBER )
 
         END DO
 
      END DO
 
 
 
 
C
C     Ensure ZZASCII signals an error for work line lengths below the 3
C     character threshold.
C
 
      CALL TCASE   ( 'Check for error when using tiny buffer.' )
      CALL ZZASCII ( FILES(1), XSHRTL, .FALSE., OUTTRM )
      CALL CHCKXC  ( .TRUE., 'SPICE(STRINGTOOSHORT)', OK )
 
 
C
C     Ensure ZZASCII signals an error for a file that cannot be opened.
C
 
      CALL TCASE   ( 'Check for error when cannot open the file.' )
      CALL ZZASCII ( 'hakuna-matata', WRKLIN, .FALSE., OUTTRM )
      CALL CHCKXC  ( .TRUE., 'SPICE(FILEOPENFAIL)', OK )
 
 
 
 
 
C
C     Set ZZASCII exception flag and exception message for each file.
C     Since all files exist and buffer is big enough there should not
C     be any exceptions on any of the platforms.
C
      DO I = 1, NFILES
         MYZZEF( I ) = .FALSE.
         MYZZEM( I ) = ' '
      END DO
 
C
C     Set ZZASCII output terminator for each file. These are a
C     different story. We assign them as they should be detected
C     everywhere, but reassign some/all for the platforms that don't
C     "behave."
C
      MYTERM(  1 ) = '?'
      MYTERM(  2 ) = '?'
      MYTERM(  3 ) = '?'
      MYTERM(  4 ) = 'CR'
      MYTERM(  5 ) = 'LF'
      MYTERM(  6 ) = 'CR-LF'
      MYTERM(  7 ) = 'CR'
      MYTERM(  8 ) = 'LF'
      MYTERM(  9 ) = 'CR-LF'
      MYTERM( 10 ) = '?'
      MYTERM( 11 ) = '?'
      MYTERM( 12 ) = '?'
 
 
 
 
C
C     The IFORT, GFORTRAN, DIGITAL, and LAHEY seem to
C     generate IOSTAT=0 when read past the end of a file and,
C     therefore, proceed and detect terminators in short files.
C
 
      MYTERM(  1 ) = 'CR'
      MYTERM(  2 ) = 'LF'
      MYTERM(  3 ) = 'CR-LF'
 
 
 
 
 
 
C
C     Set FURNSH exception flag, exception message and loaded flag for
C     each file as if FURNSH wouldn't fail and then reset some of them
C     depending on the platform.
C
      DO I = 1, NFILES
         MYFNEF( I ) = .FALSE.
         MYFNEM( I ) = ' '
      END DO
 
 
 
C
#if defined(SPICE_COMPILER_INTEL)
C     FORTRANs on MS PCs can detect terminators in small and medium
C     files and, therefore, complain about them same way as for the
C     large files.
#else
C     IFORT on Macs and Linux and GFORTRAN everywhere can detect
C     terminators in small and medium files and, therefore, complain
C     about them same way as for the large files.
#endif
C
      MYFNEF(  1 ) = .TRUE.
#if defined(SPICE_COMPILER_INTEL)
      MYFNEF(  2 ) = .TRUE.
#else
      MYFNEF(  3 ) = .TRUE.
#endif
      MYFNEF(  4 ) = .TRUE.
#if defined(SPICE_COMPILER_INTEL)
      MYFNEF(  5 ) = .TRUE.
#else
      MYFNEF(  6 ) = .TRUE.
#endif
      MYFNEF(  7 ) = .TRUE.
#if defined(SPICE_COMPILER_INTEL)
      MYFNEF(  8 ) = .TRUE.
#else
      MYFNEF(  9 ) = .TRUE.
#endif
      MYFNEM(  1 ) = 'SPICE(INCOMPATIBLEEOL)'
#if defined(SPICE_COMPILER_INTEL)
      MYFNEM(  2 ) = 'SPICE(INCOMPATIBLEEOL)'
#else
      MYFNEM(  3 ) = 'SPICE(INCOMPATIBLEEOL)'
#endif
      MYFNEM(  4 ) = 'SPICE(INCOMPATIBLEEOL)'
#if defined(SPICE_COMPILER_INTEL)
      MYFNEM(  5 ) = 'SPICE(INCOMPATIBLEEOL)'
#else
      MYFNEM(  6 ) = 'SPICE(INCOMPATIBLEEOL)'
#endif
      MYFNEM(  7 ) = 'SPICE(INCOMPATIBLEEOL)'
#if defined(SPICE_COMPILER_INTEL)
      MYFNEM(  8 ) = 'SPICE(INCOMPATIBLEEOL)'
#else
      MYFNEM(  9 ) = 'SPICE(INCOMPATIBLEEOL)'
#endif
 
 
C
C     Now go over the files and call ZZASCII and FURNSH on each of them.
C
      DO I = 1, NFILES
 
C
C        Direct check by calling ZZASCII.
C
         CALL TCASE   ( 'Check for EOL terminator in ' // FILES( I ) )
         CALL ZZASCII ( FILES( I ), WRKLIN, .FALSE., OUTTRM )
         CALL CHCKXC  ( MYZZEF( I ), MYZZEM( I ), OK )
         CALL CHCKSC  ( 'Terminator', OUTTRM, '=', MYTERM( I ), OK )
 
C
C        Indirect check by calling FURNSH.
C
         CALL TCASE  ( 'Check what FURNSH does with ' // FILES( I ) )
 
         CALL FURNSH ( FILES( I ) )
         CALL CHCKXC ( MYFNEF( I ), MYFNEM( I ), OK )
 
         IF ( .NOT. MYFNEF( I ) ) THEN
            CALL UNLOAD ( FILES( I ) )
         END IF
 
      END DO
 
C
C     Clean-up: delete test files.
C
      DO I = 1, 12
         CALL KILFIL( FILES(I) )
      END DO
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
 
 
      RETURN
      END
