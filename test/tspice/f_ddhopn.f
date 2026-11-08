C$Procedure F_DDHOPN ( ZZDDHOPN Test Family )
 
      SUBROUTINE F_DDHOPN ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDDHOPN
C     routine.
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
C     This routine exercises ZZDDHOPN's logic.  There are a few IOSTAT
C     driven exceptions that are not readily exercised with the
C     delivered source code.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) Assumes that the string labels for any supported architecture
C        are no more than 3 characters in length.
C
C     2) An acceptable file ID word for any supported file architecture
C        is of the form:
C
C           ARC/TYPE
C
C        where ARC is the at most 3 character string code for the
C        architecture, and TYPE is the 4 character type string.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    TSPICE Version 2.26.0, 28-NOV-2021 (BVS)
C
C        Updated for MAC-OSX-M1-64BIT-CLANG_C.
C
C-    TSPICE Version 2.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    TSPICE Version 2.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    TSPICE Version 2.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    TSPICE Version 2.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    TSPICE Version 2.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    TSPICE Version 2.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    TSPICE Version 2.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    TSPICE Version 2.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    TSPICE Version 2.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    TSPICE Version 2.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    TSPICE Version 2.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    TSPICE Version 2.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    TSPICE Version 2.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    TSPICE Version 2.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    TSPICE Version 2.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    TSPICE Version 2.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    TSPICE Version 2.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    TSPICE Version 2.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    TSPICE Version 2.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    TSPICE Version 2.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    TSPICE Version 2.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    TSPICE Version 2.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    TSPICE Version 2.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    TSPICE Version 2.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    TSPICE Version 2.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    TSPICE Version 2.0.0, 07-AUG-2002 (FST)
C
C        Updated module as the result of changes to the handle
C        manager interface, ZZDDHCLS.
C
C        Add MAC OS/X environments.
C
C-    TSPICE Version 1.0.0, 13-NOV-2001 (FST)
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               ISRCHI
 
C
C     Local Variables
C
      CHARACTER*(20)        ALTARC
      CHARACTER*(FILEN)     ALTFNM
      CHARACTER*(20)        ALTMTH
      CHARACTER*(20)        ARCH
      CHARACTER*(20)        BINFMT
      CHARACTER*(8)         IDWARY ( NUMARC )
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMPAT
      CHARACTER*(20)        METHOD
      CHARACTER*(STRSIZ)    STRAMH ( NUMAMH )
      CHARACTER*(STRSIZ)    STRARC ( NUMARC )
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
 
      INTEGER               ALTHAN
      INTEGER               ALTUNT
      INTEGER               HANDLE
      INTEGER               HANLCK ( UTSIZE - RSVUNT )
      INTEGER               HANTST ( FTSIZE )
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               J
      INTEGER               NATBFF
      INTEGER               NUMSUP
      INTEGER               SUPBFF ( NUMBFF )
      INTEGER               UNIT
 
      LOGICAL               EXISTS
      LOGICAL               OPENED
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DDHOPN' )
 
C
C     Setup the filename pattern for loop tests.
C
      FNMPAT = 'test#.fil'
 
C
C     Retrieve the native binary file format string to use
C     when creating test files.
C
      CALL ZZPLATFM ( 'FILE_FORMAT', BINFMT )
 
C
C     Retrieve other initialization data.
C
      CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, STRAMH, STRARC, STRBFF )
 
C
C     Setup IDWARY, an array that contains an acceptable file ID word
C     for each supported architecture.
C
      DO I = 1, NUMARC
         CALL REPMC ( '#/TEST', '#', STRARC(I), IDWARY(I) )
      END DO
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Unsupported Access Method Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'FAILURE-TEST'
      ARCH   = 'DAF'
      HANDLE = 1
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDMETHOD)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid File Architecture Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ARCH   = 'FAILURE-TEST'
      HANDLE = 1
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDARCH)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No free lockable units on SCRATCH Open Exception.' )
 
C
C     Setup the inputs and outputs.  The easiest way to do this is
C     open (UTSIZE-RSVUNT) scratch files in a loop.
C
      DO I = 1, ( UTSIZE - RSVUNT )
         CALL ZZDDHOPN ( ' ', 'SCRATCH', 'DAS', HANLCK(I) )
      END DO
 
      FNAME  = 'test.fil'
      METHOD = 'SCRATCH'
      ARCH   = 'DAS'
      HANDLE = 1
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(UTFULL)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Clean up the locked files.
C
      DO I = 1, ( UTSIZE - RSVUNT )
         CALL ZZDDHCLS ( HANLCK(I), 'DAS', .FALSE. )
      END DO
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'READ Blank Filename Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = ' '
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 1
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKFILENAME)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'NEW Blank Filename Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = ' '
      METHOD = 'NEW'
      ARCH   = 'DAF'
      HANDLE = 1
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKFILENAME)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'WRITE Blank Filename Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = ' '
      METHOD = 'WRITE'
      ARCH   = 'DAF'
      HANDLE = 1
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKFILENAME)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'READ Non-Existent File Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 1
 
      CALL KILFIL ( FNAME )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILENOTFOUND)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'WRITE Non-Existent File Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'WRITE'
      ARCH   = 'DAF'
      HANDLE = 1
 
      CALL KILFIL ( FNAME )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILENOTFOUND)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'READ File Already Connected Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 1
 
      CALL KILFIL ( FNAME )
 
C
C     Create FNAME and attach it to a logical unit.
C
      CALL T_CPTFIL ( FNAME,
     .                DAF,
     .                2,
     .                BINFMT,
     .                'ABCD',
     .                'EFGH',
     .                'IJKL',
     .                .TRUE.,
     .                .FALSE.,
     .                'DAF/SPK '  )
 
      CALL GETLUN ( UNIT )
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = FNAME,
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(IMPROPEROPEN)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'WRITE File Already Connected Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'WRITE'
      ARCH   = 'DAF'
      HANDLE = 1
 
      CALL KILFIL ( FNAME )
 
C
C     Create FNAME and attach it to a logical unit.
C
      CALL T_CPTFIL ( FNAME,
     .                DAF,
     .                2,
     .                BINFMT,
     .                'ABCD',
     .                'EFGH',
     .                'IJKL',
     .                .TRUE.,
     .                .FALSE.,
     .                'DAF/SPK '  )
 
      CALL GETLUN ( UNIT )
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = FNAME,
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(IMPROPEROPEN)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'READ DAF Architecture Mismatch Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 1
 
      CALL KILFIL ( FNAME )
 
      ALTFNM = FNAME
      ALTMTH = METHOD
      ALTARC = 'DAS'
      ALTHAN = 0
 
C
C     Create FNAME.
C
      CALL T_CPTFIL ( ALTFNM,
     .                DAS,
     .                2,
     .                BINFMT,
     .                'ABCD',
     .                'EFGH',
     .                'IJKL',
     .                .TRUE.,
     .                .FALSE.,
     .                'DAS/EK  '  )
 
C
C     Open the file as a DAS to prepare for the conflict.
C
      CALL ZZDDHOPN ( ALTFNM, ALTMTH, ALTARC, ALTHAN )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CALL ZZDDHCLS ( ALTHAN, ALTARC, .FALSE. )
 
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'WRITE DAF Architecture Mismatch Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'WRITE'
      ARCH   = 'DAF'
      HANDLE = 1
 
      CALL KILFIL ( FNAME )
 
      ALTFNM = FNAME
      ALTMTH = METHOD
      ALTARC = 'DAS'
      ALTHAN = 0
 
C
C     Create FNAME.
C
      CALL T_CPTFIL ( FNAME,
     .                DAS,
     .                2,
     .                BINFMT,
     .                'ABCD',
     .                'EFGH',
     .                'IJKL',
     .                .TRUE.,
     .                .FALSE.,
     .                'DAS/EK  '  )
 
C
C     Open the file as a DAS to prepare for the conflict.
C
      CALL ZZDDHOPN ( ALTFNM, ALTMTH, ALTARC, ALTHAN )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CALL ZZDDHCLS ( ALTHAN, ALTARC, .FALSE. )
 
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'READ DAS Architecture Mismatch Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAS'
      HANDLE = 1
 
      CALL KILFIL ( FNAME )
 
      ALTFNM = FNAME
      ALTMTH = METHOD
      ALTARC = 'DAF'
      ALTHAN = 0
 
C
C     Create FNAME.
C
      CALL T_CPTFIL ( FNAME,
     .                DAF,
     .                2,
     .                BINFMT,
     .                'ABCD',
     .                'EFGH',
     .                'IJKL',
     .                .TRUE.,
     .                .FALSE.,
     .                'DAF/SPK '  )
 
C
C     Open the file as a DAF to prepare for the conflict.
C
      CALL ZZDDHOPN ( ALTFNM, ALTMTH, ALTARC, ALTHAN )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CALL ZZDDHCLS ( ALTHAN, ALTARC, .FALSE. )
 
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'WRITE DAS Architecture Mismatch Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'WRITE'
      ARCH   = 'DAS'
      HANDLE = 1
 
      CALL KILFIL ( FNAME )
 
      ALTFNM = FNAME
      ALTMTH = METHOD
      ALTARC = 'DAF'
      ALTHAN = 0
 
C
C     Create FNAME.
C
      CALL T_CPTFIL ( FNAME,
     .                DAF,
     .                2,
     .                BINFMT,
     .                'ABCD',
     .                'EFGH',
     .                'IJKL',
     .                .TRUE.,
     .                .FALSE.,
     .                'DAF/SPK '  )
 
C
C     Open the file as a DAF to prepare for the conflict.
C
      CALL ZZDDHOPN ( ALTFNM, ALTMTH, ALTARC, ALTHAN )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CALL ZZDDHCLS ( ALTHAN, ALTARC, .FALSE. )
 
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DAF WRITE Open Conflict Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'WRITE'
      ARCH   = 'DAF'
      HANDLE = 1
 
      CALL KILFIL ( FNAME )
 
      ALTFNM = FNAME
      ALTMTH = 'READ'
      ALTARC = ARCH
      ALTHAN = 0
 
C
C     Create FNAME.
C
      CALL T_CPTFIL ( FNAME,
     .                DAF,
     .                2,
     .                BINFMT,
     .                'ABCD',
     .                'EFGH',
     .                'IJKL',
     .                .TRUE.,
     .                .FALSE.,
     .                'DAF/SPK '  )
 
C
C     Open the file for READ access to prepare for the conflict.
C
      CALL ZZDDHOPN ( ALTFNM, ALTMTH, ALTARC, ALTHAN )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILEOPENCONFLICT)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CALL ZZDDHCLS ( ALTHAN, ALTARC, .FALSE. )
 
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DAS WRITE Open Conflict Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'WRITE'
      ARCH   = 'DAS'
      HANDLE = 1
 
      CALL KILFIL ( FNAME )
 
      ALTFNM = FNAME
      ALTMTH = 'READ'
      ALTARC = ARCH
      ALTHAN = 0
 
C
C     Create FNAME.
C
      CALL T_CPTFIL ( FNAME,
     .                DAS,
     .                2,
     .                BINFMT,
     .                'ABCD',
     .                'EFGH',
     .                'IJKL',
     .                .TRUE.,
     .                .FALSE.,
     .                'DAS/EK  '  )
 
C
C     Open the file for READ access to prepare for the conflict.
C
      CALL ZZDDHOPN ( ALTFNM, ALTMTH, ALTARC, ALTHAN )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILEOPENCONFLICT)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CALL ZZDDHCLS ( ALTHAN, ALTARC, .FALSE. )
 
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DAF READ/WRITE Conflict Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 1
 
      CALL KILFIL ( FNAME )
 
      ALTFNM = FNAME
      ALTMTH = 'WRITE'
      ALTARC = ARCH
      ALTHAN = 0
 
C
C     Create FNAME.
C
      CALL T_CPTFIL ( FNAME,
     .                DAF,
     .                2,
     .                BINFMT,
     .                'ABCD',
     .                'EFGH',
     .                'IJKL',
     .                .TRUE.,
     .                .FALSE.,
     .                'DAF/SPK '  )
 
C
C     Open the file for READ access to prepare for the conflict.
C
      CALL ZZDDHOPN ( ALTFNM, ALTMTH, ALTARC, ALTHAN )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(RWCONFLICT)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CALL ZZDDHCLS ( ALTHAN, ALTARC, .FALSE. )
 
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DAS READ/WRITE Conflict Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAS'
      HANDLE = 1
 
      CALL KILFIL ( FNAME )
 
      ALTFNM = FNAME
      ALTMTH = 'WRITE'
      ALTARC = ARCH
      ALTHAN = 0
 
C
C     Create FNAME.
C
      CALL T_CPTFIL ( FNAME,
     .                DAS,
     .                2,
     .                BINFMT,
     .                'ABCD',
     .                'EFGH',
     .                'IJKL',
     .                .TRUE.,
     .                .FALSE.,
     .                'DAS/EK  '  )
 
C
C     Open the file for READ access to prepare for the conflict.
C
      CALL ZZDDHOPN ( ALTFNM, ALTMTH, ALTARC, ALTHAN )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(RWCONFLICT)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CALL ZZDDHCLS ( ALTHAN, ALTARC, .FALSE. )
 
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'File Table Full Exception.' )
 
C
C     Setup the inputs and outputs.
C
      ARCH = 'DAF'
 
      DO I = 1, FTSIZE
         CALL REPMI ( FNMPAT, '#', I, FNAME )
         CALL ZZDDHOPN ( FNAME, 'NEW', ARCH, HANTST(I) )
      END DO
 
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      HANDLE = 0
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FTFULL)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
 
      CALL KILFIL ( FNAME )
 
C
C     Close and remove all the files used to put the system
C     into the FTFULL state.
C
      DO I = 1, FTSIZE
         CALL ZZDDHCLS ( HANTST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT,    '#', I, FNAME )
         CALL KILFIL   ( FNAME                    )
      END DO
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'NEW File Open IOSTAT Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ARCH   = 'DAS'
      HANDLE = 1
 
      CALL KILFIL ( FNAME )
 
C
C     Create FNAME.
C
      CALL T_CPTFIL ( FNAME,
     .                DAS,
     .                2,
     .                BINFMT,
     .                'ABCD',
     .                'EFGH',
     .                'IJKL',
     .                .TRUE.,
     .                .FALSE.,
     .                'DAS/EK  '  )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILEOPENFAIL)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Close the file.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
 
C
C     Remove the file.
C
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP Error Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAS'
      HANDLE = 1
 
      CALL KILFIL ( FNAME )
 
C
C     Create FNAME.
C
      CALL T_CPTFIL ( FNAME,
     .                DAS,
     .                2,
     .                BINFMT,
     .                'ABCD',
     .                'EFGH',
     .                'IJKL',
     .                .TRUE.,
     .                .TRUE.,
     .                'DAS/EK  '  )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FTPXFERERROR)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Close the file.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
 
C
C     Remove the file.
C
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ID Word, Architecture Input Mismatch Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 1
 
      CALL KILFIL ( FNAME )
 
C
C     Create FNAME.
C
      CALL T_CPTFIL ( FNAME,
     .                DAS,
     .                2,
     .                BINFMT,
     .                'ABCD',
     .                'EFGH',
     .                'IJKL',
     .                .TRUE.,
     .                .FALSE.,
     .                'DAS/EK  '  )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCHMISMATCH)', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Close the file.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
 
C
C     Remove the file.
C
      CALL KILFIL ( FNAME )
 
C
C     At this point we have exercised most of the readily available
C     exceptions that signal errors.  Now address exceptions that
C     do not signal errors.
C
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Multiple OPEN for READ access.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 0
      ALTHAN = 0
 
      CALL KILFIL ( FNAME )
 
C
C     Create FNAME.
C
      CALL T_CPTFIL ( FNAME,
     .                DAF,
     .                2,
     .                BINFMT,
     .                'ABCD',
     .                'EFGH',
     .                'IJKL',
     .                .TRUE.,
     .                .FALSE.,
     .                'DAF/CK  '  )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Try to open the file for read again.  This time store the
C     handle returned in ALTHAN.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, ALTHAN )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check HANDLE.
C
      CALL CHCKSI ( 'ALTHAN', ALTHAN, '=', HANDLE, 0, OK )
 
C
C     Close the file.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
 
C
C     Remove the file.
C
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check UNIT connected to newly opened file.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 0
 
      CALL KILFIL ( FNAME )
 
C
C     Create FNAME.
C
      CALL T_CPTFIL ( FNAME,
     .                DAF,
     .                2,
     .                BINFMT,
     .                'ABCD',
     .                'EFGH',
     .                'IJKL',
     .                .TRUE.,
     .                .FALSE.,
     .                'DAF/CK  '  )
 
C
C     Invoke the module.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     INQUIRE on FNAME to see if it is attached to UNIT.
C
      INQUIRE ( FILE   = FNAME,
     .          NUMBER = ALTUNT,
     .          EXIST  = EXISTS,
     .          OPENED = OPENED,
     .          IOSTAT = IOSTAT  )
 
C
C     Check open and exists to make certain that they are appropriate
C     values.
C
      CALL CHCKSL ( 'OPENED', OPENED, .TRUE., OK )
      CALL CHCKSL ( 'EXISTS', EXISTS, .TRUE., OK )
 
C
C     Get the UNIT associated with HANDLE.
C
      CALL ZZDDHHLU ( HANDLE, ARCH, .FALSE., UNIT )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check IOSTAT for failure.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Compare ALTUNT to UNIT.
C
      CALL CHCKSI ( 'UNIT', UNIT, '=', ALTUNT, 0, OK )
 
C
C     Close the file.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
 
C
C     Remove the file.
C
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Exercise Unsupported BFF Exception.' )
 
C
C     Loop over all possible non-native configurations guaranteed
C     to fail.
C
      DO I = 1, NUMBFF
 
C
C        Start by handling WRITE access.  All non-native formats
C        are not supported for writing.
C
         IF ( I .NE. NATBFF ) THEN
 
            DO J = 1, NUMARC
 
C
C              Create a test file for each architecture.
C
               FNAME  = 'test.fil'
 
               CALL KILFIL ( FNAME )
 
C
C              Create FNAME.
C
               CALL T_CPTFIL ( FNAME,
     .                         J,
     .                         2,
     .                         STRBFF(I),
     .                         'ABCD',
     .                         'EFGH',
     .                         'IJKL',
     .                         .TRUE.,
     .                         .FALSE.,
     .                         IDWARY(J)  )
 
C
C              Begin the test by attempting to open the
C              non-native file for WRITE access.
C
               ARCH   = STRARC(J)
               METHOD = 'WRITE'
               HANDLE = 1
 
C
C              Invoke the module.
C
               CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C              Check for the exception.
C
               CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDBFF)', OK )
 
C
C              Check the value of HANDLE.
C
               CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C              Now if I is not in SUPBFF, attempt to open it for
C              READ access.
C
               IF ( ISRCHI( I, NUMSUP, SUPBFF ) .EQ. 0 ) THEN
 
                  ARCH   = STRARC(J)
                  METHOD = 'READ'
                  HANDLE = 1
 
C
C                 Invoke the module.
C
                  CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C                 Check for the exception.
C
                  CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDBFF)', OK )
 
C
C                 Check the value of HANDLE.
C
                  CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
               END IF
 
C
C              All tests related to FNAME are complete.  Delete it.
C
               CALL KILFIL ( FNAME )
 
            END DO
 
         END IF
 
      END DO
 
 
 
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
