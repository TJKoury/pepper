!$Procedure F_DDHOPN ( ZZDDHOPN Test Family )

      SUBROUTINE F_DDHOPN ( OK )
      use pepper_legacy, pepper_legacy_exists => exists
      use pepper_legacy, pepper_legacy_exists => exists
      use pepper_inc_zzddhman

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDDHOPN
!     routine.
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
!     This routine exercises ZZDDHOPN's logic.  There are a few IOSTAT
!     driven exceptions that are not readily exercised with the
!     delivered source code.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) Assumes that the string labels for any supported architecture
!        are no more than 3 characters in length.
!
!     2) An acceptable file ID word for any supported file architecture
!        is of the form:
!
!           ARC/TYPE
!
!        where ARC is the at most 3 character string code for the
!        architecture, and TYPE is the 4 character type string.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    TSPICE Version 2.26.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    TSPICE Version 2.25.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    TSPICE Version 2.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    TSPICE Version 2.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    TSPICE Version 2.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    TSPICE Version 2.21.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    TSPICE Version 2.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    TSPICE Version 2.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    TSPICE Version 2.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    TSPICE Version 2.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    TSPICE Version 2.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    TSPICE Version 2.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    TSPICE Version 2.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    TSPICE Version 2.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    TSPICE Version 2.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    TSPICE Version 2.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    TSPICE Version 2.10.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    TSPICE Version 2.9.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    TSPICE Version 2.8.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    TSPICE Version 2.7.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    TSPICE Version 2.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    TSPICE Version 2.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    TSPICE Version 2.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    TSPICE Version 2.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    TSPICE Version 2.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    TSPICE Version 2.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    TSPICE Version 2.0.0, 07-AUG-2002 (FST)
!
!        Updated module as the result of changes to the handle
!        manager interface, ZZDDHCLS.
!
!        Add MAC OS/X environments.
!
!-    TSPICE Version 1.0.0, 13-NOV-2001 (FST)
!
!-&

!
!     SPICELIB Functions
!

!
!     Local Variables
!
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

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DDHOPN' )

!
!     Setup the filename pattern for loop tests.
!
      FNMPAT = 'test#.fil'

!
!     Retrieve the native binary file format string to use
!     when creating test files.
!
      CALL ZZPLATFM ( 'FILE_FORMAT', BINFMT )

!
!     Retrieve other initialization data.
!
      CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, STRAMH, STRARC, STRBFF )

!
!     Setup IDWARY, an array that contains an acceptable file ID word
!     for each supported architecture.
!
      DO I = 1, NUMARC
         CALL REPMC ( '#/TEST', '#', STRARC(I), IDWARY(I) )
      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unsupported Access Method Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'FAILURE-TEST'
      ARCH   = 'DAF'
      HANDLE = 1

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDMETHOD)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid File Architecture Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ARCH   = 'FAILURE-TEST'
      HANDLE = 1

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDARCH)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No free lockable units on SCRATCH Open Exception.' )

!
!     Setup the inputs and outputs.  The easiest way to do this is
!     open (UTSIZE-RSVUNT) scratch files in a loop.
!
      DO I = 1, ( UTSIZE - RSVUNT )
         CALL ZZDDHOPN ( ' ', 'SCRATCH', 'DAS', HANLCK(I) )
      END DO

      FNAME  = 'test.fil'
      METHOD = 'SCRATCH'
      ARCH   = 'DAS'
      HANDLE = 1

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(UTFULL)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!     Clean up the locked files.
!
      DO I = 1, ( UTSIZE - RSVUNT )
         CALL ZZDDHCLS ( HANLCK(I), 'DAS', .FALSE. )
      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'READ Blank Filename Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = ' '
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 1

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKFILENAME)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'NEW Blank Filename Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = ' '
      METHOD = 'NEW'
      ARCH   = 'DAF'
      HANDLE = 1

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKFILENAME)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'WRITE Blank Filename Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = ' '
      METHOD = 'WRITE'
      ARCH   = 'DAF'
      HANDLE = 1

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKFILENAME)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'READ Non-Existent File Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 1

      CALL KILFIL ( FNAME )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILENOTFOUND)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'WRITE Non-Existent File Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'WRITE'
      ARCH   = 'DAF'
      HANDLE = 1

      CALL KILFIL ( FNAME )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILENOTFOUND)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'READ File Already Connected Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 1

      CALL KILFIL ( FNAME )

!
!     Create FNAME and attach it to a logical unit.
!
      CALL T_CPTFIL ( FNAME, &
                      DAF, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAF/SPK '  )

      CALL GETLUN ( UNIT )

      OPEN ( UNIT   = UNIT, &
             FILE   = FNAME, &
             ACCESS = 'DIRECT', &
             RECL   = RECL, &
             STATUS = 'OLD', &
             IOSTAT = IOSTAT    )

!
!     Check IOSTAT.
!
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(IMPROPEROPEN)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!     Remove the file.
!
      CLOSE ( UNIT = UNIT )

      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'WRITE File Already Connected Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'WRITE'
      ARCH   = 'DAF'
      HANDLE = 1

      CALL KILFIL ( FNAME )

!
!     Create FNAME and attach it to a logical unit.
!
      CALL T_CPTFIL ( FNAME, &
                      DAF, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAF/SPK '  )

      CALL GETLUN ( UNIT )

      OPEN ( UNIT   = UNIT, &
             FILE   = FNAME, &
             ACCESS = 'DIRECT', &
             RECL   = RECL, &
             STATUS = 'OLD', &
             IOSTAT = IOSTAT    )

!
!     Check IOSTAT.
!
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(IMPROPEROPEN)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!     Remove the file.
!
      CLOSE ( UNIT = UNIT )

      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'READ DAF Architecture Mismatch Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 1

      CALL KILFIL ( FNAME )

      ALTFNM = FNAME
      ALTMTH = METHOD
      ALTARC = 'DAS'
      ALTHAN = 0

!
!     Create FNAME.
!
      CALL T_CPTFIL ( ALTFNM, &
                      DAS, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAS/EK  '  )

!
!     Open the file as a DAS to prepare for the conflict.
!
      CALL ZZDDHOPN ( ALTFNM, ALTMTH, ALTARC, ALTHAN )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!     Close and remove the file.
!
      CALL ZZDDHCLS ( ALTHAN, ALTARC, .FALSE. )

      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'WRITE DAF Architecture Mismatch Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'WRITE'
      ARCH   = 'DAF'
      HANDLE = 1

      CALL KILFIL ( FNAME )

      ALTFNM = FNAME
      ALTMTH = METHOD
      ALTARC = 'DAS'
      ALTHAN = 0

!
!     Create FNAME.
!
      CALL T_CPTFIL ( FNAME, &
                      DAS, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAS/EK  '  )

!
!     Open the file as a DAS to prepare for the conflict.
!
      CALL ZZDDHOPN ( ALTFNM, ALTMTH, ALTARC, ALTHAN )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!     Close and remove the file.
!
      CALL ZZDDHCLS ( ALTHAN, ALTARC, .FALSE. )

      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'READ DAS Architecture Mismatch Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAS'
      HANDLE = 1

      CALL KILFIL ( FNAME )

      ALTFNM = FNAME
      ALTMTH = METHOD
      ALTARC = 'DAF'
      ALTHAN = 0

!
!     Create FNAME.
!
      CALL T_CPTFIL ( FNAME, &
                      DAF, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAF/SPK '  )

!
!     Open the file as a DAF to prepare for the conflict.
!
      CALL ZZDDHOPN ( ALTFNM, ALTMTH, ALTARC, ALTHAN )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!     Close and remove the file.
!
      CALL ZZDDHCLS ( ALTHAN, ALTARC, .FALSE. )

      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'WRITE DAS Architecture Mismatch Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'WRITE'
      ARCH   = 'DAS'
      HANDLE = 1

      CALL KILFIL ( FNAME )

      ALTFNM = FNAME
      ALTMTH = METHOD
      ALTARC = 'DAF'
      ALTHAN = 0

!
!     Create FNAME.
!
      CALL T_CPTFIL ( FNAME, &
                      DAF, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAF/SPK '  )

!
!     Open the file as a DAF to prepare for the conflict.
!
      CALL ZZDDHOPN ( ALTFNM, ALTMTH, ALTARC, ALTHAN )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!     Close and remove the file.
!
      CALL ZZDDHCLS ( ALTHAN, ALTARC, .FALSE. )

      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DAF WRITE Open Conflict Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'WRITE'
      ARCH   = 'DAF'
      HANDLE = 1

      CALL KILFIL ( FNAME )

      ALTFNM = FNAME
      ALTMTH = 'READ'
      ALTARC = ARCH
      ALTHAN = 0

!
!     Create FNAME.
!
      CALL T_CPTFIL ( FNAME, &
                      DAF, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAF/SPK '  )

!
!     Open the file for READ access to prepare for the conflict.
!
      CALL ZZDDHOPN ( ALTFNM, ALTMTH, ALTARC, ALTHAN )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILEOPENCONFLICT)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!     Close and remove the file.
!
      CALL ZZDDHCLS ( ALTHAN, ALTARC, .FALSE. )

      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DAS WRITE Open Conflict Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'WRITE'
      ARCH   = 'DAS'
      HANDLE = 1

      CALL KILFIL ( FNAME )

      ALTFNM = FNAME
      ALTMTH = 'READ'
      ALTARC = ARCH
      ALTHAN = 0

!
!     Create FNAME.
!
      CALL T_CPTFIL ( FNAME, &
                      DAS, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAS/EK  '  )

!
!     Open the file for READ access to prepare for the conflict.
!
      CALL ZZDDHOPN ( ALTFNM, ALTMTH, ALTARC, ALTHAN )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILEOPENCONFLICT)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!     Close and remove the file.
!
      CALL ZZDDHCLS ( ALTHAN, ALTARC, .FALSE. )

      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DAF READ/WRITE Conflict Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 1

      CALL KILFIL ( FNAME )

      ALTFNM = FNAME
      ALTMTH = 'WRITE'
      ALTARC = ARCH
      ALTHAN = 0

!
!     Create FNAME.
!
      CALL T_CPTFIL ( FNAME, &
                      DAF, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAF/SPK '  )

!
!     Open the file for READ access to prepare for the conflict.
!
      CALL ZZDDHOPN ( ALTFNM, ALTMTH, ALTARC, ALTHAN )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(RWCONFLICT)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!     Close and remove the file.
!
      CALL ZZDDHCLS ( ALTHAN, ALTARC, .FALSE. )

      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DAS READ/WRITE Conflict Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAS'
      HANDLE = 1

      CALL KILFIL ( FNAME )

      ALTFNM = FNAME
      ALTMTH = 'WRITE'
      ALTARC = ARCH
      ALTHAN = 0

!
!     Create FNAME.
!
      CALL T_CPTFIL ( FNAME, &
                      DAS, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAS/EK  '  )

!
!     Open the file for READ access to prepare for the conflict.
!
      CALL ZZDDHOPN ( ALTFNM, ALTMTH, ALTARC, ALTHAN )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(RWCONFLICT)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!     Close and remove the file.
!
      CALL ZZDDHCLS ( ALTHAN, ALTARC, .FALSE. )

      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
!
!     PEPPER: the file table grows (SPICELIB's held FTSIZE files and
!     signaled SPICE(FTFULL) for one more): opening one more file than
!     FTSIZE succeeds.
!
      CALL TCASE ( 'File Table Past FTSIZE (PEPPER: no FTFULL).' )

!
!     Setup the inputs and outputs.
!
      ARCH = 'DAF'

      DO I = 1, FTSIZE
         CALL REPMI ( FNMPAT, '#', I, FNAME )
         CALL ZZDDHOPN ( FNAME, 'NEW', ARCH, HANTST(I) )
      END DO

      FNAME  = 'test.fil'
      METHOD = 'NEW'
      HANDLE = 0

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check that there is no exception, and that the handle is the
!     next one (negative: the file is open for writing).
!
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANTST(FTSIZE)-1, 0, OK )

!
!     Close and remove the file.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )

      CALL KILFIL ( FNAME )

!
!     Close and remove all the files used to put the system
!     into the FTFULL state.
!
      DO I = 1, FTSIZE
         CALL ZZDDHCLS ( HANTST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT,    '#', I, FNAME )
         CALL KILFIL   ( FNAME                    )
      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'NEW File Open IOSTAT Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ARCH   = 'DAS'
      HANDLE = 1

      CALL KILFIL ( FNAME )

!
!     Create FNAME.
!
      CALL T_CPTFIL ( FNAME, &
                      DAS, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAS/EK  '  )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILEOPENFAIL)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!     Close the file.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )

!
!     Remove the file.
!
      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'FTP Error Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAS'
      HANDLE = 1

      CALL KILFIL ( FNAME )

!
!     Create FNAME.
!
      CALL T_CPTFIL ( FNAME, &
                      DAS, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .TRUE., &
                      'DAS/EK  '  )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FTPXFERERROR)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!     Close the file.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )

!
!     Remove the file.
!
      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ID Word, Architecture Input Mismatch Exception.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 1

      CALL KILFIL ( FNAME )

!
!     Create FNAME.
!
      CALL T_CPTFIL ( FNAME, &
                      DAS, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAS/EK  '  )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCHMISMATCH)', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!     Close the file.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )

!
!     Remove the file.
!
      CALL KILFIL ( FNAME )

!
!     At this point we have exercised most of the readily available
!     exceptions that signal errors.  Now address exceptions that
!     do not signal errors.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Multiple OPEN for READ access.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 0
      ALTHAN = 0

      CALL KILFIL ( FNAME )

!
!     Create FNAME.
!
      CALL T_CPTFIL ( FNAME, &
                      DAF, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAF/CK  '  )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Try to open the file for read again.  This time store the
!     handle returned in ALTHAN.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, ALTHAN )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check HANDLE.
!
      CALL CHCKSI ( 'ALTHAN', ALTHAN, '=', HANDLE, 0, OK )

!
!     Close the file.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )

!
!     Remove the file.
!
      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check UNIT connected to newly opened file.' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 0

      CALL KILFIL ( FNAME )

!
!     Create FNAME.
!
      CALL T_CPTFIL ( FNAME, &
                      DAF, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAF/CK  '  )

!
!     Invoke the module.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     INQUIRE on FNAME to see if it is attached to UNIT.
!
      INQUIRE ( FILE   = FNAME, &
                NUMBER = ALTUNT, &
                EXIST  = EXISTS, &
                OPENED = OPENED, &
                IOSTAT = IOSTAT  )

!
!     Check open and exists to make certain that they are appropriate
!     values.
!
      CALL CHCKSL ( 'OPENED', OPENED, .TRUE., OK )
      CALL CHCKSL ( 'EXISTS', EXISTS, .TRUE., OK )

!
!     Get the UNIT associated with HANDLE.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, .FALSE., UNIT )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check IOSTAT for failure.
!
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )

!
!     Compare ALTUNT to UNIT.
!
      CALL CHCKSI ( 'UNIT', UNIT, '=', ALTUNT, 0, OK )

!
!     Close the file.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )

!
!     Remove the file.
!
      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Exercise Unsupported BFF Exception.' )

!
!     Loop over all possible non-native configurations guaranteed
!     to fail.
!
      DO I = 1, NUMBFF

!
!        Start by handling WRITE access.  All non-native formats
!        are not supported for writing.
!
         IF ( I .NE. NATBFF ) THEN

            DO J = 1, NUMARC

!
!              Create a test file for each architecture.
!
               FNAME  = 'test.fil'

               CALL KILFIL ( FNAME )

!
!              Create FNAME.
!
               CALL T_CPTFIL ( FNAME, &
                               J, &
                               2, &
                               STRBFF(I), &
                               'ABCD', &
                               'EFGH', &
                               'IJKL', &
                               .TRUE., &
                               .FALSE., &
                               IDWARY(J)  )

!
!              Begin the test by attempting to open the
!              non-native file for WRITE access.
!
               ARCH   = STRARC(J)
               METHOD = 'WRITE'
               HANDLE = 1

!
!              Invoke the module.
!
               CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!              Check for the exception.
!
               CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDBFF)', OK )

!
!              Check the value of HANDLE.
!
               CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!              Now if I is not in SUPBFF, attempt to open it for
!              READ access.
!
               IF ( ISRCHI( I, NUMSUP, SUPBFF ) .EQ. 0 ) THEN

                  ARCH   = STRARC(J)
                  METHOD = 'READ'
                  HANDLE = 1

!
!                 Invoke the module.
!
                  CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!                 Check for the exception.
!
                  CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDBFF)', OK )

!
!                 Check the value of HANDLE.
!
                  CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

               END IF

!
!              All tests related to FNAME are complete.  Delete it.
!
               CALL KILFIL ( FNAME )

            END DO

         END IF

      END DO




!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
