!$Procedure  F_ZZASC1 ( Family of tests for ZZASCII )

      SUBROUTINE F_ZZASC1 ( OK )

      IMPLICIT NONE

! $ Abstract
!
!     Test family for the ZZASCII routine. The tests confirm correct
!     responses when scanning differently terminated text files by
!     ZZASCII and ZZLDKER.
!
! $ Required_Reading
!
!     TBD.
!
! $ Keywords
!
!     TBD.
!
! $ Declarations
!
!     TBD.
!
! $ Brief_I/O
!
!     TBD.
!
! $ Detailed_Input
!
!     TBD.
!
! $ Detailed_Output
!
!     TBD.
!
! $ Parameters
!
!     TBD.
!
! $ Exceptions
!
!     TBD.
!
! $ Files
!
!     TBD.
!
! $ Particulars
!
!     TBD.
!
! $ Examples
!
!     TBD.
!
! $ Restrictions
!
!     TBD.
!
! $ Literature_References
!
!     TBD.
!
! $ Author_and_Institution
!
!     E.D. Wright      (JPL)
!
! $ Version
!
!-    TSPICE Version 1.26.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    TSPICE Version 1.25.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    TSPICE Version 1.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    TSPICE Version 1.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    TSPICE Version 1.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    TSPICE Version 1.21.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    TSPICE Version 1.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    TSPICE Version 1.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    TSPICE Version 1.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    TSPICE Version 1.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    TSPICE Version 1.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    TSPICE Version 1.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    TSPICE Version 1.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    TSPICE Version 1.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    TSPICE Version 1.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    TSPICE Version 1.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    TSPICE Version 1.10.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    TSPICE Version 1.9.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    TSPICE Version 1.8.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    TSPICE Version 1.7.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    TSPICE Version 1.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    TSPICE Version 1.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    TSPICE Version 1.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    TSPICE Version 1.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    TSPICE Version 1.2.0, 03-OCT-2005 (EDW)
!
!        Updated to block-out all code when processed for C
!        environments. Due to the non-native text kernel capability
!        in CSPICE, tspice_c should not perform zzascii tests - so
!        always return a success.
!
!-    TSPICE Version 1.0.0, 11-FEB-2004 (EDW)
!
!-&

      LOGICAL               OK





      INTEGER               RECLEN
      PARAMETER           ( RECLEN   = 32 )

      INTEGER               SHRT
      PARAMETER           ( SHRT   = RECLEN/2 )

!
!     Write ascii text containing line terminators
!     for each platform, Macintosh classic, Unix,
!     and DOS.
!
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

!
!     SPICELIB functions.
!
      LOGICAL               EXISTS
      LOGICAL               EQSTR


!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZASC1' )

!
!     Define the data to write to test files. ID the
!     file as an LSK kernel, with a begin text block - thus
!     we don't risk damaging any kernel pool variable
!     assignments. The third line contains the data to write.
!     Thirty-two characters written forty times.
!
      LINE (1) = 'KPL/LSK                         '
      LINE (2) = '\begintext                      '
      LINE (3) = '12345678901234567890123456789012'

!
!     Initialize the file names and the corresponding line
!     terminators.
!
      FILES(1) = 'mac.ker'
      FILES(2) = 'unix.ker'
      FILES(3) = 'dos.ker'

      TERMIN(1)(1:2) = CHAR(13)
      TERMIN(2)(1:2) = CHAR(10)
      TERMIN(3)(1:2) = CHAR(13)//CHAR(10)

      EXPECT(1) = 'CR'
      EXPECT(2) = 'LF'
      EXPECT(3) = 'CR-LF'


!
!     Initialize the REC marker value. Start from the
!     beginning of the file.
!
      REC = 1

!
!     Loop over the three terminator types. Create
!     a test file with an appropriate name for the terminator
!     character.
!
      DO I=1, 3

!
!     If the test file exists, delete it.
!
         IF ( EXISTS( FILES(I) ) ) THEN

            CALL KILFIL( FILES(I) )

         END IF

!
!        Find a free logical unit for file access.
!
         CALL GETLUN ( NUMBER )

!
!        Open the file as direct access thus suppressing the print
!        of a native line terminator.
!
         OPEN ( UNIT               = NUMBER  , &
                FILE               = FILES(I), &
                ACCESS             = 'DIRECT', &
                STATUS             = 'NEW'   , &
                RECL               = RECLEN  , &
                IOSTAT             = IOSTAT  )

!
!        Add the terminator to the first line.
!
         LINE(1)(31:32) = TERMIN(I)

!
!        Write the line to record REC then increment REC.
!
         WRITE( NUMBER, REC=REC ) LINE(1)(1:32)
         REC = REC + 1

!
!        Add the terminator to the second line.
!

         LINE(2)(31:32) = TERMIN(I)

!
!        Write the line to record REC then increment REC.
!
         WRITE( NUMBER, REC=REC ) LINE(2)(1:32)
         REC = REC + 1

!
!        Add the terminator to the third data line. We
!        write this line 40 times to create a kernel file
!        larger than 1032 (the record size read by GETFAT).
!
         LINE(3)(31:32) = TERMIN(I)

!
!        Write the line, incrementing the record counter each
!        pass.
!
         DO J = 1, 40
            WRITE( NUMBER, REC=REC ) LINE(3)(1:32)
            REC = REC + 1
         END DO

!
!        Finished. Close this file, reset the REC marker for the
!        next file.
!
         CLOSE( NUMBER )
         REC = 1

      END DO


!
! Case 1
!
      CALL TCASE ( 'Check for correct terminator ID' )

!
!     Start tests. Check the correct response for each file.
!     Note, the length of WRKLIN must exceed the declared length of
!     the LINE variable for the test to succeed.
!
      DO I = 1, 3

         CALL ZZASCII ( FILES(I), WRKLIN, .FALSE., OUTTRM )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSC ( 'Terminator', OUTTRM, '=', EXPECT(I), OK )

      END DO


!
! Case 2
!
      CALL TCASE ( 'Check for failure for short line' )

!
!     Test that a scan fails when using a line length shorter than
!     the line length of the first line in the file of interest.
!     In this case we know the test kernels have line
!     lengths of thirty-two.
!
      DO I = 1, 3

         CALL ZZASCII ( FILES(I), SHRTLN, .FALSE., OUTTRM )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSC ( 'Terminator', OUTTRM, '=', '?', OK )

      END DO


!
!     Retrieve the proper line terminator for the platform.
!
      CALL ZZPLATFM ( 'TEXT_FORMAT', WORD )

!
!     Set the index for the native terminator.
!
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

!
!     BAD BUG! This block should never execute.
!
         CALL CHCKXC ( .TRUE., 'UNKNOWNLINETERMINATOR', OK )

!
!        Clean-up. Delete files.
!
         DO I = 1, 3
            CALL KILFIL( FILES(I) )
         END DO

!
!        Close out the test family.
!
         CALL T_SUCCESS ( OK )
         RETURN

      END IF


!
! Case 3
!
      CALL TCASE ('Check for correct FURNSH response, native.')

!
!     Check a native file passes through a FURNSH call without
!     an error signal.
!
      CALL FURNSH ( FILES(NATIVE) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Unload the kernel so we don't mess-up another kernel
!     test family.
!
      CALL UNLOAD ( FILES(NATIVE) )

!
! Case 4
!
      CALL TCASE ('Check for correct FURNSH response, non-native.')

!
!     PEPPER: FURNSH loads text kernels whatever their line terminators
!     (SPICELIB signaled SPICE(INCOMPATIBLEEOL) for a non-native file).
!
      CALL FURNSH ( FILES( FAIL(1) ) )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL UNLOAD ( FILES( FAIL(1) ) )

      CALL FURNSH ( FILES( FAIL(2) ) )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL UNLOAD ( FILES( FAIL(2) ) )


!
! Case 5
!
      CALL TCASE ( 'Check for ZZASCII short work line signal.' )

!
!     Ensure ZZASCII signals an error for work line lengths
!     below the 3 character threshold.
!
      CALL ZZASCII ( FILES(NATIVE), XSHRTL, .FALSE., OUTTRM )
      CALL CHCKXC ( .TRUE., 'SPICE(STRINGTOOSHORT)', OK )

!
!     Clean-up. Delete files.
!
      DO I = 1, 3
         CALL KILFIL( FILES(I) )
      END DO

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )


      RETURN
      END
