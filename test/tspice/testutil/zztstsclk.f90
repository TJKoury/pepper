!$Procedure ZZTSTSCLK (Create an SCLK file)

      SUBROUTINE ZZTSTSCLK( SCLKNM )

! $ Abstract
!
!     Create an S-clock kernel file.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!       TESTING
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         SCLKNM


! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      SCLKNM     I   The name of the S-clock kernel to create.
!
! $ Detailed_Input
!
!     SCLKNM      is the name of an S-clock Kernel to create.
!                 If an S-clock kernel of the same name exists,
!                 delete the existing kernel before creating
!                 this one.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!      None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     An SCLK kernel is created.
!
! $ Examples
!
!     Suppose that you are testing some portion of the toolkit
!     that requires the use of an SCLK kernel.  This routine
!     allows you to create an SCLK kernel without having to
!     know where a current SCLK kernel is located on the
!     file system.
!
!        CALL ZZTSTSCLK( SCLKNM )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!      None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!      E.D. Wright     (JPL)
!
! $ Version
!
!-    Test Utilities 1.0.0, 19-APR-2006 (EDW)
!
!        Reimplementation of TSTCK3 removing all code
!        except that needed to write the test SCLK.
!
!-&

! $ Index_Entries
!
!     Create test SCLK files.
!
!-&

!
!     Spicelib Functions
!
      INTEGER               RTRIM

!
!     Test Utility Functions
!
      CHARACTER*(16)        BEGDAT

!
!     Local Variables.
!

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NLINES
      PARAMETER           ( NLINES = 98 )

      CHARACTER*(LNSIZE)    SCLKBF ( NLINES )

      INTEGER               I
      INTEGER               UNIT
      INTEGER               R


!
!     The first order of business is to wipe out any existing
!     files with the same name.
!
      CALL KILFIL ( SCLKNM )

!
!     Fill up the buffer SCLKBF with the text that will make up
!     our test SCLK file.
!

      SCLKBF(  1) = 'TEST SPICE SCLK Kernel '
      SCLKBF(  2) = '------------------------------------------- ' &
      //            '------------- '
      SCLKBF(  3) = ' '
      SCLKBF(  4) = 'This file contains the data necessary for ' &
      //            'converting from '
      SCLKBF(  5) = 'ET to ticks for the fictional spacecraft ' &
      //            '-9999.  It is produced '
      SCLKBF(  6) = 'by the Test Utility routine ZZTSTSCLK. '
      SCLKBF(  7) = ' '
      SCLKBF(  8) = 'This SCLK kernel is intended to be used ' &
      //            'with the test CK file '
      SCLKBF(  9) = 'produced by that same routine.  The ' &
      //            'internal file name of the '
      SCLKBF( 10) = 'test C-Kernel is ZZ-TEST-CK-ZZ.  It ' &
      //            'contains a single type 03 '
      SCLKBF( 11) = 'segment. '
      SCLKBF( 12) = ' '
      SCLKBF( 13) = 'This fictional clock begins a 1 JAN 1980 ' &
      //            '00:00:00 ET and continues '
      SCLKBF( 14) = 'for 1 billion seconds.  (i.e. until 2011 ' &
      //            'SEP 9, 01:46:40.000 ET ). '
      SCLKBF( 15) = ' '
      SCLKBF( 16) = 'This is intended for test purposes only and ' &
      //            'can be easily rebuilt '
      SCLKBF( 17) = 'by calling the routine ZZTSTSCLK. '
      SCLKBF( 18) = ' '
      SCLKBF( 19) = 'If you have any questions about this file ' &
      //            'that these comments don''t '
      SCLKBF( 20) = 'answer, contact Bill Taber at NAIF. '
      SCLKBF( 21) = ' '
      SCLKBF( 22) = '(818) 354-4279 '
      SCLKBF( 23) = 'btaber@spice.jpl.nasa.gov '
      SCLKBF( 24) = ' '
      SCLKBF( 25) = ' '
      SCLKBF( 26) = ' '
      SCLKBF( 27) = 'Implementation notes '
      SCLKBF( 28) = '-------------------------------------------' &
      //            '------------- '
      SCLKBF( 29) = ' '
      SCLKBF( 30) = 'This SCLK file is constructed so that the ' &
      //            'valid SCLK strings '
      SCLKBF( 31) = 'are simply the number of TDB seconds that ' &
      //            'have passed '
      SCLKBF( 32) = 'since the Ephemeris epoch 1 Jan 1980 ' &
      //            '00:00:00 '
      SCLKBF( 33) = ' '
      SCLKBF( 34) = 'So that 1/ 288929292.82017  simply ' &
      //            'represents the epoch that occurs '
      SCLKBF( 35) = '288929292.82017 TDB seconds past the ET ' &
      //            'epoch 1 Jan 1980. '
      SCLKBF( 36) = ' '
      SCLKBF( 37) = 'For all time, the clock runs at the same ' &
      //            'rate as TDB. There is only '
      SCLKBF( 38) = 'one partition for this clock. '
      SCLKBF( 39) = ' '
      SCLKBF( 40) = 'You must load this file into the kernel ' &
      //            'pool before using any of the '
      SCLKBF( 41) = 'SPICELIB SCLK routines. The code fragment '
      SCLKBF( 42) = ' '
      SCLKBF( 43) = 'CALL FURNSH ( < name of this file > ) '
      SCLKBF( 44) = ' '
      SCLKBF( 45) = 'performs this task. To convert between ET ' &
      //            'and UTC, you will also need '
      SCLKBF( 46) = 'to load a leapseconds kernel. The ' &
      //            'additional call to FURNSH, '
      SCLKBF( 47) = ' '
      SCLKBF( 48) = 'CALL FURNSH ( < name of your leapsecond ' &
      //            'file > ) '
      SCLKBF( 49) = ' '
      SCLKBF( 50) = 'will accomplish this. Note that you must ' &
      //            'supply the actual names of '
      SCLKBF( 51) = 'the files used on your system as arguments ' &
      //            'to FURNSH. Because the file '
      SCLKBF( 52) = 'names are system dependent, we do not list ' &
      //            'them here. '
      SCLKBF( 53) = ' '
      SCLKBF( 54) = 'For more information, consult your SPICELIB ' &
      //            'required reading files. '
      SCLKBF( 55) = 'The following areas are covered: '
      SCLKBF( 56) = ' '
      SCLKBF( 57) = 'SCLK system                     SCLK ' &
      //            'required reading '
      SCLKBF( 58) = 'Time systems and conversion     TIME ' &
      //            'required reading '
      SCLKBF( 59) = 'Kernel pool                     KERNEL ' &
      //            'required reading '
      SCLKBF( 60) = ' '
      SCLKBF( 61) = ' '
      SCLKBF( 62) = 'Kernel data '
      SCLKBF( 63) = '-------------------------------------------' &
      //            '------------- '
      SCLKBF( 64) = ' '
      SCLKBF( 65) = ' '
      SCLKBF( 66) = BEGDAT()
      SCLKBF( 67) = ' '
      SCLKBF( 68) = ' '
      SCLKBF( 69) = 'SCLK_KERNEL_ID                = ( ' &
      //            '@28-OCT-1994        ) '
      SCLKBF( 70) = ' '
      SCLKBF( 71) = 'SCLK_DATA_TYPE_9              = ( ' &
      //            '1 ) '
      SCLKBF( 72) = ' '
      SCLKBF( 73) = 'SCLK01_TIME_SYSTEM_9          = ( ' &
      //            '1 ) '
      SCLKBF( 74) = 'SCLK01_N_FIELDS_9             = ( ' &
      //            '2 ) '
      SCLKBF( 75) = 'SCLK01_MODULI_9               = ( ' &
      //            '1000000000     10000 ) '
      SCLKBF( 76) = 'SCLK01_OFFSETS_9              = ( ' &
      //            '0         0 ) '
      SCLKBF( 77) = 'SCLK01_OUTPUT_DELIM_9         = ( ' &
      //            '1 ) '
      SCLKBF( 78) = ' '
      SCLKBF( 79) = 'SCLK_PARTITION_START_9        = ( ' &
      //            '0.0000000000000E+00 ) '
      SCLKBF( 80) = 'SCLK_PARTITION_END_9          = ( ' &
      //            '1.00000000E+14      ) '
      SCLKBF( 81) = 'SCLK01_COEFFICIENTS_9         = ( ' &
      //            '0.00000000E+00 '
      SCLKBF( 82) = '@01-JAN-1980-00:00:00.000 '
      SCLKBF( 83) = '1  ) '
      SCLKBF( 84) = ' '
      SCLKBF( 85) = ' '
      SCLKBF( 86) = 'DELTET/DELTA_T_A       =   32.184 '
      SCLKBF( 87) = 'DELTET/K               =    1.657D-3 '
      SCLKBF( 88) = 'DELTET/EB              =    1.671D-2 '
      SCLKBF( 89) = 'DELTET/M               = (  6.239996D0 ' &
      //            '1.99096871D-7 ) '
      SCLKBF( 90) = ' '
      SCLKBF( 91) = 'CK_-9999_SCLK          =   -9 '
      SCLKBF( 92) = 'CK_-9999_SPK           =   -9 '
      SCLKBF( 93) = ' '
      SCLKBF( 94) = 'CK_-10000_SCLK         =   -9 '
      SCLKBF( 95) = 'CK_-10000_SPK          =   -9 '
      SCLKBF( 96) = ' '
      SCLKBF( 97) = 'CK_-10001_SCLK         =   -9 '
      SCLKBF( 98) = 'CK_-10001_SPK          =   -9 '


!
!     Create the SCLK kernel.
!
      CALL TXTOPN ( SCLKNM, UNIT )

      DO I = 1, NLINES

         R =     RTRIM ( SCLKBF(I) )
         WRITE (UNIT,*)  SCLKBF(I)(1:R)

      END DO

      CLOSE ( UNIT )

      RETURN
      END
