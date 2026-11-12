!$Procedure F_SCLK ( SCLK tests )

      SUBROUTINE F_SCLK ( OK )
      use pepper_legacy
      use pepper_inc_sclk

! $ Abstract
!
!     Exercise the SCLK time conversion and kernel pool access routines.
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
!     trapped or unintentional and need reporting. The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SCLK routines
!
!        SC01
!        SCDECD
!        SCE2C
!        SCE2S
!        SCE2T
!        SCENCD
!        SCFMT
!        SCLU01
!        SCPARS
!        SCPART
!        SCPS01
!        SCS2E
!        SCT2E
!        SCTIKS
!        SCTRAN
!        SCTYPE
!
!     The entry points of SC01 and SCLU01 are exercised by tests of
!     the API-level routines.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 3.0.0 22-NOV-2021 (NJB)
!
!        Updated to test new SCLK database capabilities.
!        Added tests for SCTRAN and SCPS01.
!
!-    TSPICE Version 2.1.0 26-OCT-2010 (EDW)
!
!        All variables to SAVE for f2c'd version compiled with Borland.
!
!-    TSPICE Version 2.0.0, 03-MAR-2009 (NJB)
!
!        Added test cases for watcher-related error
!        handling. Routines that encounter failures
!        when looking up kernel data must not assume
!        they have the data, even if the watcher
!        system says no update is needed; formerly some
!        SCLK routines had this error.
!
!-    TSPICE Version 1.0.0, 21-FEB-2008 (NJB)
!
!-&

!
!     SPICELIB functions
!



!
!     Local Parameters
!
      CHARACTER*(*)         CK
      PARAMETER           ( CK     = 'test.bc' )

      CHARACTER*(*)         SCLK
      PARAMETER           ( SCLK   = 'test.tsc' )

      CHARACTER*(*)         SCLK2
      PARAMETER           ( SCLK2  = 'test2.tsc' )

      CHARACTER*(*)         SCLK3
      PARAMETER           ( SCLK3  = 'test3.tsc' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      MED
      PARAMETER           ( MED    = 1.D-9 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 41 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 40 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               NCOM
      PARAMETER           ( NCOM   = 3 )

      CHARACTER*(*)         DELIM
      PARAMETER           ( DELIM  = ':' )

      INTEGER               FLDWID
      PARAMETER           ( FLDWID = 25 )

!
!     Local Variables
!
      CHARACTER*(NAMLEN)    CLKNAM
      CHARACTER*(LNSIZE)    COMS   ( LBCELL : NCOM )
      CHARACTER*(NDELIM)    DLCHRS
      CHARACTER*(MSGLEN)    ERRMSG
      CHARACTER*(FLDWID)    FSTR   ( 4 )
      CHARACTER*(MSGLEN)    TITLE
      CHARACTER*(TIMLEN)    CLKSTR
      CHARACTER*(TIMLEN)    XCLKST

      DOUBLE PRECISION      CLKOFF
      DOUBLE PRECISION      COEFFS ( 3, MXCOEF )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DIVDND
      DOUBLE PRECISION      DIVSR ( 4 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ETOFF
      DOUBLE PRECISION      MAJOR
      DOUBLE PRECISION      MODULI ( MXNFLD )
      DOUBLE PRECISION      OFFSET ( MXNFLD )
      DOUBLE PRECISION      PSTART ( MXPART )
      DOUBLE PRECISION      PSTOP  ( MXPART )
      DOUBLE PRECISION      TDT
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      XCLKDP
      DOUBLE PRECISION      XET
      DOUBLE PRECISION      XSTART ( MXPART )
      DOUBLE PRECISION      XSTOP  ( MXPART )
      DOUBLE PRECISION      XTDT

      INTEGER               CLKID
      INTEGER               CLKTYP
      INTEGER               DELCDE
      INTEGER DELCDE_A1 ( 1 )
      EQUIVALENCE ( DELCDE, DELCDE_A1 )
      INTEGER               FIELD ( 4 )
      INTEGER               HANDLE
      INTEGER               I
      INTEGER I_A1 ( 1 )
      EQUIVALENCE ( I, I_A1 )
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NCOEFF
      INTEGER               NFIELD
      INTEGER               NPARTS
      INTEGER               PARTNO
      INTEGER               Q
      INTEGER               REM
      INTEGER               W

      LOGICAL               ERROR
      LOGICAL               FOUND

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                  DLCHRS / DELIMS /


!
!     Open the test family.
!
      CALL TOPEN ( 'F_SCLK' )



!*****************************************************************
!
!     Error cases
!
!*****************************************************************

!
!     Check handling of lookup failures. Routines covered below:
!
!
!        SCDECD
!        SCE2C
!        SCE2S
!        SCENCD
!        SCPART
!        SCS2E
!        SCTYPE
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check handling of lookup failure in SCPART.' )

!
!     We should see a lookup failure error on two successive calls
!     to SCPART if no data are available for a given clock.
!
      CLKID = -888

      CALL SCPART ( CLKID, N, PSTART, PSTOP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL SCPART ( CLKID, N, PSTART, PSTOP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check handling of lookup failure in SCTYPE.' )

!
!     We should see a lookup failure error on two successive calls
!     to SCTYPE if no data are available for a given clock.
!
      CLKID = -888

      CLKTYP =  SCTYPE ( CLKID )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CLKTYP =  SCTYPE ( CLKID )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

!
!     Suppress "unused variable" warnings.
!
      CLKTYP = TOUCHI( CLKTYP )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check handling of lookup failure in SCENCD.' )

!
!     We should see a lookup failure error on two successive calls
!     to SCENCD if no data are available for a given clock.
!
      CLKID  = -888
      CLKSTR = '1/987654321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check handling of lookup failure in SCDECD.' )

!
!     We should see a lookup failure error on two successive calls
!     to SCENCD if no data are available for a given clock.
!
      CLKID  = -888
      SCLKDP = 1000.D0

      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check handling of lookup failure in SCS2E.' )

!
!     We should see a lookup failure error on two successive calls
!     to SCS2E if no data are available for a given clock.
!
      CLKID  = -888
      SCLKDP = 1000.D0

      CLKSTR = '1/987654321.4321'

      CALL SCS2E  ( CLKID, CLKSTR, ET )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL SCS2E  ( CLKID, CLKSTR, ET )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check handling of lookup failure in SCE2S.' )

!
!     We should see a lookup failure error on two successive calls
!     to SCS if no data are available for a given clock.
!
      CLKID  = -888
      SCLKDP = 1000.D0

      CLKSTR = '1/987654321.4321'

      CALL SCE2S  ( CLKID, ET, CLKSTR )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL SCE2S  ( CLKID, ET, CLKSTR)
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check handling of lookup failure in SCE2C.' )

!
!     We should see a lookup failure error on two successive calls
!     to SCS if no data are available for a given clock.
!
      CLKID  = -888
      SCLKDP = 1000.D0

      CLKSTR = '1/987654321.4321'

      CALL SCE2C  ( CLKID, ET, SCLKDP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL SCE2C  ( CLKID, ET, SCLKDP)
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )








!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup:  create kernels.' )

      CALL KCLEAR

      IF ( EXISTS(SCLK) ) THEN
         CALL DELFIL( SCLK )
      END IF

      IF ( EXISTS(SCLK2) ) THEN
         CALL DELFIL( SCLK2 )
      END IF

      IF ( EXISTS(SCLK3) ) THEN
         CALL DELFIL( SCLK3 )
      END IF

!
!     Leapseconds:  Note that the LSK is deleted after loading, so we
!     don't have to clean it up later.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create and load an SCLK kernel. Keep the file; we'll be
!     re-loading it later. We'll delete it at the end of
!     the routine. W
!
      CALL TSTCK3 ( CK, SCLK, .FALSE., .TRUE., .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The following tests that use SCLK ID -9 presume the SCLK kernel
!     we just created contains the following assignments:
!
!        SCLK_KERNEL_ID                = ( @28-OCT-1994        )
!
!        SCLK_DATA_TYPE_9              = ( 1 )
!
!        SCLK01_TIME_SYSTEM_9          = ( 1 )
!        SCLK01_N_FIELDS_9             = ( 2 )
!        SCLK01_MODULI_9               = ( 1000000000     10000 )
!        SCLK01_OFFSETS_9              = ( 0         0 )
!        SCLK01_OUTPUT_DELIM_9         = ( 1 )
!
!        SCLK_PARTITION_START_9        = ( 0.0000000000000E+00 )
!        SCLK_PARTITION_END_9          = ( 1.00000000E+14      )
!        SCLK01_COEFFICIENTS_9         = ( 0.00000000E+00
!        @01-JAN-1980-00:00:00.000
!        1  )
!
!
!        DELTET/DELTA_T_A       =   32.184
!        DELTET/K               =    1.657D-3
!        DELTET/EB              =    1.671D-2
!        DELTET/M               = (  6.239996D0 1.99096871D-7 )
!
!        CK_-9999_SCLK          =   -9
!        CK_-9999_SPK           =   -9
!
!        CK_-10000_SCLK         =   -9
!        CK_-10000_SPK          =   -9
!
!        CK_-10001_SCLK         =   -9
!        CK_-10001_SPK          =   -9
!
!
!     We're going to create a second new kernel to exercise other
!     aspects of the SCLK system. The kernel contents will be as
!     follows:

!
!     KPL/SCLK
!
!     \begindata
!
!        SCLK_KERNEL_ID                  = ( @01-JAN-2000/12:00:00 )
!
!        SCLK_DATA_TYPE_-99              = ( 1 )
!
!        SCLK01_TIME_SYSTEM_-99          = ( 1 )
!        SCLK01_N_FIELDS_-99             = ( 4 )
!        SCLK01_MODULI_-99               = ( 10000000 10 9 8 )
!        SCLK01_OFFSETS_-99              = ( 0 0 0 0 )
!        SCLK01_OUTPUT_DELIM_-99         = ( 2 )
!
!        SCLK_PARTITION_START_-99        = ( 1.0,
!                                            2.0,
!                                            3.0,
!                                            ...
!                                            1001.0 )
!
!        SCLK_PARTITION_END_-99          = ( 720001.0,
!                                            720002.0,
!                                            ...
!                                            721001.0 )
!
!        SCLK01_COEFFICIENTS_-99         = (
!
!                0.0    0.0000000000000000E+00    0.2000000001000000E+01
!           180000.0    0.5000000000000000E+03    0.2000000002000000E+01
!           360000.0    0.1000000000000000E+04    0.2000000003000000E+01
!                                ...
!        720000000.0    0.2000000000000000E+07    0.2000004001000000E+01
!
!        )
!
!     \begintext
!
!     The above kernel has 1001 partitions and 4001 coefficient records.
!
      CALL TCASE ( 'Create second SCLK kernel with 1001 partitions.' )

      CALL SSIZEC ( 0, COMS )

      CLKID     = -99

      NPARTS    = 1001
      NCOEFF    = 4001

      NFIELD    = 4

      MODULI(1) = 10000000
      MODULI(2) =       10
      MODULI(3) =        9
      MODULI(4) =        8

      MAJOR = 1.D0

      DO I = 2, 4
         MAJOR = MAJOR * MODULI(I)
      END DO

      OFFSET(1) =        0
      OFFSET(2) =        0
      OFFSET(3) =        0
      OFFSET(4) =        0

      DO I = 1, NPARTS
         XSTART(I) = I
         XSTOP (I) = 720000 + I
      END DO

      DELTA = 1.D-9

      DO I = 1, NCOEFF

         COEFFS(1,I) = (I-1) * 180000.D0

         COEFFS(2,I) = COEFFS(1,I) * 2 / MAJOR

         COEFFS(3,I) = 2.D0  +  I*DELTA

      END DO

      CALL T_WCLK01 ( SCLK2, &
                      COMS, &
                      CLKID, &
                      '@01-JAN-2000/12:00:00', &
                      'TDB', &
                      NFIELD, &
                      MODULI, &
                      OFFSET, &
                      DELIM, &
                      NPARTS, &
                      XSTART, &
                      XSTOP, &
                      NCOEFF, &
                      COEFFS       )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We're going to create a third new kernel to exercise other
!     aspects of the SCLK system. This kernel uses TDT as the parallel
!     time system. The kernel contents will be as follows:

!
!     KPL/SCLK
!
!     \begindata
!
!        SCLK_KERNEL_ID                   = ( @01-JAN-2001/12:00:00 )
!
!        SCLK_DATA_TYPE_-999              = ( 1 )
!
!        SCLK01_TIME_SYSTEM_-999          = ( 2 )
!        SCLK01_N_FIELDS_-999             = ( 4 )
!        SCLK01_MODULI_-999               = ( 10000000 10 9 8 )
!        SCLK01_OFFSETS_-999              = ( 4 3 2 1 )
!        SCLK01_OUTPUT_DELIM_-999         = ( 2 )
!
!        SCLK_PARTITION_START_-999        = ( 1.0,
!                                             1000002.0,
!                                             3.0,
!                                             4.0  )
!
!        SCLK_PARTITION_END_-999          = ( 720001.0,
!                                             1720002.0,
!                                             720003.0,
!                                             720004.0 )
!
!        SCLK01_COEFFICIENTS_-999         = (
!
!
!                0.0    0.0000000000000000E+00    0.2000000001000000E+01
!           180000.0    0.5000000000000000E+03    0.2000000002000000E+01
!           360000.0    0.1000000000000000E+04    0.2000000003000000E+01
!                                ...
!          2700000.0    0.7500000000000000E+04    0.2000000016000000E+01
!
!        )
!
!     \begintext
!
!     The above kernel has 4 partitions and 16 coefficient records.
!
      CALL TCASE ( 'Create third SCLK kernel with 4 partitions. Time ' &
      //           'system is TDT.'                                   )

      CALL SSIZEC ( 0, COMS )

      CLKID     = -999

      NPARTS    = 4
      NCOEFF    = 16

      NFIELD    = 4

      MODULI(1) = 10000000
      MODULI(2) =       10
      MODULI(3) =        9
      MODULI(4) =        8

      MAJOR = 1.D0

      DO I = 2, 4
         MAJOR = MAJOR * MODULI(I)
      END DO

      OFFSET(1) =        4
      OFFSET(2) =        3
      OFFSET(3) =        2
      OFFSET(4) =        1

      DO I = 1, NPARTS
         XSTART(I) = I
         XSTOP (I) = 720000 + I
      END DO

!
!     Make the second partition start with a forward jump.
!
      XSTART(2) = XSTART(2) + 1.D6
      XSTOP (2) = XSTOP (2) + 1.D6

      DELTA = 1.D-9

      DO I = 1, NCOEFF

         COEFFS(1,I) = (I-1) * 180000.D0

         COEFFS(2,I) = COEFFS(1,I) * 2 / MAJOR

         COEFFS(3,I) = 2.D0  +  I*DELTA

      END DO

      CALL T_WCLK01 ( SCLK3, &
                      COMS, &
                      CLKID, &
                      '@01-JAN-2001/12:00:00', &
                      'TDT', &
                      NFIELD, &
                      MODULI, &
                      OFFSET, &
                      DELIM, &
                      NPARTS, &
                      XSTART, &
                      XSTOP, &
                      NCOEFF, &
                      COEFFS       )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LDPOOL ( SCLK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check clock type via SCTYPE' )

      CLKID = -9

      I     = SCTYPE ( CLKID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'Clock type', I, '=', 1, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Fetch partition boundaries via SCPART' )

      CLKID = -9

      CALL SCPART ( CLKID, NPARTS, PSTART, PSTOP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NPARTS', NPARTS, '=', 1, 0, OK )

!
!     These values come from the SCLK kernel described above.
!
      XSTART(1) = 0.D0
      XSTOP(1)  = 1.D14

      CALL CHCKAD ( 'PSTART', PSTART, '=', XSTART, NPARTS, 0.D0, OK )
      CALL CHCKAD ( 'PSTOP',  PSTOP,  '=', XSTOP,  NPARTS, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Encode SCLK strings via SCENCD' )

      CLKID  = -9
      CLKSTR = '1/987654321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Note that we're expecting exact agreement.
!
      XCLKDP = 9876543214321D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

!
!     Now repeat the test, this time excluding the partition
!     number.
!
      CLKSTR = '987654321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 9876543214321D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

!
!     Now repeat the test, this time excluding the partition
!     number and using a smaller count in the leftmost field.
!
      CLKSTR = '4321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 43214321D0
      CALL CHCKSD ( '3) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Encode SCLK strings via SCENCD using all ' &
      //           'supported delimiters.'                       )

      CLKID  = -9
      CLKSTR = '1/987654321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Note that we're expecting exact agreement.
!
      XCLKDP = 9876543214321D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )


      CLKSTR = '1/987654321:4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
      XCLKDP = 9876543214321D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )


      CLKSTR = '1/987654321-4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
      XCLKDP = 9876543214321D0
      CALL CHCKSD ( '3) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )


      CLKSTR = '1/987654321,4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
      XCLKDP = 9876543214321D0
      CALL CHCKSD ( '4) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )



      CLKSTR = '1/987654321 4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
      XCLKDP = 9876543214321D0
      CALL CHCKSD ( '5) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Encode SCLK strings via SCPARS' )

      CLKID  = -9
      CLKSTR = '1/987654321.4321'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( '1) ERROR', ERROR, .FALSE., OK )

      CALL CHCKSC ( '1) ERRMSG', ERRMSG, '=', ' ', OK )

!
!     Note that we're expecting exact agreement.
!
      XCLKDP = 9876543214321D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

!
!     Now repeat the test, this time excluding the partition
!     number.
!
      CLKSTR = '987654321.4321'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( '2) ERROR', ERROR, .FALSE., OK )

      CALL CHCKSC ( '2) ERRMSG', ERRMSG, '=', ' ', OK )

      XCLKDP = 9876543214321D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

!
!     Now repeat the test, this time excluding the partition
!     number and using a smaller count in the leftmost field.
!
      CLKSTR = '4321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 43214321D0
      CALL CHCKSD ( '3) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Encode SCLK strings via SCPARS, using ' &
      //           'partitions greater than 1.' )

      CLKID  = -99
      CLKSTR = '2/0000000:0:0:2'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( '1) ERROR', ERROR, .FALSE., OK )

      CALL CHCKSC ( '1) ERRMSG', ERRMSG, '=', ' ', OK )

!
!     Note that we're expecting exact agreement.
!
      XCLKDP = 720000.0D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

      CLKID  = -999
      CLKSTR = '3/0000005:3:2:4'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( '2) ERROR', ERROR, .FALSE., OK )

      CALL CHCKSC ( '2) ERRMSG', ERRMSG, '=', ' ', OK )

!
!     Note that we're expecting exact agreement.
!
      XCLKDP = 1440720.0D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Encode an invalid SCLK string via SCPARS' )

      CLKID  = -9
      CLKSTR = '1/987654321.XXXX'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', &
                    'Could not parse SCLK component XXXX from ' &
      //            '987654321.XXXX as a number.',              OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Encode SCLK duration strings via SCTIKS' )

      CLKID  = -9

      CLKSTR = '987654321.4321'

      CALL SCTIKS ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that we're expecting exact agreement.
!
      XCLKDP = 9876543214321D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

!
!     Now repeat the test, this time using a smaller count
!     in the leftmost field.
!
      CLKSTR = '4321.4321'

      CALL SCTIKS ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 43214321D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Decode SCLK tick values via SCDECD' )

      CLKID  = -9
      CLKSTR = '1/987654321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     We're expecting exact agreement.
!
      XCLKST = '1/987654321.4321'
      CALL CHCKSC ( '1) CLKSTR', CLKSTR, '=', XCLKST,  OK )

!
!     Now repeat the test, this time using smaller
!     tick value.
!
      CLKSTR = '4321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     We're expecting exact agreement.
!
      XCLKST = '1/000004321.4321'
      CALL CHCKSC ( '2) CLKSTR', CLKSTR, '=', XCLKST,  OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Decode SCLK tick values via SCDECD using each ' &
      //           'supported output delimiter'                    )

      CLKID  = -9

!
!     Fetch the current output delimiter code.
!
      CALL GIPOOL ( 'SCLK01_OUTPUT_DELIM_9', 1, 1, N, DELCDE_A1, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CLKSTR = '1/987654321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NDELIM

!
!        The delimiter array declared in sclk.inc has been
!        ordered so that the Ith character has code I.
!        Insert this code into the kernel pool.
!
         CALL PIPOOL ( 'SCLK01_OUTPUT_DELIM_9', 1, I_A1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Create the expected output string.
!
         XCLKST = '1/987654321#4321'

         CALL REPLCH ( XCLKST, '#', DLCHRS(I:I), XCLKST )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SCDECD ( CLKID, SCLKDP, CLKSTR )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        We're expecting exact agreement.
!
         TITLE = '#) CLKSTR'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, CLKSTR, '=', XCLKST,  OK )

      END DO

!
!     Restore the original output delimiter code.
!
      CALL PIPOOL ( 'SCLK01_OUTPUT_DELIM_9', 1, DELCDE_A1 )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Decode SCLK tick values via SCFMT' )

      CLKID  = -9

      CLKSTR = '987654321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCFMT  ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     We're expecting exact agreement.
!
      XCLKST = '987654321.4321'
      CALL CHCKSC ( 'CLKSTR', CLKSTR, '=', XCLKST,  OK )


!
!     Now repeat the test, this time using smaller
!     tick value.
!
      CLKSTR = '4321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SCFMT ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     We're expecting exact agreement.
!
      XCLKST = '000004321.4321'
      CALL CHCKSC ( '2) CLKSTR', CLKSTR, '=', XCLKST,  OK )





!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert ET values to integer ticks via SCE2T' )

      CLKID  = -9

      CALL STR2ET ( '1980 JAN 1 00:00:00 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     First try the ET value corresponding to clock start.
!
      CALL SCE2T  ( CLKID, ET0, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 0.D0
      CALL CHCKSD ( 'SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert ET values to continuous ticks via SCE2C' )

      CLKID  = -9

      CALL STR2ET ( '1980 JAN 1 00:00:00 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     First try the ET value corresponding to clock start.
!
      CALL SCE2C  ( CLKID, ET0, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 0.D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

!
!     Now a greater ET value.
!
      DELTA = 1.D8 + 0.3333D0

      ET    = ET0 + DELTA

      CALL SCE2C  ( CLKID, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      XCLKDP = DELTA * 1.D4

!
!     We should get some round-off here, so don't require
!     an exact match.
!
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '~/', XCLKDP, VTIGHT, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert continuous ticks to ET via SCT2E' )

      CLKID  = -9

      CALL STR2ET ( '1980 JAN 1 00:00:00 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Use an ET value that corresponds to a fractional tick count.
!
      DELTA = 1.D8 + 0.3333D0

      XET    = ET0 + DELTA

      CALL SCE2C  ( CLKID, XET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SCT2E  ( CLKID, SCLKDP, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We should get some round-off here, so don't require
!     an exact match.
!
      CALL CHCKSD ( '2) ET', ET, '~/', XET, VTIGHT, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert an SCLK string to ET via SCS2E' )

      CLKID  = -9
      CLKSTR = '1/987654321.4321'

!
!     Compute the expected value using SCENCD and
!     SCT2E.
!
      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCT2E  ( CLKID, SCLKDP, XET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now try the conversion with SCS2E.
!
      CALL SCS2E ( CLKID, CLKSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We're expecting exact agreement.
!
      CALL CHCKSD ( 'ET', ET, '=', XET, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert an ET value to an SCLK string via SCE2S.' )

!
!     Generate the ET value.
!
      CLKID  = -9
      CLKSTR = '1/987654321.4321'

      CALL SCS2E ( CLKID, CLKSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Compute the expected value using SCE2T and SCDECD.
!
      CALL SCE2T  ( CLKID, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCDECD ( CLKID, SCLKDP, XCLKST )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Now try the conversion with SCE2S.
!
      CALL SCE2S ( CLKID, ET, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We're expecting exact agreement.
!
      CALL CHCKSC ( 'CLKSTR', CLKSTR, '=', XCLKST, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clock -99: set expected values.' )

      CLKID     = -99

      NPARTS    = 1001
      NCOEFF    = 4001

      NFIELD    = 4

      MODULI(1) = 10000000
      MODULI(2) =       10
      MODULI(3) =        9
      MODULI(4) =        8

      MAJOR = 1.D0

      DO I = 2, 4
         MAJOR = MAJOR * MODULI(I)
      END DO

      OFFSET(1) =        0
      OFFSET(2) =        0
      OFFSET(3) =        0
      OFFSET(4) =        0

      DO I = 1, NPARTS
         XSTART(I) = I
         XSTOP (I) = 720000 + I
      END DO

      DELTA = 1.D-9

      DO I = 1, NCOEFF

         COEFFS(1,I) = (I-1) * 180000.D0

         COEFFS(2,I) = COEFFS(1,I) * 2 / MAJOR

         COEFFS(3,I) = 2.D0  +  I*DELTA

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clock -99: Recover partition bounds via SCPART' )

      CLKID = -99


      CALL SCPART ( CLKID, N, PSTART, PSTOP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', NPARTS, 0, OK )

      CALL CHCKAD ( 'PSTART', PSTART, '=', XSTART, NPARTS, 0.D0, OK )
      CALL CHCKAD ( 'PSTOP',  PSTOP,  '=', XSTOP,  NPARTS, 0.D0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clock -99: Convert tick coefficients to SCLK ' &
      //           'strings via SCDECD. Invert the conversion via ' &
      //           'SCENCD.'                                  )

      CLKID  = -99

      DO I = 1, NCOEFF

         CALL SCDECD  ( CLKID, COEFFS(1,I), CLKSTR )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Derive the expected string based on the SCLK kernel
!        structure: every record whose index is equivalent
!        to 1 mod 4 is at the start of a new partition. Each
!        record is 180000 ticks ahead of the previous one.
!
         CALL RMAINI ( I-1, 4, Q, REM )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        The partition number is simply Q+1.
!
         PARTNO = Q + 1

         CALL INTSTR ( PARTNO, XCLKST )

         CALL SUFFIX ( '/', 0, XCLKST )
!
!        Let J be the number of ticks past the
!        partition start reading. Let SCLKDP
!        be the actual clock reading; we obtain
!        SCLKDP by adding the partition start reading
!        to J.
!
         J      = NINT ( REM * 180000.D0 )
         SCLKDP = J + XSTART(PARTNO )
!
!        Convert SCLKDP to an SCLK string.
!
         DIVSR(1) = 720.D0
         DIVSR(2) =  72.D0
         DIVSR(3) =   8.D0

         DIVDND = SCLKDP

         DO K = 1, 3

            FIELD(K) = NINT(DIVDND) / NINT(DIVSR(K))

            CALL INTSTR ( FIELD(K), FSTR(K) )
            CALL LJUST  ( FSTR(K),  FSTR(K) )

            DIVDND   = DIVDND - FIELD(K)*DIVSR(K)

         END DO

         FIELD(4) = NINT(DIVDND)

         CALL INTSTR ( FIELD(4), FSTR(4) )

!
!        Pad the first field on the left with zeros.
!
         W = LASTNB( FSTR(1) )

         DO K = 1, 7-W
            CALL PREFIX ( '0', 0, FSTR(1) )
         END DO
!
!        Append the field and delimiters to the expected
!        string.
!
         DO K = 1, 3
            CALL SUFFIX ( FSTR(K), 0, XCLKST )
            CALL SUFFIX ( ':',     0, XCLKST )
         END DO

         CALL SUFFIX ( FSTR(4), 0, XCLKST )

         TITLE = 'CLKSTR(#)'
         CALL REPMI ( TITLE, '#', I, TITLE )

!
!        Look for an exact match.
!
         CALL CHCKSC ( TITLE, CLKSTR, '=', XCLKST, OK )

!
!        Now convert the string to a tick value.
!
         CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Compare the tick value to the tick coefficient.
!
         CALL CHCKSD ( 'SCLKDP', SCLKDP, '=', COEFFS(1,I), 0.D0, OK )

      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clock -99: Convert tick coefficients to ET ' &
      //           'via SCT2E' )

      CLKID  = -99

      DO I = 1, NCOEFF

         CALL SCT2E  ( CLKID, COEFFS(1,I), ET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XET = COEFFS(2,I)

         TITLE = 'ET(#)'
         CALL REPMI ( TITLE, '#', I, TITLE )

!
!        Don't look for an exact match, since NPARSD, which is
!        used by the text kernel reader routines, can introduce
!        round-off error in integral inputs (!) depending on
!        their format.
!
         CALL CHCKSD ( TITLE, ET, '~', XET, MED, OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clock -99: Convert tick coefficient ' &
      //           'midpoints to ET via SCT2E'  )

      CLKID  = -99

!
!     The tick offset value is constant, given the SCLK kernel
!     we've created.
!
      CLKOFF = ( COEFFS(1,1) + COEFFS(1,2) ) / 2

      DO I = 1, NCOEFF

         SCLKDP = COEFFS(1,I) + CLKOFF

         CALL SCT2E  ( CLKID, SCLKDP, ET )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        The ET offset is based on the rate for the previous
!        (Ith) coefficient record.
!
         XET = COEFFS(2,I) + (CLKOFF/MAJOR)*COEFFS(3,I)

         TITLE = 'ET(#)'
         CALL REPMI ( TITLE, '#', I, TITLE )

!
!        Look for tight, but not exact, match.
!
         CALL CHCKSD ( TITLE, ET, '~/', XET, VTIGHT, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clock -99: Convert TDB coefficients to ' &
      //           'ticks via SCE2T'                          )

      CLKID  = -99

      DO I = 1, NCOEFF

         CALL SCE2T  ( CLKID, COEFFS(2,I), SCLKDP )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XCLKDP = COEFFS(1,I)

         TITLE = 'SCLKDP(#)'
         CALL REPMI ( TITLE, '#', I, TITLE )

!
!        Look for an exact match.
!
         CALL CHCKSD ( TITLE, SCLKDP, '=', XCLKDP, 0.D0, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clock -99: Convert tick coefficient ' &
      //           'midpoints to ET via SCT2E'            )

      CLKID  = -99

!
!     The tick offset value is constant, given the SCLK kernel
!     we've created.
!
      CLKOFF = ( COEFFS(1,1) + COEFFS(1,2) ) / 2

      DO I = 1, NCOEFF

         SCLKDP = COEFFS(1,I) + CLKOFF

         CALL SCT2E  ( CLKID, SCLKDP, ET )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        The ET offset is based on the rate for the previous
!        (Ith) coefficient record.
!
         XET = COEFFS(2,I) + (CLKOFF/MAJOR)*COEFFS(3,I)

         TITLE = 'ET(#)'
         CALL REPMI ( TITLE, '#', I, TITLE )

!
!        Look for tight, but not exact, match.
!
         CALL CHCKSD ( TITLE, ET, '~/', XET, VTIGHT, OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clock -99: Convert ET coefficient midpoints to ' &
      //           'continuous ticks via SCE2C'                     )

      CLKID  = -99

!
!     The ET offset value is constant, given the SCLK kernel
!     we've created.
!
      ETOFF = ( COEFFS(2,1) + COEFFS(2,2) ) / 2

      DO I = 1, NCOEFF

         ET = COEFFS(2,I) + ETOFF

         CALL SCE2C  ( CLKID, ET, SCLKDP )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        The tick offset is based on the rate for the previous
!        (Ith) coefficient record.
!
!        The expected tick offset is the ET offset divided
!        by the applicable rate, which has units of seconds
!        per major field, times the ticks per major field.
!
         XCLKDP = COEFFS(1,I) + MAJOR*ETOFF/COEFFS(3,I)

         TITLE = 'SCLKDP (#)'
         CALL REPMI ( TITLE, '#', I, TITLE )

!
!        Look for tight, but not exact, match.
!
         CALL CHCKSD ( TITLE, SCLKDP, '~/', XCLKDP, VTIGHT, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Set expected values for clock -999:  ' )

      CLKID     = -999

      NPARTS    = 4
      NCOEFF    = 16

      NFIELD    = 4

      MODULI(1) = 10000000
      MODULI(2) =       10
      MODULI(3) =        9
      MODULI(4) =        8

      MAJOR = 1.D0

      DO I = 2, 4
         MAJOR = MAJOR * MODULI(I)
      END DO

      OFFSET(1) =        4
      OFFSET(2) =        3
      OFFSET(3) =        2
      OFFSET(4) =        1

      DO I = 1, NPARTS
         XSTART(I) = I
         XSTOP (I) = 720000 + I
      END DO

!
!     Make the second partition start with a forward jump.
!
      XSTART(2) = XSTART(2) + 1.D6
      XSTOP (2) = XSTOP (2) + 1.D6

      DELTA = 1.D-9

      DO I = 1, NCOEFF

         COEFFS(1,I) = (I-1) * 180000.D0

         COEFFS(2,I) = COEFFS(1,I) * 2 / MAJOR

         COEFFS(3,I) = 2.D0  +  I*DELTA

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clock -999: Encode SCLK strings via SCENCD.' )

      CLKID  = -999
      CLKSTR = '1/4:03:02:2'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Note that we're expecting exact agreement.
!
      XCLKDP = 0.D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

!
!     Now repeat the test, this time excluding the partition
!     number.
!
      CLKSTR = '4:03:02:2'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 0.D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

!
!     Use a larger input value.
!
      CLKSTR = '1/5:03:02:2'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 720.D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

!
!     Use an input value in the second partition.
!
      CLKSTR = '2/1393:11:10:3'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 720720.D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clock -999: Encode SCLK string with overflow ' &
      //           'field values via SCENCD.'                     )

      CLKID  = -999
      CLKSTR = '1/4:03:02:11'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Note that we're expecting exact agreement.
!
      XCLKDP = 9.D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )


      CLKSTR = '1/4:13:02:11'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Note that we're expecting exact agreement.
!
      XCLKDP = 729.D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

!
!     Overflow in the second and fourth fields:
!
      CLKSTR = '1/4:13:02:11'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Note that we're expecting exact agreement.
!
      XCLKDP = 729.D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

!
!     Overflow in the second, third and fourth fields:
!
      CLKSTR = '1/4:13:12:11'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Note that we're expecting exact agreement.
!
      XCLKDP = 809.D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clock -999: Decode SCLK tick values via SCDECD.' )

      CLKID  = -999
      XCLKST = '1/00000004:03:02:2'

      CALL SCENCD ( CLKID, XCLKST, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( '1) CLKSTR', CLKSTR, '=', XCLKST,  OK )


!
!     Use a larger input value.
!
      XCLKST = '1/00000005:03:02:2'

      CALL SCENCD ( CLKID, XCLKST, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( '2) CLKSTR', CLKSTR, '=', XCLKST,  OK )

!
!     Use an input value in the second partition.
!
      XCLKST = '2/00001393:11:10:3'

      CALL SCENCD ( CLKID, XCLKST, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( '3) CLKSTR', CLKSTR, '=', XCLKST,  OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clock -999: Convert TDT values to ' &
      //           'integer ticks via SCE2T'           )

      CLKID  = -999

      CALL STR2ET ( '2000 JAN 1 12:00:00 TDT', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     First try the TDT value corresponding to clock start.
!
      CALL SCE2T  ( CLKID, ET0, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 0.D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )


!
!     First now try a greater TDT value: 1000 seconds past clock
!     start.
!
      CALL STR2ET ( '2000 JAN 1 12:16:40 TDT', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCE2T  ( CLKID, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 3.6D5
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clock -999: Convert TDT values to ' &
      //           'continuous ticks via SCE2C'          )

      CLKID  = -999

      CALL STR2ET ( '2000 JAN 1 12:00:00.125 TDT', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     First try the TDT value corresponding to 1/8 second
!     past clock start.
!
      CALL SCE2C  ( CLKID, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = ( .125D0 / COEFFS(3,1) ) * 720.D0

      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '~', XCLKDP, VTIGHT, OK )

!
!     Now a greater TDT value.
!
      TDT = 1.D3 + 0.3333D0

      ET  = UNITIM ( TDT, 'TDT', 'TDB' )

      CALL SCE2C  ( CLKID, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      XCLKDP =      COEFFS(1,3) &
                + ( (TDT-COEFFS(2,3)) / COEFFS(3,3) ) * 720.D0

!
!     We should get some round-off here, so don't require
!     an exact match.
!
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '~/', XCLKDP, VTIGHT, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clock -999: Convert continuous ticks to ' &
      //           'TDT via SCT2E'                            )

      CLKID  = -999

      CALL STR2ET ( '2000 JAN 1 12:00:00 TDT', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Use a TDT value that corresponds to a fractional
!     tick count.
!
      DELTA = 1.D3 + 0.3333D0

      XTDT  = UNITIM ( ET0,  'TDB', 'TDT' ) + DELTA
      XET   = UNITIM ( XTDT, 'TDT', 'TDB' )

      CALL SCE2C  ( CLKID, XET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SCT2E  ( CLKID, SCLKDP, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TDT = UNITIM ( ET, 'TDB', 'TDT' )

!
!     We should get some round-off here, so don't require
!     an exact match.
!
      CALL CHCKSD ( '2) TDT', XTDT, '~/', XTDT, VTIGHT, OK )



!
!     Test entry points of SCTRAN.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SCN2ID: convert GLL clock name to ID' )

      CALL SCN2ID ( 'GLL SCLK', CLKID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CLKID', CLKID, '=', -77, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SCN2ID: convert unknown clock name to ID' )

      CLKID = 7
      CALL SCN2ID ( 'XYZ SCLK', CLKID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL CHCKSI ( 'CLKID', CLKID, '=', 7, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SCN2ID: convert recognized non-clock name to ID' )

      CALL SCN2ID ( 'JUPITER BARYCENTER SCLK', CLKID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,    OK )
      CALL CHCKSI ( 'CLKID', CLKID, '=', 5, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SCID2N: convert CASSINI clock ID to name' )

      CALL SCID2N ( -82, CLKNAM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
      CALL CHCKSC ( 'CLKNAM', CLKNAM, '=', 'CASSINI SCLK', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SCID2N: convert unknown clock ID to name' )

      CALL SCID2N ( 999999, CLKNAM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )

!      CALL BYEBYE (' ' )

      CALL DELFIL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SCLK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
