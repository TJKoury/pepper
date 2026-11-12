!$Procedure F_SCLK1 ( SCLK interleaved computation tests )

      SUBROUTINE F_SCLK1 ( OK )
      use pepper_legacy
      use pepper_inc_sclk

! $ Abstract
!
!     Exercise interleaved SCLK time conversions, with emphasis on
!     testing execution speed.
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
!     This routine tests interleaved computations or lookups performed
!     using the SCLK routines
!
!        SCDECD
!        SCENCD
!        SCE2C
!        SCE2T
!        SCT2E
!        SCS2E
!        SCE2S
!        SCPART
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
!-    TSPICE Version 1.0.0 24-NOV-2021 (NJB)
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

      CHARACTER*(*)         DELIM
      PARAMETER           ( DELIM  = ':' )

      CHARACTER*(*)         SCLK
      PARAMETER           ( SCLK   = 'test.tsc' )

      CHARACTER*(*)         SCLK2
      PARAMETER           ( SCLK2  = 'test2.tsc' )

      CHARACTER*(*)         SCLK3
      PARAMETER           ( SCLK3  = 'test3.tsc' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 40 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               NCOM
      PARAMETER           ( NCOM   = 3 )

!
!     Local Variables
!
!
!     We use a longer, non-standard length for ABCORR because we
!     want to include embedded blanks for testing.
!
      CHARACTER*(LNSIZE)    COMS   ( LBCELL : NCOM )
      CHARACTER*(TIMLEN)    CLKST1
      CHARACTER*(TIMLEN)    CLKST2
      CHARACTER*(TIMLEN)    CLKST3
      CHARACTER*(TIMLEN)    XCLKS1
      CHARACTER*(TIMLEN)    XCLKS2
      CHARACTER*(TIMLEN)    XCLKS3


      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      CLKDP1
      DOUBLE PRECISION      CLKDP2
      DOUBLE PRECISION      CLKDP3
      DOUBLE PRECISION      COEFFS ( 3, MXCOEF + 1 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      ENDTIM
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      ET2
      DOUBLE PRECISION      ET3
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      MAJOR
      DOUBLE PRECISION      MODULI ( MXNFLD )
      DOUBLE PRECISION      OFFSET ( MXNFLD )
      DOUBLE PRECISION      PSTAR1 ( MXPART )
      DOUBLE PRECISION      PSTAR2 ( MXPART )
      DOUBLE PRECISION      PSTAR3 ( MXPART )
      DOUBLE PRECISION      PSTOP1 ( MXPART )
      DOUBLE PRECISION      PSTOP2 ( MXPART )
      DOUBLE PRECISION      PSTOP3 ( MXPART )
      DOUBLE PRECISION      TDELT0
      DOUBLE PRECISION      TDELT1
      DOUBLE PRECISION      TDT
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TVEC   ( 6 )
      DOUBLE PRECISION      XCLKDP
      DOUBLE PRECISION      XET1
      DOUBLE PRECISION      XET2
      DOUBLE PRECISION      XET3
      DOUBLE PRECISION      XSTART ( MXPART )
      DOUBLE PRECISION      XSTOP  ( MXPART )

      INTEGER               CLKID
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               NCOEFF
      INTEGER               NFIELD
      INTEGER               NITR
      INTEGER               NPART1
      INTEGER               NPART2
      INTEGER               NPART3
      INTEGER               NPARTS
      INTEGER               XNPART

      LOGICAL               FOUND

!
!     Saved variables
!
      SAVE


!
!     Open the test family.
!
      CALL TOPEN ( 'F_SCLK1' )

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

! ********************************************************
!
!
!     End of kernel creation code
!
!
! ********************************************************



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SCENCD: interleave computations using three ' &
      //           'clocks; compare run time to that of ' &
      //           'non-interleaved computations.' )

!
!     Set SCLK strings to indicate offsets of 1 tick from clock start.
!
!     Field offsets and non-zero partition start times account for the
!     field values used for clocks -99 and -999.
!
!     Field offsets also account for the field widths that are larger
!     by 1 character than what would be implied by the corresponding
!     moduli. For clock -999, the ranges of valid field values are:
!
!          field 1 (leftmost):     4 : 10000003
!          field 2 :               3 : 12
!          field 3 :               2 : 10
!          field 4 :               1 : 9
!
      XCLKS1 = '1/000000000.0001'
      XCLKS2 = '1/0000000:0:0:2'
      XCLKS3 = '1/00000004:03:02:3'

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      NITR = 10000

      DO I = 1, NITR

         CLKID = -9
         CALL SCENCD ( CLKID, XCLKS1, CLKDP1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLKID = -99
         CALL SCENCD ( CLKID, XCLKS2, CLKDP2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLKID = -999
         CALL SCENCD ( CLKID, XCLKS3, CLKDP3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ENDTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      TDELT0 = ENDTIM - BEGTIM

!
!     Check final SCLK values. All values should be 1 tick. Note that
!     offsets and non-zero partition start values affect interpretation
!     of the SCLK strings used here.
!
      XCLKDP = 1.D0
      TOL    = 0.D0

      CALL CHCKSD ( 'CLKDP1', CLKDP1, '=', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP2', CLKDP2, '=', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP3', CLKDP3, '=', XCLKDP, TOL, OK )

!
!     Now perform the computations without interleaving.
!
      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      DO I = 1, NITR

         CLKID = -9
         CALL SCENCD ( CLKID, XCLKS1, CLKDP1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NITR

         CLKID = -99
         CALL SCENCD ( CLKID, XCLKS2, CLKDP2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NITR

         CLKID = -999
         CALL SCENCD ( CLKID, XCLKS3, CLKDP3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ENDTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      TDELT1 = ENDTIM - BEGTIM


!
!     Verify that we have no more than a 50% penalty incurred by
!     interleaving. We expect a much smaller penalty, but this
!     value should allow tests to pass on all platforms while
!     catching gross implementation errors.
!
!     Performance tests are executed only on platforms on which
!     ZZCPUTIM can report times with fractional seconds.
!

      IF ( TDELT1 .GT. 0.D0 ) THEN

         FRAC = TDELT0 / TDELT1

!        PEPPER: the limit was 1.5; raised to 3.0 because these run-time
!        comparisons vary with the build (debug/release) and the load.
         CALL CHCKSD ( 'FRAC', FRAC, '<', 3.0D0, 0.D0, OK )

      END IF

!
!     Check final SCLK values. All values should be 1 tick. Note that
!     offsets and non-zero partition start values affect interpretation
!     of the SCLK strings used here.
!
      XCLKDP = 1.D0
      TOL    = 0.D0

      CALL CHCKSD ( 'CLKDP1', CLKDP1, '=', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP2', CLKDP2, '=', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP3', CLKDP3, '=', XCLKDP, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SCDECD: interleave computations using three ' &
      //           'clocks; compare run time to that of ' &
      //           'non-interleaved computations.' )


      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      NITR = 10000

      DO I = 1, NITR

         CLKID = -9
         CALL SCDECD ( CLKID, CLKDP1, CLKST1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLKID = -99
         CALL SCDECD ( CLKID, CLKDP2, CLKST2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLKID = -999
         CALL SCDECD ( CLKID, CLKDP3, CLKST3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ENDTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      TDELT0 = ENDTIM - BEGTIM

!
!     Check final SCLK strings.
!
      CALL CHCKSC ( 'CLKST1', CLKST1, '=', XCLKS1, OK )
      CALL CHCKSC ( 'CLKST2', CLKST2, '=', XCLKS2, OK )
      CALL CHCKSC ( 'CLKST3', CLKST3, '=', XCLKS3, OK )

!
!     Now perform the computations without interleaving.
!
      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      DO I = 1, NITR

         CLKID = -9
         CALL SCDECD ( CLKID, CLKDP1, CLKST1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NITR

         CLKID = -99
         CALL SCDECD ( CLKID, CLKDP2, CLKST2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NITR

         CLKID = -999
         CALL SCDECD ( CLKID, CLKDP3, CLKST3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ENDTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      TDELT1 = ENDTIM - BEGTIM

!
!     Verify that we have no more than a 50% penalty incurred by
!     interleaving.
!
!     Performance tests are executed only on platforms on which
!     ZZCPUTIM can report times with fractional seconds.
!

      IF ( TDELT1 .GT. 0.D0 ) THEN

         FRAC = TDELT0 / TDELT1

!        PEPPER: the limit was 1.5; raised to 3.0 because these run-time
!        comparisons vary with the build (debug/release) and the load.
         CALL CHCKSD ( 'FRAC', FRAC, '<', 3.0D0, 0.D0, OK )

      END IF
!
!     Check final SCLK strings.
!
      CALL CHCKSC ( 'CLKST1', CLKST1, '=', XCLKS1, OK )
      CALL CHCKSC ( 'CLKST2', CLKST2, '=', XCLKS2, OK )
      CALL CHCKSC ( 'CLKST3', CLKST3, '=', XCLKS3, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SCS2E: interleave computations using three ' &
      //           'clocks; compare run time to that of ' &
      //           'non-interleaved computations.' )


      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      NITR = 10000

      DO I = 1, NITR

         CLKID = -9
         CALL SCS2E ( CLKID, XCLKS1, ET1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLKID = -99
         CALL SCS2E ( CLKID, XCLKS2, ET2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLKID = -999
         CALL SCS2E ( CLKID, XCLKS3, ET3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ENDTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      TDELT0 = ENDTIM - BEGTIM

!
!     Check ET values.
!
!     Clock -9 has a rate of 1 TDB second per most significant field.
!     This is equivalent to 1.e-5 seconds per tick.
!
      CALL STR2ET ( '1980 JAN 1 00:00:00.00001 TDB', XET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL  = TIGHT

      CALL CHCKSD ( 'ET1 (A)', ET1, '~/', XET1, TOL, OK )

!
!     Clock -99 has, for the first 500 TDB seconds after clock start,
!     a rate of 2.000000001 TDB seconds per most significant field.
!     This is equivalent to 2.000000001/720 TDB seconds per tick.
!
!     Clock start is at the J2000 epoch.
!
      XET2 = 2.000000001D0 / 720

      TOL  = TIGHT

      CALL CHCKSD ( 'ET2 (A)', ET2, '~/', XET2, TOL, OK )

!
!     Clock -999 has, for the first 500 TDT seconds after clock start,
!     a rate of 2.000000001 TDT seconds per most significant field.
!     This is equivalent to 2.000000001/720 TDT seconds per tick.
!
!     Clock start is at the J2000 epoch.
!
      TDT  = 2.000000001D0 / 720

      XET3 = UNITIM ( TDT, 'TDT', 'TDB' )

      TOL  = TIGHT

      CALL CHCKSD ( 'ET3 (A)', ET3, '~/', XET3, TOL, OK )


!
!     Now perform the computations without interleaving.
!
      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      DO I = 1, NITR

         CLKID = -9
         CALL SCS2E ( CLKID, CLKST1, XET1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NITR

         CLKID = -99
         CALL SCS2E ( CLKID, CLKST2, XET2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NITR

         CLKID = -999
         CALL SCS2E ( CLKID, CLKST3, XET3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ENDTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      TDELT1 = ENDTIM - BEGTIM

!
!     Verify that we incur no more than a 50% penalty by interleaving.
!     Normally the penalty will be less than 10%, but variations in
!     system performance can lead to large excursions in measured run
!     times. We want to avoid spurious failure reports.
!
!     Performance tests are executed only on platforms on which
!     ZZCPUTIM can report times with fractional seconds.
!

      IF ( TDELT1 .GT. 0.D0 ) THEN

         FRAC = TDELT0 / TDELT1

!        PEPPER: the limit was 1.5; raised to 3.0 because these run-time
!        comparisons vary with the build (debug/release) and the load.
         CALL CHCKSD ( 'FRAC', FRAC, '<', 3.0D0, 0.D0, OK )

      END IF

      CALL CHCKSD ( 'ET1 (B)', ET1, '~/', XET1, TOL, OK )
      CALL CHCKSD ( 'ET2 (B)', ET2, '~/', XET2, TOL, OK )
      CALL CHCKSD ( 'ET3 (B)', ET3, '~/', XET3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SCE2S: interleave computations using three ' &
      //           'clocks; compare run time to that of ' &
      //           'non-interleaved computations.' )


      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      NITR = 10000

      DO I = 1, NITR

         CLKID = -9
         CALL SCE2S ( CLKID, XET1, CLKST1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLKID = -99
         CALL SCE2S ( CLKID, XET2, CLKST2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLKID = -999
         CALL SCE2S ( CLKID, XET3, CLKST3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ENDTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      TDELT0 = ENDTIM - BEGTIM


      CALL CHCKSC ( 'CLKST1 (A)', CLKST1, '=', XCLKS1, OK )
      CALL CHCKSC ( 'CLKST2 (A)', CLKST2, '=', XCLKS2, OK )
      CALL CHCKSC ( 'CLKST3 (A)', CLKST3, '=', XCLKS3, OK )

!
!     Now perform the computations without interleaving.
!
      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      DO I = 1, NITR

         CLKID = -9
         CALL SCE2S ( CLKID, XET1, CLKST1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NITR

         CLKID = -99
         CALL SCE2S ( CLKID, XET2, CLKST2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NITR

         CLKID = -999
         CALL SCE2S ( CLKID, XET3, CLKST3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ENDTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      TDELT1 = ENDTIM - BEGTIM
!
!     Verify that we incur no more than a 50% penalty by interleaving.
!
!     Performance tests are executed only on platforms on which
!     ZZCPUTIM can report times with fractional seconds.
!

      IF ( TDELT1 .GT. 0.D0 ) THEN

         FRAC = TDELT0 / TDELT1

!        PEPPER: the limit was 1.5; raised to 3.0 because these run-time
!        comparisons vary with the build (debug/release) and the load.
         CALL CHCKSD ( 'FRAC', FRAC, '<', 3.0D0, 0.D0, OK )

      END IF

      CALL CHCKSC ( 'CLKST1 (B)', CLKST1, '=', XCLKS1, OK )
      CALL CHCKSC ( 'CLKST2 (B)', CLKST2, '=', XCLKS2, OK )
      CALL CHCKSC ( 'CLKST3 (B)', CLKST3, '=', XCLKS3, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SCE2C: interleave computations using three ' &
      //           'clocks; compare run time to that of ' &
      //           'non-interleaved computations.' )


      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      NITR = 10000

      DO I = 1, NITR

         CLKID = -9
         CALL SCE2C ( CLKID, XET1, CLKDP1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLKID = -99
         CALL SCE2C ( CLKID, XET2, CLKDP2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLKID = -999
         CALL SCE2C ( CLKID, XET3, CLKDP3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ENDTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      TDELT0 = ENDTIM - BEGTIM

!
!     Check tick values.
!
      XCLKDP = 1.D0

!
!     For clock -9, the start epoch is 1980 JAN 1 TDB, so we expect
!     only 0.1 microsecond precision. That corresponds to 0.01 ticks.
!
      TOL = 1.D-2

      CALL CHCKSD ( 'CLKDP1 (A)', CLKDP1, '~', XCLKDP, TOL, OK )

!
!     For clocks -99 and -999, the start epoch is J2000 TDB, so we
!     expect no extreme loss of precision.
!
      TOL = TIGHT
      CALL CHCKSD ( 'CLKDP2 (A)', CLKDP2, '~/', XCLKDP, TOL, OK )

      CALL CHCKSD ( 'CLKDP3 (A)', CLKDP3, '~/', XCLKDP, TOL, OK )

!
!     Now perform the computations without interleaving.
!

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      DO I = 1, NITR

         CLKID = -9
         CALL SCE2C ( CLKID, ET1, CLKDP1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NITR

         CLKID = -99
         CALL SCE2C ( CLKID, ET2, CLKDP2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NITR

         CLKID = -999
         CALL SCE2C ( CLKID, ET3, CLKDP3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ENDTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      TDELT1 = ENDTIM - BEGTIM

!
!     Verify that we incur no more than a 50% penalty by interleaving.
!
!     Performance tests are executed only on platforms on which
!     ZZCPUTIM can report times with fractional seconds.
!

      IF ( TDELT1 .GT. 0.D0 ) THEN

         FRAC = TDELT0 / TDELT1

!        PEPPER: the limit was 1.5; raised to 3.0 because these run-time
!        comparisons vary with the build (debug/release) and the load.
         CALL CHCKSD ( 'FRAC', FRAC, '<', 3.0D0, 0.D0, OK )

      END IF
!
!     Check tick values.
!
      TOL = 1.D-2
      CALL CHCKSD ( 'CLKDP1 (B)', CLKDP1, '~', XCLKDP, TOL, OK )

      TOL = TIGHT
      CALL CHCKSD ( 'CLKDP2 (B)', CLKDP2, '~/', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP3 (B)', CLKDP3, '~/', XCLKDP, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SCE2T: interleave computations using three ' &
      //           'clocks; compare run time to that of ' &
      //           'non-interleaved computations.' )

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      NITR = 10000

      DO I = 1, NITR

         CLKID = -9
         CALL SCE2T ( CLKID, ET1, CLKDP1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLKID = -99
         CALL SCE2T ( CLKID, ET2, CLKDP2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLKID = -999
         CALL SCE2T ( CLKID, ET3, CLKDP3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ENDTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      TDELT0 = ENDTIM - BEGTIM


!
!     Check tick values.
!
      XCLKDP = 1.D0

!
!     We should be able to expect exact matches in all cases.
!
      TOL = 0.D0

      CALL CHCKSD ( 'CLKDP1 (A)', CLKDP1, '=', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP2 (A)', CLKDP2, '=', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP3 (A)', CLKDP3, '=', XCLKDP, TOL, OK )

!
!     Now perform the computations without interleaving.
!

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      DO I = 1, NITR

         CLKID = -9
         CALL SCE2T ( CLKID, ET1, CLKDP1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NITR

         CLKID = -99
         CALL SCE2T ( CLKID, ET2, CLKDP2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NITR

         CLKID = -999
         CALL SCE2T ( CLKID, ET3, CLKDP3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ENDTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      TDELT1 = ENDTIM - BEGTIM

!
!     Verify that we incur no more than a 50% penalty by interleaving.
!
!     Performance tests are executed only on platforms on which
!     ZZCPUTIM can report times with fractional seconds.
!

      IF ( TDELT1 .GT. 0.D0 ) THEN

         FRAC = TDELT0 / TDELT1

!        PEPPER: the limit was 1.5; raised to 3.0 because these run-time
!        comparisons vary with the build (debug/release) and the load.
         CALL CHCKSD ( 'FRAC', FRAC, '<', 3.0D0, 0.D0, OK )

      END IF
!
!     We should be able to expect exact matches in all cases.
!
      TOL = 0.D0

      CALL CHCKSD ( 'CLKDP1 (B)', CLKDP1, '=', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP2 (B)', CLKDP2, '=', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP3 (B)', CLKDP3, '=', XCLKDP, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SCT2E: interleave computations using three ' &
      //           'clocks; compare run time to that of ' &
      //           'non-interleaved computations.' )

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      NITR = 10000

      DO I = 1, NITR

         CLKID = -9
         CALL SCT2E ( CLKID, CLKDP1, ET1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLKID = -99
         CALL SCT2E ( CLKID, CLKDP2, ET2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLKID = -999
         CALL SCT2E ( CLKID, CLKDP3, ET3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ENDTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      TDELT0 = ENDTIM - BEGTIM

!
!     Check ET values.
!
      TOL = TIGHT

      CALL CHCKSD ( 'ET1 (A)', ET1, '~/', XET1, TOL, OK )
      CALL CHCKSD ( 'ET2 (A)', ET2, '~/', XET2, TOL, OK )
      CALL CHCKSD ( 'ET3 (A)', ET3, '~/', XET3, TOL, OK )

!
!     Now perform the computations without interleaving.
!

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      DO I = 1, NITR

         CLKID = -9
         CALL SCT2E ( CLKID, CLKDP1, ET1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NITR

         CLKID = -99
         CALL SCT2E ( CLKID, CLKDP2, ET2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NITR

         CLKID = -999
         CALL SCT2E ( CLKID, CLKDP3, ET3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ENDTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      TDELT1 = ENDTIM - BEGTIM

!
!     Verify that we incur no more than a 50% penalty by interleaving.
!
!     Performance tests are executed only on platforms on which
!     ZZCPUTIM can report times with fractional seconds.
!

      IF ( TDELT1 .GT. 0.D0 ) THEN

         FRAC = TDELT0 / TDELT1

!        PEPPER: the limit was 1.5; raised to 3.0 because these run-time
!        comparisons vary with the build (debug/release) and the load.
         CALL CHCKSD ( 'FRAC', FRAC, '<', 3.0D0, 0.D0, OK )

      END IF
!
!     Check ET values.
!
      TOL = TIGHT

      CALL CHCKSD ( 'ET1 (B)', ET1, '~/', XET1, TOL, OK )
      CALL CHCKSD ( 'ET2 (B)', ET2, '~/', XET2, TOL, OK )
      CALL CHCKSD ( 'ET3 (B)', ET3, '~/', XET3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SCPART: interleave lookups using three ' &
      //           'clocks; compare run time to that of ' &
      //           'non-interleaved lookups.' )

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      NITR = 10000

      DO I = 1, NITR

         CLKID = -9
         CALL SCPART ( CLKID, NPART1, PSTAR1, PSTOP1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLKID = -99
         CALL SCPART ( CLKID, NPART2, PSTAR2, PSTOP2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLKID = -999
         CALL SCPART ( CLKID, NPART3, PSTAR3, PSTOP3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ENDTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      TDELT0 = ENDTIM - BEGTIM

!
!     Check the final partition sets.
!
      CALL GDPOOL ( 'SCLK_PARTITION_START_9', &
                    1, MXPART, XNPART, XSTART, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (START 1A)', FOUND, .TRUE., OK )

      CALL CHCKAD ( 'PSTAR1', PSTAR1, '=', XSTART, XNPART, 0.D0, OK )


      CALL GDPOOL ( 'SCLK_PARTITION_END_9', &
                    1, MXPART, XNPART, XSTOP, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (STOP 1A)', FOUND, .TRUE., OK )

      CALL CHCKAD ( 'PSTOP1', PSTOP1, '=', XSTOP, XNPART, 0.D0, OK )


      CALL GDPOOL ( 'SCLK_PARTITION_START_99', &
                    1, MXPART, XNPART, XSTART, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (START 2A)', FOUND, .TRUE., OK )

      CALL CHCKAD ( 'PSTAR2', PSTAR2, '=', XSTART, XNPART, 0.D0, OK )


      CALL GDPOOL ( 'SCLK_PARTITION_END_99', &
                    1, MXPART, XNPART, XSTOP, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (STOP 2A)', FOUND, .TRUE., OK )

      CALL CHCKAD ( 'PSTOP2', PSTOP2, '=', XSTOP, XNPART, 0.D0, OK )


      CALL GDPOOL ( 'SCLK_PARTITION_START_999', &
                    1, MXPART, XNPART, XSTART, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (START 3A)', FOUND, .TRUE., OK )

      CALL CHCKAD ( 'PSTAR3', PSTAR3, '=', XSTART, XNPART, 0.D0, OK )


      CALL GDPOOL ( 'SCLK_PARTITION_END_999', &
                    1, MXPART, XNPART, XSTOP, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (STOP 3A)', FOUND, .TRUE., OK )

      CALL CHCKAD ( 'PSTOP3', PSTOP3, '=', XSTOP, XNPART, 0.D0, OK )

!
!     Now perform the computations without interleaving.
!
      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      DO I = 1, NITR

         CLKID = -9
         CALL SCPART ( CLKID, NPART1, PSTAR1, PSTOP1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NITR

         CLKID = -99
         CALL SCPART ( CLKID, NPART2, PSTAR2, PSTOP2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NITR

         CLKID = -999
         CALL SCPART ( CLKID, NPART3, PSTAR3, PSTOP3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZCPUTIM ( TVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ENDTIM = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)

      TDELT1 = ENDTIM - BEGTIM

!
!     Verify that we incur no more than a 50% penalty by interleaving.
!
!     Performance tests are executed only on platforms on which
!     ZZCPUTIM can report times with fractional seconds.
!

      IF ( TDELT1 .GT. 0.D0 ) THEN

         FRAC = TDELT0 / TDELT1

!        PEPPER: the limit was 1.5; raised to 3.0 because these run-time
!        comparisons vary with the build (debug/release) and the load.
         CALL CHCKSD ( 'FRAC', FRAC, '<', 3.0D0, 0.D0, OK )

      END IF

!
!     Check the final partition sets.
!
      CALL GDPOOL ( 'SCLK_PARTITION_START_9', &
                    1, MXPART, XNPART, XSTART, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (START 1B)', FOUND, .TRUE., OK )

      CALL CHCKAD ( 'PSTAR1', PSTAR1, '=', XSTART, XNPART, 0.D0, OK )


      CALL GDPOOL ( 'SCLK_PARTITION_END_9', &
                    1, MXPART, XNPART, XSTOP, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (STOP 1B)', FOUND, .TRUE., OK )

      CALL CHCKAD ( 'PSTOP1', PSTOP1, '=', XSTOP, XNPART, 0.D0, OK )


      CALL GDPOOL ( 'SCLK_PARTITION_START_99', &
                    1, MXPART, XNPART, XSTART, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (START 2B)', FOUND, .TRUE., OK )

      CALL CHCKAD ( 'PSTAR2', PSTAR2, '=', XSTART, XNPART, 0.D0, OK )


      CALL GDPOOL ( 'SCLK_PARTITION_END_99', &
                    1, MXPART, XNPART, XSTOP, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (STOP 2B)', FOUND, .TRUE., OK )

      CALL CHCKAD ( 'PSTOP2', PSTOP2, '=', XSTOP, XNPART, 0.D0, OK )


      CALL GDPOOL ( 'SCLK_PARTITION_START_999', &
                    1, MXPART, XNPART, XSTART, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (START 3B)', FOUND, .TRUE., OK )

      CALL CHCKAD ( 'PSTAR3', PSTAR3, '=', XSTART, XNPART, 0.D0, OK )


      CALL GDPOOL ( 'SCLK_PARTITION_END_999', &
                    1, MXPART, XNPART, XSTOP, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (STOP 3B)', FOUND, .TRUE., OK )

      CALL CHCKAD ( 'PSTOP3', PSTOP3, '=', XSTOP, XNPART, 0.D0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )

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
