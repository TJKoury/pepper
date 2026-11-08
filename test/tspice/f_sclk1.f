C$Procedure F_SCLK1 ( SCLK interleaved computation tests )
 
      SUBROUTINE F_SCLK1 ( OK )
 
C$ Abstract
C
C     Exercise interleaved SCLK time conversions, with emphasis on
C     testing execution speed.
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
 
      INCLUDE 'sclk.inc'
 
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
C     trapped or unintentional and need reporting. The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests interleaved computations or lookups performed
C     using the SCLK routines
C
C        SCDECD
C        SCENCD
C        SCE2C
C        SCE2T
C        SCT2E
C        SCS2E
C        SCE2S
C        SCPART
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0 24-NOV-2021 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      UNITIM
 
      LOGICAL               EXISTS
 
C
C     Local Parameters
C
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
 
C
C     Local Variables
C
C
C     We use a longer, non-standard length for ABCORR because we
C     want to include embedded blanks for testing.
C
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
 
C
C     Saved variables
C
      SAVE
 
 
C
C     Open the test family.
C
      CALL TOPEN ( 'F_SCLK1' )
 
C
C --- Case: ------------------------------------------------------
C
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
 
C
C     Leapseconds:  Note that the LSK is deleted after loading, so we
C     don't have to clean it up later.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Create and load an SCLK kernel. Keep the file; we'll be
C     re-loading it later. We'll delete it at the end of
C     the routine. W
C
      CALL TSTCK3 ( CK, SCLK, .FALSE., .TRUE., .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     The following tests that use SCLK ID -9 presume the SCLK kernel
C     we just created contains the following assignments:
C
C        SCLK_KERNEL_ID                = ( @28-OCT-1994        )
C
C        SCLK_DATA_TYPE_9              = ( 1 )
C
C        SCLK01_TIME_SYSTEM_9          = ( 1 )
C        SCLK01_N_FIELDS_9             = ( 2 )
C        SCLK01_MODULI_9               = ( 1000000000     10000 )
C        SCLK01_OFFSETS_9              = ( 0         0 )
C        SCLK01_OUTPUT_DELIM_9         = ( 1 )
C
C        SCLK_PARTITION_START_9        = ( 0.0000000000000E+00 )
C        SCLK_PARTITION_END_9          = ( 1.00000000E+14      )
C        SCLK01_COEFFICIENTS_9         = ( 0.00000000E+00
C        @01-JAN-1980-00:00:00.000
C        1  )
C
C
C        DELTET/DELTA_T_A       =   32.184
C        DELTET/K               =    1.657D-3
C        DELTET/EB              =    1.671D-2
C        DELTET/M               = (  6.239996D0 1.99096871D-7 )
C
C        CK_-9999_SCLK          =   -9
C        CK_-9999_SPK           =   -9
C
C        CK_-10000_SCLK         =   -9
C        CK_-10000_SPK          =   -9
C
C        CK_-10001_SCLK         =   -9
C        CK_-10001_SPK          =   -9
C
C
C     We're going to create a second new kernel to exercise other
C     aspects of the SCLK system. The kernel contents will be as
C     follows:
 
C
C     KPL/SCLK
C
C     \begindata
C
C        SCLK_KERNEL_ID                  = ( @01-JAN-2000/12:00:00 )
C
C        SCLK_DATA_TYPE_-99              = ( 1 )
C
C        SCLK01_TIME_SYSTEM_-99          = ( 1 )
C        SCLK01_N_FIELDS_-99             = ( 4 )
C        SCLK01_MODULI_-99               = ( 10000000 10 9 8 )
C        SCLK01_OFFSETS_-99              = ( 0 0 0 0 )
C        SCLK01_OUTPUT_DELIM_-99         = ( 2 )
C
C        SCLK_PARTITION_START_-99        = ( 1.0,
C                                            2.0,
C                                            3.0,
C                                            ...
C                                            1001.0 )
C
C        SCLK_PARTITION_END_-99          = ( 720001.0,
C                                            720002.0,
C                                            ...
C                                            721001.0 )
C
C        SCLK01_COEFFICIENTS_-99         = (
C
C                0.0    0.0000000000000000E+00    0.2000000001000000E+01
C           180000.0    0.5000000000000000E+03    0.2000000002000000E+01
C           360000.0    0.1000000000000000E+04    0.2000000003000000E+01
C                                ...
C        720000000.0    0.2000000000000000E+07    0.2000004001000000E+01
C
C        )
C
C     \begintext
C
C     The above kernel has 1001 partitions and 4001 coefficient records.
C
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
 
      CALL T_WCLK01 ( SCLK2,
     .                COMS,
     .                CLKID,
     .                '@01-JAN-2000/12:00:00',
     .                'TDB',
     .                NFIELD,
     .                MODULI,
     .                OFFSET,
     .                DELIM,
     .                NPARTS,
     .                XSTART,
     .                XSTOP,
     .                NCOEFF,
     .                COEFFS       )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     We're going to create a third new kernel to exercise other
C     aspects of the SCLK system. This kernel uses TDT as the parallel
C     time system. The kernel contents will be as follows:
 
C
C     KPL/SCLK
C
C     \begindata
C
C        SCLK_KERNEL_ID                   = ( @01-JAN-2001/12:00:00 )
C
C        SCLK_DATA_TYPE_-999              = ( 1 )
C
C        SCLK01_TIME_SYSTEM_-999          = ( 2 )
C        SCLK01_N_FIELDS_-999             = ( 4 )
C        SCLK01_MODULI_-999               = ( 10000000 10 9 8 )
C        SCLK01_OFFSETS_-999              = ( 4 3 2 1 )
C        SCLK01_OUTPUT_DELIM_-999         = ( 2 )
C
C        SCLK_PARTITION_START_-999        = ( 1.0,
C                                             1000002.0,
C                                             3.0,
C                                             4.0  )
C
C        SCLK_PARTITION_END_-999          = ( 720001.0,
C                                             1720002.0,
C                                             720003.0,
C                                             720004.0 )
C
C        SCLK01_COEFFICIENTS_-999         = (
C
C
C                0.0    0.0000000000000000E+00    0.2000000001000000E+01
C           180000.0    0.5000000000000000E+03    0.2000000002000000E+01
C           360000.0    0.1000000000000000E+04    0.2000000003000000E+01
C                                ...
C          2700000.0    0.7500000000000000E+04    0.2000000016000000E+01
C
C        )
C
C     \begintext
C
C     The above kernel has 4 partitions and 16 coefficient records.
C
      CALL TCASE ( 'Create third SCLK kernel with 4 partitions. Time '
     .//           'system is TDT.'                                   )
 
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
 
C
C     Make the second partition start with a forward jump.
C
      XSTART(2) = XSTART(2) + 1.D6
      XSTOP (2) = XSTOP (2) + 1.D6
 
      DELTA = 1.D-9
 
      DO I = 1, NCOEFF
 
         COEFFS(1,I) = (I-1) * 180000.D0
 
         COEFFS(2,I) = COEFFS(1,I) * 2 / MAJOR
 
         COEFFS(3,I) = 2.D0  +  I*DELTA
 
      END DO
 
      CALL T_WCLK01 ( SCLK3,
     .                COMS,
     .                CLKID,
     .                '@01-JAN-2001/12:00:00',
     .                'TDT',
     .                NFIELD,
     .                MODULI,
     .                OFFSET,
     .                DELIM,
     .                NPARTS,
     .                XSTART,
     .                XSTOP,
     .                NCOEFF,
     .                COEFFS       )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL LDPOOL ( SCLK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C ********************************************************
C
C
C     End of kernel creation code
C
C
C ********************************************************
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCENCD: interleave computations using three '
     .//           'clocks; compare run time to that of '
     .//           'non-interleaved computations.' )
 
C
C     Set SCLK strings to indicate offsets of 1 tick from clock start.
C
C     Field offsets and non-zero partition start times account for the
C     field values used for clocks -99 and -999.
C
C     Field offsets also account for the field widths that are larger
C     by 1 character than what would be implied by the corresponding
C     moduli. For clock -999, the ranges of valid field values are:
C
C          field 1 (leftmost):     4 : 10000003
C          field 2 :               3 : 12
C          field 3 :               2 : 10
C          field 4 :               1 : 9
C
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
 
C
C     Check final SCLK values. All values should be 1 tick. Note that
C     offsets and non-zero partition start values affect interpretation
C     of the SCLK strings used here.
C
      XCLKDP = 1.D0
      TOL    = 0.D0
 
      CALL CHCKSD ( 'CLKDP1', CLKDP1, '=', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP2', CLKDP2, '=', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP3', CLKDP3, '=', XCLKDP, TOL, OK )
 
C
C     Now perform the computations without interleaving.
C
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
 
 
C
C     Verify that we have no more than a 50% penalty incurred by
C     interleaving. We expect a much smaller penalty, but this
C     value should allow tests to pass on all platforms while
C     catching gross implementation errors.
C
C     Performance tests are executed only on platforms on which
C     ZZCPUTIM can report times with fractional seconds.
C
 
      IF ( TDELT1 .GT. 0.D0 ) THEN
 
         FRAC = TDELT0 / TDELT1
 
         CALL CHCKSD ( 'FRAC', FRAC, '<', 1.5D0, 0.D0, OK )
 
      END IF
 
C
C     Check final SCLK values. All values should be 1 tick. Note that
C     offsets and non-zero partition start values affect interpretation
C     of the SCLK strings used here.
C
      XCLKDP = 1.D0
      TOL    = 0.D0
 
      CALL CHCKSD ( 'CLKDP1', CLKDP1, '=', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP2', CLKDP2, '=', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP3', CLKDP3, '=', XCLKDP, TOL, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCDECD: interleave computations using three '
     .//           'clocks; compare run time to that of '
     .//           'non-interleaved computations.' )
 
 
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
 
C
C     Check final SCLK strings.
C
      CALL CHCKSC ( 'CLKST1', CLKST1, '=', XCLKS1, OK )
      CALL CHCKSC ( 'CLKST2', CLKST2, '=', XCLKS2, OK )
      CALL CHCKSC ( 'CLKST3', CLKST3, '=', XCLKS3, OK )
 
C
C     Now perform the computations without interleaving.
C
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
 
C
C     Verify that we have no more than a 50% penalty incurred by
C     interleaving.
C
C     Performance tests are executed only on platforms on which
C     ZZCPUTIM can report times with fractional seconds.
C
 
      IF ( TDELT1 .GT. 0.D0 ) THEN
 
         FRAC = TDELT0 / TDELT1
 
         CALL CHCKSD ( 'FRAC', FRAC, '<', 1.5D0, 0.D0, OK )
 
      END IF
C
C     Check final SCLK strings.
C
      CALL CHCKSC ( 'CLKST1', CLKST1, '=', XCLKS1, OK )
      CALL CHCKSC ( 'CLKST2', CLKST2, '=', XCLKS2, OK )
      CALL CHCKSC ( 'CLKST3', CLKST3, '=', XCLKS3, OK )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCS2E: interleave computations using three '
     .//           'clocks; compare run time to that of '
     .//           'non-interleaved computations.' )
 
 
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
 
C
C     Check ET values.
C
C     Clock -9 has a rate of 1 TDB second per most significant field.
C     This is equivalent to 1.e-5 seconds per tick.
C
      CALL STR2ET ( '1980 JAN 1 00:00:00.00001 TDB', XET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      TOL  = TIGHT
 
      CALL CHCKSD ( 'ET1 (A)', ET1, '~/', XET1, TOL, OK )
 
C
C     Clock -99 has, for the first 500 TDB seconds after clock start,
C     a rate of 2.000000001 TDB seconds per most significant field.
C     This is equivalent to 2.000000001/720 TDB seconds per tick.
C
C     Clock start is at the J2000 epoch.
C
      XET2 = 2.000000001D0 / 720
 
      TOL  = TIGHT
 
      CALL CHCKSD ( 'ET2 (A)', ET2, '~/', XET2, TOL, OK )
 
C
C     Clock -999 has, for the first 500 TDT seconds after clock start,
C     a rate of 2.000000001 TDT seconds per most significant field.
C     This is equivalent to 2.000000001/720 TDT seconds per tick.
C
C     Clock start is at the J2000 epoch.
C
      TDT  = 2.000000001D0 / 720
 
      XET3 = UNITIM ( TDT, 'TDT', 'TDB' )
 
      TOL  = TIGHT
 
      CALL CHCKSD ( 'ET3 (A)', ET3, '~/', XET3, TOL, OK )
 
 
C
C     Now perform the computations without interleaving.
C
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
 
C
C     Verify that we incur no more than a 50% penalty by interleaving.
C     Normally the penalty will be less than 10%, but variations in
C     system performance can lead to large excursions in measured run
C     times. We want to avoid spurious failure reports.
C
C     Performance tests are executed only on platforms on which
C     ZZCPUTIM can report times with fractional seconds.
C
 
      IF ( TDELT1 .GT. 0.D0 ) THEN
 
         FRAC = TDELT0 / TDELT1
 
         CALL CHCKSD ( 'FRAC', FRAC, '<', 1.5D0, 0.D0, OK )
 
      END IF
 
      CALL CHCKSD ( 'ET1 (B)', ET1, '~/', XET1, TOL, OK )
      CALL CHCKSD ( 'ET2 (B)', ET2, '~/', XET2, TOL, OK )
      CALL CHCKSD ( 'ET3 (B)', ET3, '~/', XET3, TOL, OK )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCE2S: interleave computations using three '
     .//           'clocks; compare run time to that of '
     .//           'non-interleaved computations.' )
 
 
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
 
C
C     Now perform the computations without interleaving.
C
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
C
C     Verify that we incur no more than a 50% penalty by interleaving.
C
C     Performance tests are executed only on platforms on which
C     ZZCPUTIM can report times with fractional seconds.
C
 
      IF ( TDELT1 .GT. 0.D0 ) THEN
 
         FRAC = TDELT0 / TDELT1
 
         CALL CHCKSD ( 'FRAC', FRAC, '<', 1.5D0, 0.D0, OK )
 
      END IF
 
      CALL CHCKSC ( 'CLKST1 (B)', CLKST1, '=', XCLKS1, OK )
      CALL CHCKSC ( 'CLKST2 (B)', CLKST2, '=', XCLKS2, OK )
      CALL CHCKSC ( 'CLKST3 (B)', CLKST3, '=', XCLKS3, OK )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCE2C: interleave computations using three '
     .//           'clocks; compare run time to that of '
     .//           'non-interleaved computations.' )
 
 
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
 
C
C     Check tick values.
C
      XCLKDP = 1.D0
 
C
C     For clock -9, the start epoch is 1980 JAN 1 TDB, so we expect
C     only 0.1 microsecond precision. That corresponds to 0.01 ticks.
C
      TOL = 1.D-2
 
      CALL CHCKSD ( 'CLKDP1 (A)', CLKDP1, '~', XCLKDP, TOL, OK )
 
C
C     For clocks -99 and -999, the start epoch is J2000 TDB, so we
C     expect no extreme loss of precision.
C
      TOL = TIGHT
      CALL CHCKSD ( 'CLKDP2 (A)', CLKDP2, '~/', XCLKDP, TOL, OK )
 
      CALL CHCKSD ( 'CLKDP3 (A)', CLKDP3, '~/', XCLKDP, TOL, OK )
 
C
C     Now perform the computations without interleaving.
C
 
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
 
C
C     Verify that we incur no more than a 50% penalty by interleaving.
C
C     Performance tests are executed only on platforms on which
C     ZZCPUTIM can report times with fractional seconds.
C
 
      IF ( TDELT1 .GT. 0.D0 ) THEN
 
         FRAC = TDELT0 / TDELT1
 
         CALL CHCKSD ( 'FRAC', FRAC, '<', 1.5D0, 0.D0, OK )
 
      END IF
C
C     Check tick values.
C
      TOL = 1.D-2
      CALL CHCKSD ( 'CLKDP1 (B)', CLKDP1, '~', XCLKDP, TOL, OK )
 
      TOL = TIGHT
      CALL CHCKSD ( 'CLKDP2 (B)', CLKDP2, '~/', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP3 (B)', CLKDP3, '~/', XCLKDP, TOL, OK )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCE2T: interleave computations using three '
     .//           'clocks; compare run time to that of '
     .//           'non-interleaved computations.' )
 
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
 
 
C
C     Check tick values.
C
      XCLKDP = 1.D0
 
C
C     We should be able to expect exact matches in all cases.
C
      TOL = 0.D0
 
      CALL CHCKSD ( 'CLKDP1 (A)', CLKDP1, '=', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP2 (A)', CLKDP2, '=', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP3 (A)', CLKDP3, '=', XCLKDP, TOL, OK )
 
C
C     Now perform the computations without interleaving.
C
 
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
 
C
C     Verify that we incur no more than a 50% penalty by interleaving.
C
C     Performance tests are executed only on platforms on which
C     ZZCPUTIM can report times with fractional seconds.
C
 
      IF ( TDELT1 .GT. 0.D0 ) THEN
 
         FRAC = TDELT0 / TDELT1
 
         CALL CHCKSD ( 'FRAC', FRAC, '<', 1.5D0, 0.D0, OK )
 
      END IF
C
C     We should be able to expect exact matches in all cases.
C
      TOL = 0.D0
 
      CALL CHCKSD ( 'CLKDP1 (B)', CLKDP1, '=', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP2 (B)', CLKDP2, '=', XCLKDP, TOL, OK )
      CALL CHCKSD ( 'CLKDP3 (B)', CLKDP3, '=', XCLKDP, TOL, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCT2E: interleave computations using three '
     .//           'clocks; compare run time to that of '
     .//           'non-interleaved computations.' )
 
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
 
C
C     Check ET values.
C
      TOL = TIGHT
 
      CALL CHCKSD ( 'ET1 (A)', ET1, '~/', XET1, TOL, OK )
      CALL CHCKSD ( 'ET2 (A)', ET2, '~/', XET2, TOL, OK )
      CALL CHCKSD ( 'ET3 (A)', ET3, '~/', XET3, TOL, OK )
 
C
C     Now perform the computations without interleaving.
C
 
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
 
C
C     Verify that we incur no more than a 50% penalty by interleaving.
C
C     Performance tests are executed only on platforms on which
C     ZZCPUTIM can report times with fractional seconds.
C
 
      IF ( TDELT1 .GT. 0.D0 ) THEN
 
         FRAC = TDELT0 / TDELT1
 
         CALL CHCKSD ( 'FRAC', FRAC, '<', 1.5D0, 0.D0, OK )
 
      END IF
C
C     Check ET values.
C
      TOL = TIGHT
 
      CALL CHCKSD ( 'ET1 (B)', ET1, '~/', XET1, TOL, OK )
      CALL CHCKSD ( 'ET2 (B)', ET2, '~/', XET2, TOL, OK )
      CALL CHCKSD ( 'ET3 (B)', ET3, '~/', XET3, TOL, OK )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPART: interleave lookups using three '
     .//           'clocks; compare run time to that of '
     .//           'non-interleaved lookups.' )
 
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
 
C
C     Check the final partition sets.
C
      CALL GDPOOL ( 'SCLK_PARTITION_START_9',
     .              1, MXPART, XNPART, XSTART, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND (START 1A)', FOUND, .TRUE., OK )
 
      CALL CHCKAD ( 'PSTAR1', PSTAR1, '=', XSTART, XNPART, 0.D0, OK )
 
 
      CALL GDPOOL ( 'SCLK_PARTITION_END_9',
     .              1, MXPART, XNPART, XSTOP, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND (STOP 1A)', FOUND, .TRUE., OK )
 
      CALL CHCKAD ( 'PSTOP1', PSTOP1, '=', XSTOP, XNPART, 0.D0, OK )
 
 
      CALL GDPOOL ( 'SCLK_PARTITION_START_99',
     .              1, MXPART, XNPART, XSTART, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND (START 2A)', FOUND, .TRUE., OK )
 
      CALL CHCKAD ( 'PSTAR2', PSTAR2, '=', XSTART, XNPART, 0.D0, OK )
 
 
      CALL GDPOOL ( 'SCLK_PARTITION_END_99',
     .              1, MXPART, XNPART, XSTOP, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND (STOP 2A)', FOUND, .TRUE., OK )
 
      CALL CHCKAD ( 'PSTOP2', PSTOP2, '=', XSTOP, XNPART, 0.D0, OK )
 
 
      CALL GDPOOL ( 'SCLK_PARTITION_START_999',
     .              1, MXPART, XNPART, XSTART, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND (START 3A)', FOUND, .TRUE., OK )
 
      CALL CHCKAD ( 'PSTAR3', PSTAR3, '=', XSTART, XNPART, 0.D0, OK )
 
 
      CALL GDPOOL ( 'SCLK_PARTITION_END_999',
     .              1, MXPART, XNPART, XSTOP, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND (STOP 3A)', FOUND, .TRUE., OK )
 
      CALL CHCKAD ( 'PSTOP3', PSTOP3, '=', XSTOP, XNPART, 0.D0, OK )
 
C
C     Now perform the computations without interleaving.
C
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
 
C
C     Verify that we incur no more than a 50% penalty by interleaving.
C
C     Performance tests are executed only on platforms on which
C     ZZCPUTIM can report times with fractional seconds.
C
 
      IF ( TDELT1 .GT. 0.D0 ) THEN
 
         FRAC = TDELT0 / TDELT1
 
         CALL CHCKSD ( 'FRAC', FRAC, '<', 1.5D0, 0.D0, OK )
 
      END IF
 
C
C     Check the final partition sets.
C
      CALL GDPOOL ( 'SCLK_PARTITION_START_9',
     .              1, MXPART, XNPART, XSTART, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND (START 1B)', FOUND, .TRUE., OK )
 
      CALL CHCKAD ( 'PSTAR1', PSTAR1, '=', XSTART, XNPART, 0.D0, OK )
 
 
      CALL GDPOOL ( 'SCLK_PARTITION_END_9',
     .              1, MXPART, XNPART, XSTOP, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND (STOP 1B)', FOUND, .TRUE., OK )
 
      CALL CHCKAD ( 'PSTOP1', PSTOP1, '=', XSTOP, XNPART, 0.D0, OK )
 
 
      CALL GDPOOL ( 'SCLK_PARTITION_START_99',
     .              1, MXPART, XNPART, XSTART, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND (START 2B)', FOUND, .TRUE., OK )
 
      CALL CHCKAD ( 'PSTAR2', PSTAR2, '=', XSTART, XNPART, 0.D0, OK )
 
 
      CALL GDPOOL ( 'SCLK_PARTITION_END_99',
     .              1, MXPART, XNPART, XSTOP, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND (STOP 2B)', FOUND, .TRUE., OK )
 
      CALL CHCKAD ( 'PSTOP2', PSTOP2, '=', XSTOP, XNPART, 0.D0, OK )
 
 
      CALL GDPOOL ( 'SCLK_PARTITION_START_999',
     .              1, MXPART, XNPART, XSTART, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND (START 3B)', FOUND, .TRUE., OK )
 
      CALL CHCKAD ( 'PSTAR3', PSTAR3, '=', XSTART, XNPART, 0.D0, OK )
 
 
      CALL GDPOOL ( 'SCLK_PARTITION_END_999',
     .              1, MXPART, XNPART, XSTOP, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND (STOP 3B)', FOUND, .TRUE., OK )
 
      CALL CHCKAD ( 'PSTOP3', PSTOP3, '=', XSTOP, XNPART, 0.D0, OK )
 
 
 
C
C --- Case: ------------------------------------------------------
C
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
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END
