C$Procedure F_SCLK ( SCLK tests )

      SUBROUTINE F_SCLK ( OK )

C$ Abstract
C
C     Exercise the SCLK time conversion and kernel pool access routines.
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
C     This routine tests the SCLK routines
C
C        SC01
C        SCDECD
C        SCE2C
C        SCE2S
C        SCE2T
C        SCENCD
C        SCFMT
C        SCLU01
C        SCPARS
C        SCPART
C        SCPS01
C        SCS2E
C        SCT2E
C        SCTIKS
C        SCTRAN
C        SCTYPE
C
C     The entry points of SC01 and SCLU01 are exercised by tests of
C     the API-level routines.
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
C-    TSPICE Version 3.0.0 22-NOV-2021 (NJB)
C
C        Updated to test new SCLK database capabilities.
C        Added tests for SCTRAN and SCPS01.
C
C-    TSPICE Version 2.1.0 26-OCT-2010 (EDW)
C
C        All variables to SAVE for f2c'd version compiled with Borland.
C
C-    TSPICE Version 2.0.0, 03-MAR-2009 (NJB)
C
C        Added test cases for watcher-related error
C        handling. Routines that encounter failures
C        when looking up kernel data must not assume
C        they have the data, even if the watcher
C        system says no update is needed; formerly some
C        SCLK routines had this error.
C
C-    TSPICE Version 1.0.0, 21-FEB-2008 (NJB)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      UNITIM

      INTEGER               LASTNB
      INTEGER               SCTYPE
      INTEGER               TOUCHI

      LOGICAL               EXISTS

C
C     Local Parameters
C
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

C
C     Local Variables
C
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
      INTEGER               FIELD ( 4 )
      INTEGER               HANDLE
      INTEGER               I
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

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
      DATA                  DLCHRS / DELIMS /


C
C     Open the test family.
C
      CALL TOPEN ( 'F_SCLK' )



C*****************************************************************
C
C     Error cases
C
C*****************************************************************

C
C     Check handling of lookup failures. Routines covered below:
C
C
C        SCDECD
C        SCE2C
C        SCE2S
C        SCENCD
C        SCPART
C        SCS2E
C        SCTYPE
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check handling of lookup failure in SCPART.' )

C
C     We should see a lookup failure error on two successive calls
C     to SCPART if no data are available for a given clock.
C
      CLKID = -888

      CALL SCPART ( CLKID, N, PSTART, PSTOP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL SCPART ( CLKID, N, PSTART, PSTOP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check handling of lookup failure in SCTYPE.' )

C
C     We should see a lookup failure error on two successive calls
C     to SCTYPE if no data are available for a given clock.
C
      CLKID = -888

      CLKTYP =  SCTYPE ( CLKID )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CLKTYP =  SCTYPE ( CLKID )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Suppress "unused variable" warnings.
C
      CLKTYP = TOUCHI( CLKTYP )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check handling of lookup failure in SCENCD.' )

C
C     We should see a lookup failure error on two successive calls
C     to SCENCD if no data are available for a given clock.
C
      CLKID  = -888
      CLKSTR = '1/987654321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check handling of lookup failure in SCDECD.' )

C
C     We should see a lookup failure error on two successive calls
C     to SCENCD if no data are available for a given clock.
C
      CLKID  = -888
      SCLKDP = 1000.D0

      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check handling of lookup failure in SCS2E.' )

C
C     We should see a lookup failure error on two successive calls
C     to SCS2E if no data are available for a given clock.
C
      CLKID  = -888
      SCLKDP = 1000.D0

      CLKSTR = '1/987654321.4321'

      CALL SCS2E  ( CLKID, CLKSTR, ET )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL SCS2E  ( CLKID, CLKSTR, ET )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check handling of lookup failure in SCE2S.' )

C
C     We should see a lookup failure error on two successive calls
C     to SCS if no data are available for a given clock.
C
      CLKID  = -888
      SCLKDP = 1000.D0

      CLKSTR = '1/987654321.4321'

      CALL SCE2S  ( CLKID, ET, CLKSTR )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL SCE2S  ( CLKID, ET, CLKSTR)
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check handling of lookup failure in SCE2C.' )

C
C     We should see a lookup failure error on two successive calls
C     to SCS if no data are available for a given clock.
C
      CLKID  = -888
      SCLKDP = 1000.D0

      CLKSTR = '1/987654321.4321'

      CALL SCE2C  ( CLKID, ET, SCLKDP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL SCE2C  ( CLKID, ET, SCLKDP)
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )








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



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check clock type via SCTYPE' )

      CLKID = -9

      I     = SCTYPE ( CLKID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'Clock type', I, '=', 1, 0, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Fetch partition boundaries via SCPART' )

      CLKID = -9

      CALL SCPART ( CLKID, NPARTS, PSTART, PSTOP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NPARTS', NPARTS, '=', 1, 0, OK )

C
C     These values come from the SCLK kernel described above.
C
      XSTART(1) = 0.D0
      XSTOP(1)  = 1.D14

      CALL CHCKAD ( 'PSTART', PSTART, '=', XSTART, NPARTS, 0.D0, OK )
      CALL CHCKAD ( 'PSTOP',  PSTOP,  '=', XSTOP,  NPARTS, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Encode SCLK strings via SCENCD' )

      CLKID  = -9
      CLKSTR = '1/987654321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Note that we're expecting exact agreement.
C
      XCLKDP = 9876543214321D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

C
C     Now repeat the test, this time excluding the partition
C     number.
C
      CLKSTR = '987654321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 9876543214321D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

C
C     Now repeat the test, this time excluding the partition
C     number and using a smaller count in the leftmost field.
C
      CLKSTR = '4321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 43214321D0
      CALL CHCKSD ( '3) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Encode SCLK strings via SCENCD using all '
     .//           'supported delimiters.'                       )

      CLKID  = -9
      CLKSTR = '1/987654321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Note that we're expecting exact agreement.
C
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



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Encode SCLK strings via SCPARS' )

      CLKID  = -9
      CLKSTR = '1/987654321.4321'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( '1) ERROR', ERROR, .FALSE., OK )

      CALL CHCKSC ( '1) ERRMSG', ERRMSG, '=', ' ', OK )

C
C     Note that we're expecting exact agreement.
C
      XCLKDP = 9876543214321D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

C
C     Now repeat the test, this time excluding the partition
C     number.
C
      CLKSTR = '987654321.4321'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( '2) ERROR', ERROR, .FALSE., OK )

      CALL CHCKSC ( '2) ERRMSG', ERRMSG, '=', ' ', OK )

      XCLKDP = 9876543214321D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

C
C     Now repeat the test, this time excluding the partition
C     number and using a smaller count in the leftmost field.
C
      CLKSTR = '4321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 43214321D0
      CALL CHCKSD ( '3) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Encode SCLK strings via SCPARS, using '
     .//           'partitions greater than 1.' )

      CLKID  = -99
      CLKSTR = '2/0000000:0:0:2'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( '1) ERROR', ERROR, .FALSE., OK )

      CALL CHCKSC ( '1) ERRMSG', ERRMSG, '=', ' ', OK )

C
C     Note that we're expecting exact agreement.
C
      XCLKDP = 720000.0D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

      CLKID  = -999
      CLKSTR = '3/0000005:3:2:4'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( '2) ERROR', ERROR, .FALSE., OK )

      CALL CHCKSC ( '2) ERRMSG', ERRMSG, '=', ' ', OK )

C
C     Note that we're expecting exact agreement.
C
      XCLKDP = 1440720.0D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Encode an invalid SCLK string via SCPARS' )

      CLKID  = -9
      CLKSTR = '1/987654321.XXXX'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=',
     .              'Could not parse SCLK component XXXX from '
     .//            '987654321.XXXX as a number.',              OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Encode SCLK duration strings via SCTIKS' )

      CLKID  = -9

      CLKSTR = '987654321.4321'

      CALL SCTIKS ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note that we're expecting exact agreement.
C
      XCLKDP = 9876543214321D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

C
C     Now repeat the test, this time using a smaller count
C     in the leftmost field.
C
      CLKSTR = '4321.4321'

      CALL SCTIKS ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 43214321D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Decode SCLK tick values via SCDECD' )

      CLKID  = -9
      CLKSTR = '1/987654321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     We're expecting exact agreement.
C
      XCLKST = '1/987654321.4321'
      CALL CHCKSC ( '1) CLKSTR', CLKSTR, '=', XCLKST,  OK )

C
C     Now repeat the test, this time using smaller
C     tick value.
C
      CLKSTR = '4321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     We're expecting exact agreement.
C
      XCLKST = '1/000004321.4321'
      CALL CHCKSC ( '2) CLKSTR', CLKSTR, '=', XCLKST,  OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Decode SCLK tick values via SCDECD using each '
     .//           'supported output delimiter'                    )

      CLKID  = -9

C
C     Fetch the current output delimiter code.
C
      CALL GIPOOL ( 'SCLK01_OUTPUT_DELIM_9', 1, 1, N, DELCDE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CLKSTR = '1/987654321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NDELIM

C
C        The delimiter array declared in sclk.inc has been
C        ordered so that the Ith character has code I.
C        Insert this code into the kernel pool.
C
         CALL PIPOOL ( 'SCLK01_OUTPUT_DELIM_9', 1, I )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Create the expected output string.
C
         XCLKST = '1/987654321#4321'

         CALL REPLCH ( XCLKST, '#', DLCHRS(I:I), XCLKST )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SCDECD ( CLKID, SCLKDP, CLKSTR )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        We're expecting exact agreement.
C
         TITLE = '#) CLKSTR'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSC ( TITLE, CLKSTR, '=', XCLKST,  OK )

      END DO

C
C     Restore the original output delimiter code.
C
      CALL PIPOOL ( 'SCLK01_OUTPUT_DELIM_9', 1, DELCDE )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Decode SCLK tick values via SCFMT' )

      CLKID  = -9

      CLKSTR = '987654321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCFMT  ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     We're expecting exact agreement.
C
      XCLKST = '987654321.4321'
      CALL CHCKSC ( 'CLKSTR', CLKSTR, '=', XCLKST,  OK )


C
C     Now repeat the test, this time using smaller
C     tick value.
C
      CLKSTR = '4321.4321'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SCFMT ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     We're expecting exact agreement.
C
      XCLKST = '000004321.4321'
      CALL CHCKSC ( '2) CLKSTR', CLKSTR, '=', XCLKST,  OK )





C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Convert ET values to integer ticks via SCE2T' )

      CLKID  = -9

      CALL STR2ET ( '1980 JAN 1 00:00:00 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     First try the ET value corresponding to clock start.
C
      CALL SCE2T  ( CLKID, ET0, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 0.D0
      CALL CHCKSD ( 'SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Convert ET values to continuous ticks via SCE2C' )

      CLKID  = -9

      CALL STR2ET ( '1980 JAN 1 00:00:00 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     First try the ET value corresponding to clock start.
C
      CALL SCE2C  ( CLKID, ET0, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 0.D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

C
C     Now a greater ET value.
C
      DELTA = 1.D8 + 0.3333D0

      ET    = ET0 + DELTA

      CALL SCE2C  ( CLKID, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      XCLKDP = DELTA * 1.D4

C
C     We should get some round-off here, so don't require
C     an exact match.
C
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '~/', XCLKDP, VTIGHT, OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Convert continuous ticks to ET via SCT2E' )

      CLKID  = -9

      CALL STR2ET ( '1980 JAN 1 00:00:00 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Use an ET value that corresponds to a fractional tick count.
C
      DELTA = 1.D8 + 0.3333D0

      XET    = ET0 + DELTA

      CALL SCE2C  ( CLKID, XET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SCT2E  ( CLKID, SCLKDP, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should get some round-off here, so don't require
C     an exact match.
C
      CALL CHCKSD ( '2) ET', ET, '~/', XET, VTIGHT, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Convert an SCLK string to ET via SCS2E' )

      CLKID  = -9
      CLKSTR = '1/987654321.4321'

C
C     Compute the expected value using SCENCD and
C     SCT2E.
C
      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCT2E  ( CLKID, SCLKDP, XET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Now try the conversion with SCS2E.
C
      CALL SCS2E ( CLKID, CLKSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We're expecting exact agreement.
C
      CALL CHCKSD ( 'ET', ET, '=', XET, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Convert an ET value to an SCLK string via SCE2S.' )

C
C     Generate the ET value.
C
      CLKID  = -9
      CLKSTR = '1/987654321.4321'

      CALL SCS2E ( CLKID, CLKSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Compute the expected value using SCE2T and SCDECD.
C
      CALL SCE2T  ( CLKID, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCDECD ( CLKID, SCLKDP, XCLKST )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Now try the conversion with SCE2S.
C
      CALL SCE2S ( CLKID, ET, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We're expecting exact agreement.
C
      CALL CHCKSC ( 'CLKSTR', CLKSTR, '=', XCLKST, OK )


C
C --- Case: ------------------------------------------------------
C
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

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clock -99: Recover partition bounds via SCPART' )

      CLKID = -99


      CALL SCPART ( CLKID, N, PSTART, PSTOP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', NPARTS, 0, OK )

      CALL CHCKAD ( 'PSTART', PSTART, '=', XSTART, NPARTS, 0.D0, OK )
      CALL CHCKAD ( 'PSTOP',  PSTOP,  '=', XSTOP,  NPARTS, 0.D0, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clock -99: Convert tick coefficients to SCLK '
     .//           'strings via SCDECD. Invert the conversion via '
     .//           'SCENCD.'                                  )

      CLKID  = -99

      DO I = 1, NCOEFF

         CALL SCDECD  ( CLKID, COEFFS(1,I), CLKSTR )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Derive the expected string based on the SCLK kernel
C        structure: every record whose index is equivalent
C        to 1 mod 4 is at the start of a new partition. Each
C        record is 180000 ticks ahead of the previous one.
C
         CALL RMAINI ( I-1, 4, Q, REM )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        The partition number is simply Q+1.
C
         PARTNO = Q + 1

         CALL INTSTR ( PARTNO, XCLKST )

         CALL SUFFIX ( '/', 0, XCLKST )
C
C        Let J be the number of ticks past the
C        partition start reading. Let SCLKDP
C        be the actual clock reading; we obtain
C        SCLKDP by adding the partition start reading
C        to J.
C
         J      = NINT ( REM * 180000.D0 )
         SCLKDP = J + XSTART(PARTNO )
C
C        Convert SCLKDP to an SCLK string.
C
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

C
C        Pad the first field on the left with zeros.
C
         W = LASTNB( FSTR(1) )

         DO K = 1, 7-W
            CALL PREFIX ( '0', 0, FSTR(1) )
         END DO
C
C        Append the field and delimiters to the expected
C        string.
C
         DO K = 1, 3
            CALL SUFFIX ( FSTR(K), 0, XCLKST )
            CALL SUFFIX ( ':',     0, XCLKST )
         END DO

         CALL SUFFIX ( FSTR(4), 0, XCLKST )

         TITLE = 'CLKSTR(#)'
         CALL REPMI ( TITLE, '#', I, TITLE )

C
C        Look for an exact match.
C
         CALL CHCKSC ( TITLE, CLKSTR, '=', XCLKST, OK )

C
C        Now convert the string to a tick value.
C
         CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Compare the tick value to the tick coefficient.
C
         CALL CHCKSD ( 'SCLKDP', SCLKDP, '=', COEFFS(1,I), 0.D0, OK )

      END DO



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clock -99: Convert tick coefficients to ET '
     .//           'via SCT2E' )

      CLKID  = -99

      DO I = 1, NCOEFF

         CALL SCT2E  ( CLKID, COEFFS(1,I), ET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XET = COEFFS(2,I)

         TITLE = 'ET(#)'
         CALL REPMI ( TITLE, '#', I, TITLE )

C
C        Don't look for an exact match, since NPARSD, which is
C        used by the text kernel reader routines, can introduce
C        round-off error in integral inputs (!) depending on
C        their format.
C
         CALL CHCKSD ( TITLE, ET, '~', XET, MED, OK )

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clock -99: Convert tick coefficient '
     .//           'midpoints to ET via SCT2E'  )

      CLKID  = -99

C
C     The tick offset value is constant, given the SCLK kernel
C     we've created.
C
      CLKOFF = ( COEFFS(1,1) + COEFFS(1,2) ) / 2

      DO I = 1, NCOEFF

         SCLKDP = COEFFS(1,I) + CLKOFF

         CALL SCT2E  ( CLKID, SCLKDP, ET )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        The ET offset is based on the rate for the previous
C        (Ith) coefficient record.
C
         XET = COEFFS(2,I) + (CLKOFF/MAJOR)*COEFFS(3,I)

         TITLE = 'ET(#)'
         CALL REPMI ( TITLE, '#', I, TITLE )

C
C        Look for tight, but not exact, match.
C
         CALL CHCKSD ( TITLE, ET, '~/', XET, VTIGHT, OK )

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clock -99: Convert TDB coefficients to '
     .//           'ticks via SCE2T'                          )

      CLKID  = -99

      DO I = 1, NCOEFF

         CALL SCE2T  ( CLKID, COEFFS(2,I), SCLKDP )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XCLKDP = COEFFS(1,I)

         TITLE = 'SCLKDP(#)'
         CALL REPMI ( TITLE, '#', I, TITLE )

C
C        Look for an exact match.
C
         CALL CHCKSD ( TITLE, SCLKDP, '=', XCLKDP, 0.D0, OK )

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clock -99: Convert tick coefficient '
     .//           'midpoints to ET via SCT2E'            )

      CLKID  = -99

C
C     The tick offset value is constant, given the SCLK kernel
C     we've created.
C
      CLKOFF = ( COEFFS(1,1) + COEFFS(1,2) ) / 2

      DO I = 1, NCOEFF

         SCLKDP = COEFFS(1,I) + CLKOFF

         CALL SCT2E  ( CLKID, SCLKDP, ET )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        The ET offset is based on the rate for the previous
C        (Ith) coefficient record.
C
         XET = COEFFS(2,I) + (CLKOFF/MAJOR)*COEFFS(3,I)

         TITLE = 'ET(#)'
         CALL REPMI ( TITLE, '#', I, TITLE )

C
C        Look for tight, but not exact, match.
C
         CALL CHCKSD ( TITLE, ET, '~/', XET, VTIGHT, OK )

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clock -99: Convert ET coefficient midpoints to '
     .//           'continuous ticks via SCE2C'                     )

      CLKID  = -99

C
C     The ET offset value is constant, given the SCLK kernel
C     we've created.
C
      ETOFF = ( COEFFS(2,1) + COEFFS(2,2) ) / 2

      DO I = 1, NCOEFF

         ET = COEFFS(2,I) + ETOFF

         CALL SCE2C  ( CLKID, ET, SCLKDP )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        The tick offset is based on the rate for the previous
C        (Ith) coefficient record.
C
C        The expected tick offset is the ET offset divided
C        by the applicable rate, which has units of seconds
C        per major field, times the ticks per major field.
C
         XCLKDP = COEFFS(1,I) + MAJOR*ETOFF/COEFFS(3,I)

         TITLE = 'SCLKDP (#)'
         CALL REPMI ( TITLE, '#', I, TITLE )

C
C        Look for tight, but not exact, match.
C
         CALL CHCKSD ( TITLE, SCLKDP, '~/', XCLKDP, VTIGHT, OK )

      END DO


C
C --- Case: ------------------------------------------------------
C
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

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clock -999: Encode SCLK strings via SCENCD.' )

      CLKID  = -999
      CLKSTR = '1/4:03:02:2'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Note that we're expecting exact agreement.
C
      XCLKDP = 0.D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

C
C     Now repeat the test, this time excluding the partition
C     number.
C
      CLKSTR = '4:03:02:2'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 0.D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

C
C     Use a larger input value.
C
      CLKSTR = '1/5:03:02:2'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 720.D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

C
C     Use an input value in the second partition.
C
      CLKSTR = '2/1393:11:10:3'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 720720.D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clock -999: Encode SCLK string with overflow '
     .//           'field values via SCENCD.'                     )

      CLKID  = -999
      CLKSTR = '1/4:03:02:11'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Note that we're expecting exact agreement.
C
      XCLKDP = 9.D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )


      CLKSTR = '1/4:13:02:11'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Note that we're expecting exact agreement.
C
      XCLKDP = 729.D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

C
C     Overflow in the second and fourth fields:
C
      CLKSTR = '1/4:13:02:11'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Note that we're expecting exact agreement.
C
      XCLKDP = 729.D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

C
C     Overflow in the second, third and fourth fields:
C
      CLKSTR = '1/4:13:12:11'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Note that we're expecting exact agreement.
C
      XCLKDP = 809.D0
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clock -999: Decode SCLK tick values via SCDECD.' )

      CLKID  = -999
      XCLKST = '1/00000004:03:02:2'

      CALL SCENCD ( CLKID, XCLKST, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( '1) CLKSTR', CLKSTR, '=', XCLKST,  OK )


C
C     Use a larger input value.
C
      XCLKST = '1/00000005:03:02:2'

      CALL SCENCD ( CLKID, XCLKST, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( '2) CLKSTR', CLKSTR, '=', XCLKST,  OK )

C
C     Use an input value in the second partition.
C
      XCLKST = '2/00001393:11:10:3'

      CALL SCENCD ( CLKID, XCLKST, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( '3) CLKSTR', CLKSTR, '=', XCLKST,  OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clock -999: Convert TDT values to '
     .//           'integer ticks via SCE2T'           )

      CLKID  = -999

      CALL STR2ET ( '2000 JAN 1 12:00:00 TDT', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     First try the TDT value corresponding to clock start.
C
      CALL SCE2T  ( CLKID, ET0, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 0.D0
      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )


C
C     First now try a greater TDT value: 1000 seconds past clock
C     start.
C
      CALL STR2ET ( '2000 JAN 1 12:16:40 TDT', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCE2T  ( CLKID, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 3.6D5
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clock -999: Convert TDT values to '
     .//           'continuous ticks via SCE2C'          )

      CLKID  = -999

      CALL STR2ET ( '2000 JAN 1 12:00:00.125 TDT', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     First try the TDT value corresponding to 1/8 second
C     past clock start.
C
      CALL SCE2C  ( CLKID, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = ( .125D0 / COEFFS(3,1) ) * 720.D0

      CALL CHCKSD ( '1) SCLKDP', SCLKDP, '~', XCLKDP, VTIGHT, OK )

C
C     Now a greater TDT value.
C
      TDT = 1.D3 + 0.3333D0

      ET  = UNITIM ( TDT, 'TDT', 'TDB' )

      CALL SCE2C  ( CLKID, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      XCLKDP =      COEFFS(1,3)
     .          + ( (TDT-COEFFS(2,3)) / COEFFS(3,3) ) * 720.D0

C
C     We should get some round-off here, so don't require
C     an exact match.
C
      CALL CHCKSD ( '2) SCLKDP', SCLKDP, '~/', XCLKDP, VTIGHT, OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clock -999: Convert continuous ticks to '
     .//           'TDT via SCT2E'                            )

      CLKID  = -999

      CALL STR2ET ( '2000 JAN 1 12:00:00 TDT', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Use a TDT value that corresponds to a fractional
C     tick count.
C
      DELTA = 1.D3 + 0.3333D0

      XTDT  = UNITIM ( ET0,  'TDB', 'TDT' ) + DELTA
      XET   = UNITIM ( XTDT, 'TDT', 'TDB' )

      CALL SCE2C  ( CLKID, XET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SCT2E  ( CLKID, SCLKDP, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TDT = UNITIM ( ET, 'TDB', 'TDT' )

C
C     We should get some round-off here, so don't require
C     an exact match.
C
      CALL CHCKSD ( '2) TDT', XTDT, '~/', XTDT, VTIGHT, OK )



C
C     Test entry points of SCTRAN.
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCN2ID: convert GLL clock name to ID' )

      CALL SCN2ID ( 'GLL SCLK', CLKID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CLKID', CLKID, '=', -77, 0, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCN2ID: convert unknown clock name to ID' )

      CLKID = 7
      CALL SCN2ID ( 'XYZ SCLK', CLKID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL CHCKSI ( 'CLKID', CLKID, '=', 7, 0, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCN2ID: convert recognized non-clock name to ID' )

      CALL SCN2ID ( 'JUPITER BARYCENTER SCLK', CLKID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,    OK )
      CALL CHCKSI ( 'CLKID', CLKID, '=', 5, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCID2N: convert CASSINI clock ID to name' )

      CALL SCID2N ( -82, CLKNAM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
      CALL CHCKSC ( 'CLKNAM', CLKNAM, '=', 'CASSINI SCLK', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCID2N: convert unknown clock ID to name' )

      CALL SCID2N ( 999999, CLKNAM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )

C      CALL BYEBYE (' ' )

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
