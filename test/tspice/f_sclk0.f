C$Procedure F_SCLK0 ( SCLK error handling tests )

      SUBROUTINE F_SCLK0 ( OK )

C$ Abstract
C
C     Exercise error handling in the SCLK time conversion and
C     kernel pool access routines.
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

      INCLUDE 'errhnd.inc'
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
C     This routine tests error handling in the SCLK routines
C
C        SC01
C        SCDECD
C        SCEC01
C        SCENCD
C        SCET01
C        SCFM01
C        SCLD01
C        SCLI01
C        SCLU01
C        SCPARS
C        SCPS01
C        SCTE01
C        SCTK01
C        SCTY01
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
C-    TSPICE Version 1.0.0 01-DEC-2021 (NJB)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      UNITIM

      INTEGER               SCTYPE

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


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      INTEGER               DVALSZ
      PARAMETER           ( DVALSZ =  50 )

      INTEGER               IVALSZ
      PARAMETER           ( IVALSZ =  50 )

      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN =  32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN =  50 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 40 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               NCOM
      PARAMETER           ( NCOM   = 3 )

      INTEGER               SHRTLN
      PARAMETER           ( SHRTLN = 5 )

C
C     Local Variables
C
C
C     We use a longer, non-standard length for ABCORR because we
C     want to include embedded blanks for testing.
C
      CHARACTER*(NAMLEN)    CLKNAM
      CHARACTER*(TIMLEN)    CLKSTR
      CHARACTER*(LNSIZE)    COMS   ( LBCELL : NCOM )
      CHARACTER*(LMSGLN)    ERRMSG
      CHARACTER*(KVNMLN)    ITEM
      CHARACTER*(KVNMLN)    KVNAME
      CHARACTER*(SHRTLN)    SHORT
      CHARACTER*(SMSGLN)    SHRTMS
      CHARACTER*(LMSGLN)    XMSG

      DOUBLE PRECISION      COEFFS ( 3, MXCOEF + 1 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DVALS  ( DVALSZ )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      KERNID
      DOUBLE PRECISION      MAJOR
      DOUBLE PRECISION      MODULI ( MXNFLD )
      DOUBLE PRECISION      OFFSET ( MXNFLD )
      DOUBLE PRECISION      PSTART ( MXPART + 1 )
      DOUBLE PRECISION      PSTOP  ( MXPART + 1 )
      DOUBLE PRECISION      TDT
      DOUBLE PRECISION      TICKS
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      XCLKDP
      DOUBLE PRECISION      XET
      DOUBLE PRECISION      XSTART ( MXPART + 1 )
      DOUBLE PRECISION      XSTOP  ( MXPART + 1 )

      INTEGER               CLKID
      INTEGER               CLKTYP
      INTEGER               FIELD ( 4 )
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IVAL
      INTEGER               IVALS ( IVALSZ )
      INTEGER               N
      INTEGER               NCOEFF
      INTEGER               NFIELD
      INTEGER               NPARTS

      LOGICAL               ERROR
      LOGICAL               FOUND

C
C     Saved variables
C
      SAVE


C
C     Open the test family.
C
      CALL TOPEN ( 'F_SCLK0' )

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

C*********************************************************
C
C
C     End of kernel creation code
C
C
C*********************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCDECD: ticks not in any partition' )

      CLKID  = -99
      SCLKDP = -1.0D0

      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C     Output string is supposed to be blank upon failure.
C
      CALL CHCKSC ( 'CLKSTR (-1.D0)', CLKSTR, '=', ' ', OK )


      SCLKDP = 720720001.D0

      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL CHCKSC ( 'CLKSTR (720721002.D0)', CLKSTR, '=', ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCDECD: output string truncated, caught in '
     .//           'SCFMT' )

      CLKID  = -99
      SCLKDP =  720720000.D0

      CALL SCDECD ( CLKID, SCLKDP, SHORT )

      CALL CHCKXC ( .TRUE., 'SPICE(SCLKTRUNCATED)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCDECD: output string truncated, caught in '
     .//           'SCDECD' )

      CLKID  = -99
      SCLKDP =  720720000.D0

      CALL SCDECD ( CLKID, SCLKDP, CLKSTR(:15) )

      CALL CHCKXC ( .TRUE., 'SPICE(SCLKTRUNCATED)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCDECD: SCPART failure' )

C
C     Force a failure of SCPART.
C
      CALL DVPOOL ( 'SCLK_PARTITION_END_99' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLKID  = -99
      SCLKDP =  720720000.D0

      CALL SCDECD ( CLKID, SCLKDP, CLKSTR )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCENCD: SCPART failure' )

C
C     Force a failure of SCPART.
C
      CALL DVPOOL ( 'SCLK_PARTITION_END_99' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLKID  = -99
      CLKSTR = '9999999:9:8:7'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCENCD: SCTIKS failure' )

      CLKID  = -99

C
C     Create clock string with too many fields.
C
      CLKSTR = '0000000:9:8:7:6'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKSTRING)', OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCENCD: blank input string' )

      CLKID  = -99

      CLKSTR = ' '

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKSTRING)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCENCD: time string ends with slash' )

      CLKID  = -99

      CALL SCENCD ( CLKID, ' /', SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKSTRING)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCENCD: no time string following slash' )

      CLKID  = -99

      CALL SCENCD ( CLKID, '1/ ', SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKSTRING)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCENCD: No partition number preceding slash' )

      CLKID  = -99

      CLKSTR = '/0000000:9:8:7'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(BADPARTNUMBER)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCENCD: Non-numeric partition token preceding '
     .//           'slash' )

      CLKID  = -99

      CLKSTR = 'x/0000000:9:8:7'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(BADPARTNUMBER)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCENCD: partition number out of range' )

      CLKID  = -99

C
C     Try partition numbers that are
C
C        negative
C        zero
C        too large
C
      CLKSTR = '-1/0000000:9:8:7'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(BADPARTNUMBER)', OK )


      CLKSTR = '0/0000000:9:8:7'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(BADPARTNUMBER)', OK )


      CLKSTR = '1002/0000000:9:8:7'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(BADPARTNUMBER)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCENCD: clock count not in indicated partition' )

      CLKID  = -99

      CLKSTR = '1/0001001:0:0:0'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTINPART)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCENCD: clock count not in any partition' )

      CLKID  = -99

      CLKSTR = '0000000:0:0:0'

      CALL SCENCD ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(NOPARTITION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPARS: SCPART failure' )

C
C     Force a failure of SCPART.
C
      CALL DVPOOL ( 'SCLK_PARTITION_END_99' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLKID  = -99
      CLKSTR = '9999999:9:8:7'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPARS: SCTIKS failure' )

      CLKID  = -99

C
C     Create clock string with too many fields.
C
      CLKSTR = '0000000:9:8:7:6'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

      XMSG = 'Input clock string 0000000:9:8:7:6 has 5 fields; '
     .//     'maximum for this spacecraft clock is 4.'

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPARS: unsupported clock type' )

      CLKID = -101

      KVNAME = 'SCLK_DATA_TYPE_101'

      CALL PIPOOL ( KVNAME, 1, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCLKDP = 0.D0

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPARS: blank input string' )

      CLKID  = -99

      CLKSTR = ' '

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

      XMSG = 'Input spacecraft clock string is blank.'

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPARS: input string ends with slash' )

      CLKID  = -99

      CALL SCPARS ( CLKID, ' /', ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

      XMSG = 'SCLK string ends with slash.'

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPARS: no time string following slash' )

      CLKID  = -99

      CLKSTR = '1/ '

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

      XMSG = 'Non partition part of the input clock string is blank.'

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPARS: No partition number preceding slash' )

      CLKID  = -99

      CLKSTR = '/0000000:9:8:7'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

      XMSG = 'Unable to parse the partition number from SCLK string '
     .//     '/0000000:9:8:7.'

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPARS: Non-numeric partition token preceding '
     .//           'slash' )

      CLKID  = -99

      CLKSTR = 'x/0000000:9:8:7'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

      XMSG = 'Unable to parse the partition number from SCLK string '
     .//     'x/0000000:9:8:7.'

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPARS: partition number out of range' )

      CLKID  = -99

C
C     Try partition numbers that are
C
C        negative
C        zero
C        too large
C
      CLKSTR = '-1/0000000:9:8:7'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

      XMSG = 'Partition number -1 taken from SCLK string '
     .//     '-1/0000000:9:8:7 is not in acceptable range 1 to 1001.'

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )


      CLKSTR = '0/0000000:9:8:7'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

      XMSG = 'Partition number 0 taken from SCLK string '
     .//     '0/0000000:9:8:7 is not in acceptable range 1 to 1001.'

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )


      CLKSTR = '1002/0000000:9:8:7'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

      XMSG = 'Partition number 1002 taken from SCLK string '
     .//     '1002/0000000:9:8:7 is not in acceptable range 1 to 1001.'

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPARS: clock count not in indicated partition' )

      CLKID  = -99

      CLKSTR = '1/0001001:0:0:0'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

      XMSG = 'SCLK count from 0001001:0:0:0 does not fall in the '
     .//     'boundaries of partition number 1.'

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPARS: clock count not in any partition' )

      CLKID  = -99

      CLKSTR = '0000000:0:0:0'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ERROR', ERROR, .TRUE., OK )

      XMSG = 'SCLK count 0000000:0:0:0 does not fall in the '
     .//     'boundaries of any of the partitions for spacecraft -99.'

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPARS: non-error exception: fields are blank, '
     .//           'but string is not blank.' )

      CLKID  = -999

C
C     The spec of SCTK01 says that blank fields are treated as though
C     they contain the offset corresponding to that field. This makes
C     the values of these fields effectively zero.
C
      CLKSTR = ':::3'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'ERROR',  ERROR,  .FALSE.,  OK )
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', ' ', OK )

      XCLKDP = 1.D0

      TOL = 0.D0
      CALL CHCKSD ( 'SCLKDP (A)', SCLKDP, '=', XCLKDP, TOL, OK )


      CLKSTR = ' : : : 3'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'ERROR',  ERROR,  .FALSE.,  OK )
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', ' ', OK )

      XCLKDP = 1.D0

      TOL = 0.D0
      CALL CHCKSD ( 'SCLKDP (B)', SCLKDP, '=', XCLKDP, TOL, OK )



      CLKSTR = '0000004: : :3'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'ERROR',  ERROR,  .FALSE.,  OK )
      CALL CHCKSC ( 'ERRMST', ERRMSG, '=', ' ', OK )

      XCLKDP = 1.D0

      TOL = 0.D0
      CALL CHCKSD ( 'SCLKDP (C)', SCLKDP, '=', XCLKDP, TOL, OK )


      CLKSTR = '0000004: :4: 2'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'ERROR',  ERROR,  .FALSE.,  OK )
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', ' ', OK )

      XCLKDP = 16.D0

      TOL = 0.D0
      CALL CHCKSD ( 'SCLKDP (D)', SCLKDP, '=', XCLKDP, TOL, OK )


      CLKSTR = '0000004:6: : 2'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'ERROR',  ERROR,  .FALSE.,  OK )
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', ' ', OK )

      XCLKDP = 216.D0

      TOL = 0.D0
      CALL CHCKSD ( 'SCLKDP (E)', SCLKDP, '=', XCLKDP, TOL, OK )


      CLKSTR = '0000008: : : 2'

      CALL SCPARS ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'ERROR',  ERROR,  .FALSE.,  OK )
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', ' ', OK )

      XCLKDP = 2880.D0

      TOL = 0.D0
      CALL CHCKSD ( 'SCLKDP (E)', SCLKDP, '=', XCLKDP, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPS01: moduli not found by SCLD01' )

      CLKID = -9

      CALL DVPOOL ( 'SCLK01_MODULI_9' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLKSTR = '1/000000000.0001'

      CALL SCPS01 ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Restore data for this clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPS01: offsets not found by SCLD01' )

      CLKID = -9

      CALL DVPOOL ( 'SCLK01_OFFSETS_9' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLKSTR = '1/000000000.0001'

      CALL SCPS01 ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Restore data for this clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPS01: offsets not found by SCLD01' )

      CLKID = -9

      CALL DVPOOL ( 'SCLK01_OFFSETS_9' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLKSTR = '1/000000000.0001'

      CALL SCPS01 ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Restore data for this clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPS01: field count not found by SCLI01' )

      CLKID = -9

      CALL DVPOOL ( 'SCLK01_N_FIELDS_9' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLKSTR = '1/000000000.0001'

      CALL SCPS01 ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Restore data for this clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPS01: blank input string' )

      CLKID  = -999

      CLKSTR = ' '

      CALL SCPS01 ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'ERROR',  ERROR,  .TRUE.,  OK )

      XMSG = 'Non partition part of the input clock string is blank.'

      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPS01: components out of range' )

      CLKID  = -999

      CLKSTR = '0000000:3:2:2'

      CALL SCPS01 ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'ERROR (A)',  ERROR,  .TRUE.,  OK )

      XMSG = 'Component number 1, 0000000 in the SCLK string  '
     .//     '0000000:3:2:2 is invalid.'

      CALL CHCKSC ( 'ERRMSG (A)', ERRMSG, '=', XMSG, OK )


      CLKSTR = '0000004:1:2:2'

      CALL SCPS01 ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'ERROR (B)',  ERROR,  .TRUE.,  OK )

      XMSG = 'Component number 2, 1 in the SCLK string  '
     .//     '0000004:1:2:2 is invalid.'

      CALL CHCKSC ( 'ERRMSG (B)', ERRMSG, '=', XMSG, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPS01: non-numeric clock component' )

      CLKID = -9

      CLKSTR = '000000000.x001'

      CALL SCPS01 ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XMSG = 'Could not parse SCLK component x001 from '
     .//     '000000000.x001 as a number.'

      CALL CHCKSL ( 'ERROR',  ERROR,  .TRUE.,    OK )
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCPS01: too many fields in clock string' )

      CLKID = -9

      CLKSTR = '000000000.001.00'

      CALL SCPS01 ( CLKID, CLKSTR, ERROR, ERRMSG, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XMSG = 'Input clock string 000000000.001.00 has 3 fields; '
     .//     'maximum for this spacecraft clock is 2.'

      CALL CHCKSL ( 'ERROR',  ERROR,  .TRUE.,    OK )
      CALL CHCKSC ( 'ERRMSG', ERRMSG, '=', XMSG, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCFMT: unsupported clock type' )

      CLKID = -101

      KVNAME = 'SCLK_DATA_TYPE_101'

      CALL PIPOOL ( KVNAME, 1, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCLKDP = 0.D0

      CALL SCFMT ( CLKID, SCLKDP, CLKSTR )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCT2E: unsupported clock type' )

      CLKID = -101

      KVNAME = 'SCLK_DATA_TYPE_101'

      CALL PIPOOL ( KVNAME, 1, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCLKDP = 0.D0

      CALL SCT2E ( CLKID, SCLKDP, ET )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCTIKS: unsupported clock type' )

      CLKID = -101

      KVNAME = 'SCLK_DATA_TYPE_101'

      CALL PIPOOL ( KVNAME, 1, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLKSTR = '0000000:0:0:0'

      CALL SCTIKS ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCTK01: blank input string' )

      CLKID  = -99

      CLKSTR = ' '

      CALL SCTK01 ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKSTRING)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCTK01: too many fields in input string' )

      CLKID  = -99

      CLKSTR = '9:9:8:7:6'

      CALL SCTK01 ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKSTRING)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCTK01: input string component is not a number' )

      CLKID  = -99

      CLKSTR = '0000000:w:7:6'

      CALL SCTK01 ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKSTRING)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCTK01: input string component is invalid after '
     .//           'subtracting offset' )

C
C     Note: change of clock
C
      CLKID  = -999

      CLKSTR = '00000004:4:7:0'

      CALL SCTK01 ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKSTRING)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCTK01: non-error exception: fields are blank, '
     .//           'but string is not blank.' )

      CLKID  = -999

C
C     The spec of SCTK01 says that blank fields are treated as though
C     they contain the offset corresponding to that field. This makes
C     the values of these fields effectively zero.
C
      CLKSTR = ':::'

      CALL SCTK01 ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 0.D0

      TOL = 0.D0
      CALL CHCKSD ( 'SCLKDP (A)', SCLKDP, '=', XCLKDP, TOL, OK )


      CLKSTR = ' : : : '

      CALL SCTK01 ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 0.D0

      TOL = 0.D0
      CALL CHCKSD ( 'SCLKDP (B)', SCLKDP, '=', XCLKDP, TOL, OK )



      CLKSTR = '0000004: : :2'

      CALL SCTK01 ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 1.D0

      TOL = 0.D0
      CALL CHCKSD ( 'SCLKDP (C)', SCLKDP, '=', XCLKDP, TOL, OK )


      CLKSTR = '0000004: :4: '

      CALL SCTK01 ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 16.D0

      TOL = 0.D0
      CALL CHCKSD ( 'SCLKDP (D)', SCLKDP, '=', XCLKDP, TOL, OK )


      CLKSTR = '0000004:6: : '

      CALL SCTK01 ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 216.D0

      TOL = 0.D0
      CALL CHCKSD ( 'SCLKDP (E)', SCLKDP, '=', XCLKDP, TOL, OK )


      CLKSTR = '0000008: : : '

      CALL SCTK01 ( CLKID, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 2880.D0

      TOL = 0.D0
      CALL CHCKSD ( 'SCLKDP (E)', SCLKDP, '=', XCLKDP, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCTK01: exercise pass 1 logic after error' )

C
C     Cause error:
C
      CLKID  = -999

      CLKSTR = '0000004:4:7:0'

      CALL SCTK01 ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKSTRING)', OK )

C
C     Call routine with valid inputs:
C
C     Note: offsets for this clock are 4, 3, 2, 1, so the string below
C     is equivalent to
C
C        '0000999:9:8:7'
C
C     after offsets are subtracted.
C
      CLKSTR = '0001003:12:10:8'

      CALL SCTK01 ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'SCLKDP', SCLKDP, '=', 719999.D0, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCTK01: ZZSCUP01 failure' )

C
C     Cause failure.
C
      CALL DVPOOL ( 'SCLK01_MODULI_999' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLKID  = -999

      CLKSTR = '0001003:12:10:8'

      CALL SCTK01 ( CLKID, CLKSTR, SCLKDP )

      CALL SCEC01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Check recovery.
C
      CALL LDPOOL ( SCLK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLKSTR = '0001003:12:10:8'

      CALL SCTK01 ( CLKID, CLKSTR, SCLKDP )

      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'SCLKDP', SCLKDP, '=', 719999.D0, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCFM01: negative ticks' )

      CLKID  = -999

      SCLKDP = -1

      CALL SCFM01 ( CLKID, SCLKDP, CLKSTR )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCFM01: output string truncation' )

      CLKID  = -999

      SCLKDP = 719999.D0

      CALL SCFM01 ( CLKID, SCLKDP, SHORT )

      CALL CHCKXC ( .TRUE., 'SPICE(SCLKTRUNCATED)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCFM01: exercise pass 1 logic after error' )

C
C     Cause error:
C
      SCLKDP = -1

      CALL SCFM01 ( CLKID, SCLKDP, CLKSTR )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C     Call routine with valid inputs:
C
      SCLKDP = 719999.D0

      CALL SCFM01 ( CLKID, SCLKDP, CLKSTR )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'CLKSTR', CLKSTR, '=', '00001003:12:10:8', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCFM01: ZZSCUP01 failure' )

C
C     Cause failure.
C
      CALL DVPOOL ( 'SCLK01_MODULI_999' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLKSTR = '0001003:12:10:8'

      SCLKDP = 719999.D0

      CALL SCFM01 ( CLKID, SCLKDP, CLKSTR )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Restore data for clock.
C
      CALL LDPOOL ( SCLK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCLKDP = 719999.D0

      CALL SCFM01 ( CLKID, SCLKDP, CLKSTR )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'CLKSTR', CLKSTR, '=', '00001003:12:10:8', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCTE01: ticks out of range' )

      CLKID  = -999

      SCLKDP = -1.D0

      CALL SCTE01 ( CLKID, SCLKDP, ET )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      SCLKDP = ( 4 * 720000.D0 ) + 1.D0

      CALL SCTE01 ( CLKID, SCLKDP, ET )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCTE01: invalid clock rate' )

C
C     Note: change of clock
C
      CLKID  = -9

      SCLKDP = 1.D0

      KVNAME = 'SCLK01_COEFFICIENTS_9'

      CALL GDPOOL ( KVNAME, 1, 3, N, COEFFS, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Use 0 rate.
C
      COEFFS( 3, 1 ) = 0.D0

      CALL PDPOOL ( KVNAME, 3, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCTE01 ( CLKID, SCLKDP, ET )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C     Use negative rate.
C
      COEFFS( 3, 1 ) = -1.D0

      CALL PDPOOL ( KVNAME, 3, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCTE01 ( CLKID, SCLKDP, ET )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C     Restore data for clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCTE01: exercise pass 1 logic after error' )

C
C     Cause error:
C
      CLKID  = -999

      SCLKDP = -1.D0

      CALL SCTE01 ( CLKID, SCLKDP, ET )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C
C     Check recovery.
C
      SCLKDP = 180000.D0

      CALL SCTE01 ( CLKID, SCLKDP, ET )

      CALL CHCKXC ( .FALSE., ' ', OK )

      TDT = 5.D2

      XET = UNITIM ( TDT, 'TDT', 'TDB' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'ET', ET, '~/', XET, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCTE01: ZZSCUP01 failure' )

C
C     Cause failure.
C
      CALL DVPOOL ( 'SCLK01_MODULI_999' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCLKDP = 180000.D0

      CALL SCTE01 ( CLKID, SCLKDP, ET )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Check recovery.
C
      CALL LDPOOL ( SCLK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCLKDP = 180000.D0

      CALL SCTE01 ( CLKID, SCLKDP, ET )

      CALL CHCKXC ( .FALSE., ' ', OK )

      TDT = 5.D2

      XET = UNITIM ( TDT, 'TDT', 'TDB' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'ET', ET, '~/', XET, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCET01: computed ticks out of range' )

      CLKID  = -999

      ET = -1.D0

      CALL SCET01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      ET = 1.D13

      CALL SCET01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCET01: ET is less than least coefficient '
     .//           'but corresponding tick value rounds up to zero. '
     .//           'This is a non-error exception.' )

      CLKID  = -999

      ET = -1.D-6

      CALL SCET01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'SCLKDP', SCLKDP, '~', 0.D0, TOL, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCET01: invalid clock rate' )

C
C     Note: change of clock
C
      CLKID  = -9

      ET = 1.D0

      KVNAME = 'SCLK01_COEFFICIENTS_9'

      CALL GDPOOL ( KVNAME, 1, 3, N, COEFFS, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Use 0 rate.
C
      COEFFS( 3, 1 ) = 0.D0

      CALL PDPOOL ( KVNAME, 3, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCET01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C     Use negative rate.
C
      COEFFS( 3, 1 ) = -1.D0

      CALL PDPOOL ( KVNAME, 3, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCET01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C     Restore data for clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCET01: exercise pass 1 logic after error' )

C
C     Cause error:
C
C     Note: change of clock
C
      CLKID  = -999

      ET     = -1.D0

      CALL SCET01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C
C     Check recovery.
C
      TDT = 5.D2

      ET = UNITIM ( TDT, 'TDT', 'TDB' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 180000.D0

      CALL SCET01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'SCLKDP', SCLKDP, '~/', XCLKDP, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCET01: ZZSCUP01 failure' )

C
C     Cause failure.
C
      CALL DVPOOL ( 'SCLK01_MODULI_999' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLKID  = -999

      ET = 1.D0

      CALL SCET01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Check recovery.
C
      CALL LDPOOL ( SCLK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TDT = 5.D2

      ET = UNITIM ( TDT, 'TDT', 'TDB' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 180000.D0

      CALL SCET01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'SCLKDP', SCLKDP, '~/', XCLKDP, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCEC01: invalid clock rate' )

C
C     Note: change of clock
C
      CLKID  = -9

      ET = 1.D0

      KVNAME = 'SCLK01_COEFFICIENTS_9'

      CALL GDPOOL ( KVNAME, 1, 3, N, COEFFS, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Use 0 rate.
C
      COEFFS( 3, 1 ) = 0.D0

      CALL PDPOOL ( KVNAME, 3, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCEC01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C     Use negative rate.
C
      COEFFS( 3, 1 ) = -1.D0

      CALL PDPOOL ( KVNAME, 3, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCEC01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C     Restore data for clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCEC01: computed ticks out of range' )

      CLKID  = -999

      ET = -1.D0

      CALL SCEC01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      ET = 1.D13

      CALL SCEC01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCEC01: exercise pass 1 logic after error' )

C
C     Cause error:
C
C     Note: change of clock
C
      CLKID  = -999

      ET     = -1.D0

      CALL SCEC01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C
C     Check recovery.
C
      TDT = 5.D2

      ET = UNITIM ( TDT, 'TDT', 'TDB' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 180000.D0

      CALL SCEC01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'SCLKDP', SCLKDP, '~/', XCLKDP, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCEC01: ZZSCUP01 failure' )

C
C     Cause failure.
C
      CALL DVPOOL ( 'SCLK01_MODULI_999' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLKID  = -999

      ET = 1.D0

      CALL SCEC01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Check recovery.
C
      CALL LDPOOL ( SCLK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TDT = 5.D2

      ET = UNITIM ( TDT, 'TDT', 'TDB' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XCLKDP = 180000.D0

      CALL SCEC01 ( CLKID, ET, SCLKDP )

      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'SCLKDP', SCLKDP, '~/', XCLKDP, TOL, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCTY01: clock type not found' )

C
C     Note: change of clock
C
      CLKID  =  1

      ET     =  1.D0

      CALL SCTY01 ( CLKID, CLKTYP )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCTY01: exercise pass 1 logic after error' )

C
C     Cause error:
C
C     Note: change of clock
C
      CLKID  =  1

      ET     =  1.D0

      CALL SCTY01 ( CLKID, CLKTYP )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


C
C     Check recovery.
C
      CLKID  =  -999

      CALL SCTY01 ( CLKID, CLKTYP )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'CLKTYP', CLKTYP, '=', 1, 0, OK )

C
C     Check use of database for second call using same clock,
C     with no kernel pool update.
C
      CLKID  =  -999

      CALL SCTY01 ( CLKID, CLKTYP )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'CLKTYP', CLKTYP, '=', 1, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCTY01: ZZSCAD01 error' )

C
C     Cause error:
C
C     Note: change of clock
C
      CLKID  =  -999

      ET     =  1.D0

      CALL DVPOOL ( 'SCLK01_MODULI_999')
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCTY01 ( CLKID, CLKTYP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Restore data.
C
      CALL LDPOOL ( SCLK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check recovery.
C
      CLKID  =  -999

      CALL SCTY01 ( CLKID, CLKTYP )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'CLKTYP', CLKTYP, '=', 1, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLI01: number of fields not found' )

      CLKID = 1

      ITEM = 'SCLK01_N_FIELDS'
      CALL SCLI01 ( ITEM, CLKID, 1, N, IVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLI01: output delimiter code not found' )

      CLKID = 1

      ITEM = 'SCLK01_OUTPUT_DELIM'
      CALL SCLI01 ( ITEM, CLKID, 1, N, IVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLI01: time system code not found' )

C
C     NOTE: time system code is optional. If not present, the number
C     of values returned should be zero. No error is signaled.
C
      CLKID = 1

      ITEM = 'SCLK01_TIME_SYSTEM'
      CALL SCLI01 ( ITEM, CLKID, 1, N, IVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 0, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLI01: clock data type not found' )

C
C     NOTE: SCLU01 doesn't have size or range checks for the clock
C     type.
C
      CLKID = 1

      ITEM = 'SCLK_DATA_TYPE'
      CALL SCLI01 ( ITEM, CLKID, 1, N, IVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLI01: data item has character type' )

      CLKID = 1

      ITEM = 'SCLK01_N_FIELDS'

      KVNAME = ITEM
      CALL SUFFIX ( '_-1', 0, KVNAME )

      CALL PCPOOL ( KVNAME, 1, 'XYZ' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLI01 ( ITEM, CLKID, 1, N, IVALS )
      CALL CHCKXC ( .TRUE., 'SPICE(BADKERNELVARTYPE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: coefficients not found' )

      CLKID = 1

      ITEM = 'SCLK01_COEFFICIENTS'
      CALL SCLD01 ( ITEM, CLKID, 1, N, COEFFS )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: partition starts not found' )

      CLKID = 1

      ITEM = 'SCLK_PARTITION_START'
      CALL SCLD01 ( ITEM, CLKID, 1, N, PSTART )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: partition stops not found' )

      CLKID = 1

      ITEM = 'SCLK_PARTITION_STOP'
      CALL SCLD01 ( ITEM, CLKID, 1, N, PSTOP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: moduli not found' )

      CLKID = 1

      ITEM = 'SCLK01_MODULI'
      CALL SCLD01 ( ITEM, CLKID, 1, N, MODULI )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: offsets not found' )

      CLKID = 1

      ITEM = 'SCLK01_OFFSETS'
      CALL SCLD01 ( ITEM, CLKID, 1, N, OFFSET )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: kernel ID not found' )

      CLKID = 1

      ITEM = 'SCLK01_KERNEL_ID'
      CALL SCLD01 ( ITEM, CLKID, 1, N, KERNID )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: data item has character type' )

      CLKID = 1

      ITEM = 'SCLK01_MODULI'

      KVNAME = ITEM
      CALL SUFFIX ( '_-1', 0, KVNAME )

      CALL PCPOOL ( KVNAME, 1, 'XYZ' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLD01 ( ITEM, CLKID, 1, N, KERNID )
      CALL CHCKXC ( .TRUE., 'SPICE(BADKERNELVARTYPE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLI01: output array size check' )

C
C     We need try only one kernel variable for this test.
C
      CLKID = -9

      ITEM   = 'SCLK01_N_FIELDS'

      CALL SCLI01 ( ITEM, CLKID, 0, N, IVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYTOOSMALL)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: output array size check' )

C
C     We need try only one kernel variable for this test.
C
      CLKID = -9

      ITEM   = 'SCLK01_COEFFICIENTS'

      CALL SCLD01 ( ITEM, CLKID, 0, N, COEFFS )
      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYTOOSMALL)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLI01: output delimiter kernel variable '
     .//           'has size > 1 where scalar is expected.' )

      CLKID = -9

      ET    = 1.D0

      ITEM   = 'SCLK01_OUTPUT_DELIM'

      KVNAME = ITEM
      CALL SUFFIX ( '_9', 0, KVNAME )

      IVALS(1) = 1
      IVALS(2) = 2

      CALL PIPOOL ( KVNAME, 2, IVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLI01 ( ITEM, CLKID, IVALSZ, N, IVALS )
      CALL CHCKXC ( .TRUE., 'SPICE(SIZEOUTOFRANGE)', OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLI01: field count kernel variable '
     .//           'has size > 1 where scalar is expected.' )

      CLKID = -9

      ET    = 1.D0

      ITEM   = 'SCLK01_N_FIELDS'

      KVNAME = ITEM
      CALL SUFFIX ( '_9', 0, KVNAME )

      IVALS(1) = 1
      IVALS(2) = 2

      CALL PIPOOL ( KVNAME, 2, IVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLI01 ( ITEM, CLKID, IVALSZ, N, IVALS )
      CALL CHCKXC ( .TRUE., 'SPICE(SIZEOUTOFRANGE)', OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLI01: time system kernel variable '
     .//           'has size > 1 where scalar is expected.' )

      CLKID = -9

      ET    = 1.D0

      ITEM   = 'SCLK01_TIME_SYSTEM'

      KVNAME = ITEM
      CALL SUFFIX ( '_9', 0, KVNAME )

      IVALS(1) = 1
      IVALS(2) = 2

      CALL PIPOOL ( KVNAME, 2, IVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLI01 ( ITEM, CLKID, IVALSZ, N, IVALS )
      CALL CHCKXC ( .TRUE., 'SPICE(SIZEOUTOFRANGE)', OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: kernel ID kernel variable '
     .//           'has size > 1 where scalar is expected.' )

      CLKID = -9

      ET    = 1.D0

      ITEM  = 'SCLK01_KERNEL_ID'

      KVNAME = ITEM
      CALL SUFFIX ( '_9', 0, KVNAME )

      DVALS(1) = 1.D0
      DVALS(2) = 2.D0

      CALL PDPOOL ( KVNAME, 2, DVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLD01 ( ITEM, CLKID, DVALSZ, N, DVALS )
      CALL CHCKXC ( .TRUE., 'SPICE(SIZEOUTOFRANGE)', OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: moduli kernel variable '
     .//           'has size > MXNFLD.' )

      CLKID = -9

      ET    = 1.D0

      ITEM  = 'SCLK01_MODULI'

      KVNAME = ITEM
      CALL SUFFIX ( '_9', 0, KVNAME )

      CALL FILLD  ( 10.D0, DVALSZ, DVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( KVNAME, DVALSZ, DVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLD01 ( ITEM, CLKID, DVALSZ, N, DVALS )
      CALL CHCKXC ( .TRUE., 'SPICE(SIZEOUTOFRANGE)', OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: offset kernel variable '
     .//           'has size > MXNFLD.' )

      CLKID = -9

      ET    = 1.D0

      ITEM  = 'SCLK01_OFFSETS'

      KVNAME = ITEM
      CALL SUFFIX ( '_9', 0, KVNAME )

      CALL FILLD  ( 10.D0, DVALSZ, DVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( KVNAME, DVALSZ, DVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLD01 ( ITEM, CLKID, DVALSZ, N, DVALS )
      CALL CHCKXC ( .TRUE., 'SPICE(SIZEOUTOFRANGE)', OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: partition start kernel variable '
     .//           'has size > MXPART.' )

      CLKID = -9

      ET    = 1.D0

      ITEM  = 'SCLK_PARTITION_START'

      KVNAME = ITEM
      CALL SUFFIX ( '_9', 0, KVNAME )

      CALL FILLD  ( 10.D0, MXPART+1, XSTART )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( KVNAME, MXPART+1, XSTART )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLD01 ( ITEM, CLKID, MXPART+1, N, PSTART )
      CALL CHCKXC ( .TRUE., 'SPICE(SIZEOUTOFRANGE)', OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: partition stop kernel variable '
     .//           'has size > MXPART.' )

      CLKID = -9

      ET    = 1.D0

      ITEM  = 'SCLK_PARTITION_END'

      KVNAME = ITEM
      CALL SUFFIX ( '_9', 0, KVNAME )

      CALL FILLD  ( 10.D0, MXPART+1, XSTOP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( KVNAME, MXPART+1, XSTOP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLD01 ( ITEM, CLKID, MXPART+1, N, PSTOP )
      CALL CHCKXC ( .TRUE., 'SPICE(SIZEOUTOFRANGE)', OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: coefficient kernel variable size check' )

      CLKID = 1

      ITEM   = 'SCLK01_COEFFICIENTS'

      KVNAME = ITEM
      CALL SUFFIX ( '_-1', 0, KVNAME )

      CALL CLEARD ( 3 * (MXCOEF+1), COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, COEFFS )

C
C     Try a coefficient set that's too small.
C
      CALL PDPOOL ( KVNAME, 1, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLD01 ( ITEM, CLKID, 3, N, COEFFS )
      CALL CHCKXC ( .TRUE., 'SPICE(SIZEOUTOFRANGE)', OK )

C
C     Try a coefficient set that's too large.
C
      CALL PDPOOL ( KVNAME, 3*(MXCOEF+1), COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLD01 ( ITEM, CLKID, 3*(MXCOEF+1), N, COEFFS )
      CALL CHCKXC ( .TRUE., 'SPICE(SIZEOUTOFRANGE)', OK )

C
C     Try a coefficient set that has size not a multiple of 3.
C
      CALL PDPOOL ( KVNAME, 5, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLD01 ( ITEM, CLKID, MXCOEF, N, COEFFS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLI01: delimiter code out of range' )

      CLKID = 1

      ITEM   = 'SCLK01_OUTPUT_DELIM'

      KVNAME = ITEM
      CALL SUFFIX ( '_-1', 0, KVNAME )

      IVAL = 0

      CALL PIPOOL ( KVNAME, 1, IVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLI01 ( ITEM, CLKID, 1, N, IVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      IVAL = NDELIM + 1

      CALL PIPOOL ( KVNAME, 1, IVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLI01 ( ITEM, CLKID, 1, N, IVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLI01: field count out of range' )

      CLKID = 1

      ITEM   = 'SCLK01_N_FIELDS'

      KVNAME = ITEM
      CALL SUFFIX ( '_-1', 0, KVNAME )

      IVAL = 0

      CALL PIPOOL ( KVNAME, 1, IVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLI01 ( ITEM, CLKID, 1, N, IVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      IVAL = MXNFLD + 1

      CALL PIPOOL ( KVNAME, 1, IVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLI01 ( ITEM, CLKID, 1, N, IVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLI01: time system code out of range' )


      CLKID = 1

      ITEM   = 'SCLK01_TIME_SYSTEM'

      KVNAME = ITEM
      CALL SUFFIX ( '_-1', 0, KVNAME )

      IVAL = 0

      CALL PIPOOL ( KVNAME, 1, IVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLI01 ( ITEM, CLKID, 1, N, IVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      IVAL = MXTSYS + 1

      CALL PIPOOL ( KVNAME, 1, IVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLI01 ( ITEM, CLKID, 1, N, IVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: offset kernel variable '
     .//           'has size not equal to field count.' )

      CLKID = -9

      ET    = 1.D0

      ITEM  = 'SCLK01_OFFSETS'

      KVNAME = ITEM
      CALL SUFFIX ( '_9', 0, KVNAME )

      CALL FILLD  ( 0.D0, DVALSZ, DVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( KVNAME, 1, DVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLD01 ( ITEM, CLKID, DVALSZ, N, DVALS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)', OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: offset kernel variable size comparison '
     .//           'fails due to missing field count variable.' )

      CLKID = -9

      ET    = 1.D0

      ITEM  = 'SCLK01_N_FIELDS'

      KVNAME = ITEM
      CALL SUFFIX ( '_9', 0, KVNAME )

      CALL DVPOOL ( KVNAME )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ITEM  = 'SCLK01_OFFSETS'

      KVNAME = ITEM
      CALL SUFFIX ( '_9', 0, KVNAME )

      CALL SCLD01 ( ITEM, CLKID, DVALSZ, N, DVALS )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: moduli kernel variable '
     .//           'has size not equal to field count.' )

      CLKID = -9

      ET    = 1.D0

      ITEM  = 'SCLK01_MODULI'

      KVNAME = ITEM
      CALL SUFFIX ( '_9', 0, KVNAME )

      CALL FILLD  ( 10.D0, DVALSZ, DVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( KVNAME, 1, DVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLD01 ( ITEM, CLKID, DVALSZ, N, DVALS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)', OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: moduli kernel variable size comparison '
     .//           'fails due to missing field count variable.' )

      CLKID = -9

      ET    = 1.D0

      ITEM  = 'SCLK01_N_FIELDS'

      KVNAME = ITEM
      CALL SUFFIX ( '_9', 0, KVNAME )

      CALL DVPOOL ( KVNAME )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ITEM  = 'SCLK01_MODULI'

      KVNAME = ITEM
      CALL SUFFIX ( '_9', 0, KVNAME )

      CALL SCLD01 ( ITEM, CLKID, DVALSZ, N, DVALS )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Restore kernel variables for this clock.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLD01: invalid moduli check' )


      CLKID = 1

      ITEM   = 'SCLK01_MODULI'

      KVNAME = ITEM
      CALL SUFFIX ( '_-1', 0, KVNAME )

      CALL VPACK ( 10.D0, 0.D0, 10.D0, MODULI )

      CALL PDPOOL ( KVNAME, 3, MODULI )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCLD01 ( ITEM, CLKID, 3, N, MODULI )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'All SCLK routines: exercise return on entry '
     .//           'logic' )

      SHRTMS = 'SPICE(TESTMESSAGE)'
C
C     Signal an error. We'll try to recover this short message
C     after calling a sequence of SCLK routines.
C
      CLKID  = -9
      CLKNAM = '-9 SCLK'
      CLKSTR = '1/000000000.0000'
      SCLKDP = 0.D0
      ET     = 0.D0

      CALL SIGERR ( SHRTMS )

      CALL SC01   ( CLKID,  CLKSTR, TICKS, SCLKDP, ET,
     .              NPARTS, PSTART, PSTOP, CLKTYP     )
      CALL SCDECD ( CLKID,  SCLKDP, CLKSTR )
      CALL SCE2C  ( CLKID,  ET,     SCLKDP )
      CALL SCE2S  ( CLKID,  ET,     CLKSTR )
      CALL SCE2T  ( CLKID,  ET,     SCLKDP )
      CALL SCEC01 ( CLKID,  ET,     SCLKDP )
      CALL SCENCD ( CLKID,  CLKSTR, SCLKDP )
      CALL SCET01 ( CLKID,  ET,     SCLKDP )
      CALL SCFM01 ( CLKID,  SCLKDP, CLKSTR )
      CALL SCFMT  ( CLKID,  SCLKDP, CLKSTR )
      CALL SCID2N ( CLKID,  CLKNAM, FOUND  )
      CALL SCLD01 ( ITEM,   CLKID,  1,  N, DVALS )
      CALL SCLI01 ( ITEM,   CLKID,  1,  N, IVALS )
      CALL SCLU01 ( 'XXX',  1,      10, N, FIELD, PSTART )
      CALL SCN2ID ( CLKNAM, CLKID,  FOUND  )
      CALL SCPARS ( CLKID,  CLKSTR, ERROR,  ERRMSG, SCLKDP )
      CALL SCPART ( CLKID,  NPARTS, PSTART, PSTOP )
      CALL SCPR01 ( CLKID,  NPARTS, PSTART, PSTOP )
      CALL SCPS01 ( CLKID,  CLKSTR, ERROR,  ERRMSG, SCLKDP )
      CALL SCS2E  ( CLKID,  CLKSTR, ET     )
      CALL SCT2E  ( CLKID,  SCLKDP, ET     )
      CALL SCTE01 ( CLKID,  SCLKDP, ET     )
      CALL SCTIKS ( CLKID,  CLKSTR, SCLKDP )
      CALL SCTK01 ( CLKID,  CLKSTR, SCLKDP )
      CALL SCTRAN ( 'XYZ',  CLKID,  FOUND )
      CALL SCTY01 ( CLKID,  I )
      I  = SCTYPE ( CLKID )


      CALL CHCKXC ( .TRUE., SHRTMS, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SC01: call to umbrella routine' )


C
C     Inputs and outputs are arbitrary in this call.
C
      CALL SC01 ( CLKID,  CLKSTR, TICKS, SCLKDP, ET,
     .            NPARTS, PSTART, PSTOP, CLKTYP     )

      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCLU01: call to umbrella routine' )

C
C     Inputs and outputs are arbitrary in this call.
C
      CALL SCLU01 (  'XXX',  1,  10,  N,  FIELD, PSTART )

      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SCTRAN: call to umbrella routine' )

C
C     Inputs and outputs are arbitrary in this call.
C
      CLKID  = 1
      CALL SCTRAN ( 'XYZ', CLKID, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )

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
