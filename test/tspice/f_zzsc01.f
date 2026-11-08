C$Procedure F_ZZSC01 ( SCLK type 01 database tests )

      SUBROUTINE F_ZZSC01 ( OK )

C$ Abstract
C
C     Exercise the SCLK type 1 database routines.
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
      INCLUDE 'zzctr.inc'
      INCLUDE 'zzsc01.inc'

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
C        ZZSCIN01
C        ZZSCUP01
C        ZZSCAD01
C
C     and entry points of
C
C        SC01
C
C     that use them.
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
C-    TSPICE Version 1.0.0 01-DEC-2020 (NJB)
C
C-&

C
C     SPICELIB functions
C
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
      PARAMETER           ( VTIGHT = 1.D-13 )

      INTEGER               DVALSZ
      PARAMETER           ( DVALSZ = MXNFLD + 1)

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NCOM
      PARAMETER           ( NCOM   = 3 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 40 )




C
C     Other parameters
C
      INTEGER               MAXITR
      PARAMETER           ( MAXITR = 10000 )

      INTEGER               NCLOCK
      PARAMETER           ( NCLOCK = 3 )

      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )

C
C     Local Variables
C
C
C     We use a longer, non-standard length for ABCORR because we
C     want to include embedded blanks for testing.
C
      CHARACTER*(TIMLEN)    CLKSTR
      CHARACTER*(LNSIZE)    COMS   ( LBCELL : NCOM )
      CHARACTER*(KVNMLN)    KVNAME
      CHARACTER*(LNSIZE)    LABEL
      CHARACTER*(SMSGLN)    SHRTMS
      CHARACTER*(TIMLEN)    XCLKST

C
C     The coefficient array must support a test in which too many
C     coefficients are provided.
C
      DOUBLE PRECISION      COEFFS ( 3, MXCOEF + 1 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DPBUFF ( DBUFSZ )
      DOUBLE PRECISION      DVALS  ( DVALSZ )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      MAJOR
      DOUBLE PRECISION      MAXMOD
      DOUBLE PRECISION      MODULI ( MXNFLD )
      DOUBLE PRECISION      OFFSET ( MXNFLD )
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XCLKDP
      DOUBLE PRECISION      XET
      DOUBLE PRECISION      XOFF   ( MXNFLD )
      DOUBLE PRECISION      XMOD   ( MXNFLD )
      DOUBLE PRECISION      XSTART ( MXPART + 1 )
      DOUBLE PRECISION      XSTOP  ( MXPART + 1 )
      DOUBLE PRECISION      XTICKS ( MAXITR, NCLOCK )

      INTEGER               CLKID
      INTEGER               CLKLST ( MXNCLK )
      INTEGER               CLKTYP
      INTEGER               CLOCKS ( NCLOCK )
      INTEGER               COFBAS
      INTEGER               DELCDE
      INTEGER               DPFREE
      INTEGER               DSTART
      INTEGER               ENDBAS
      INTEGER               HANDLE
      INTEGER               HDSCLK ( MXNCLK )
      INTEGER               I
      INTEGER               IFREE
      INTEGER               INTBUF ( IBUFSZ )
      INTEGER               ISTART
      INTEGER               J
      INTEGER               MODBAS
      INTEGER               NCOEFF
      INTEGER               NFIELD
      INTEGER               NITER
      INTEGER               NPARTS
      INTEGER               NREC
      INTEGER               NSCAVL
      INTEGER               OFFBAS
      INTEGER               POLCTR ( CTRSIZ )
      INTEGER               PRVSC
      INTEGER               SC
      INTEGER               SCBASE ( MXNCLK )
      INTEGER               SCLKAT
      INTEGER               SCPOOL ( LBSNGL : MXNCLK )
      INTEGER               STRBAS
      INTEGER               TIMSYS
      INTEGER               XTYPE
      INTEGER               XIBUFF ( MXNCLK )

      LOGICAL               UPDATE

C
C     Saved variables
C
      SAVE

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZSC01' )


C*****************************************************************
C
C     Normal cases
C
C*****************************************************************

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

      CALL FURNSH ( SCLK2 )
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

      CALL FURNSH ( SCLK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Tests of supporting private routines follow.
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Initialize the database.' )

      CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The previous SCLK should be zero.
C
      CALL CHCKSI ( 'PRVSC', PRVSC, '=', 0, 0, OK )

C
C     The integer and d.p. buffers should be empty: the first free
C     element of each should be at index 1.
C
      CALL CHCKSI ( 'DPFREE', DPFREE, '=', 1, 0, OK )
      CALL CHCKSI ( 'IFREE',  IFREE,  '=', 1, 0, OK )

C
C     The clock list should be empty.
C
      CALL FILLI  ( 0, MXNCLK, XIBUFF )
      CALL CHCKAI ( 'CLKLST',  CLKLST, '=', XIBUFF, MXNCLK, OK )

C
C     The array of collision list head nodes should be empty.
C
      CALL CHCKAI ( 'HDSCLK', HDSCLK, '=', XIBUFF, MXNCLK, OK )

C
C     The number of free nodes in the collision list pool should be
C     the pool size.
C
      CALL ZZHSIAVL ( SCPOOL, NSCAVL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NSCAVL', NSCAVL,  '=', MXNCLK, 0, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Add data for clock -9 to the database.' )

C
C     Do a conversion using clock -9. This will populate the database.
C
      SC = -9
      ET = 0.D0

      CALL SCE2C ( SC, ET, SCLKDP )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Initialize the pool counter.
C
      CALL ZZCTRUIN ( POLCTR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Look up information for clock -9.
C
      CALL ZZSCUP01 ( SC,     POLCTR, HDSCLK, SCPOOL, CLKLST,
     .                DPFREE, DPBUFF, IFREE,  INTBUF, SCBASE,
     .                PRVSC,  NFIELD, DELCDE, TIMSYS, NCOEFF,
     .                NPARTS, COFBAS, STRBAS, ENDBAS, MODBAS,
     .                OFFBAS                                  )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Check the number of partitions.
C
      CALL CHCKSI ( 'NPARTS', NPARTS,  '=', 1, 0, OK )

C
C     Check the partitions.
C
      XSTART(1) = 0.D0

      CALL CHCKAD ( 'STARTS', DPBUFF(STRBAS+1), '~',
     .              XSTART,   NPARTS,           VTIGHT, OK )

      XSTOP(1)  = 1.D14

      CALL CHCKAD ( 'STOPS', DPBUFF(ENDBAS+1), '~',
     .              XSTOP,   NPARTS,            VTIGHT, OK )

C
C     Check the number of individual SCLK coefficients.
C
      CALL CHCKSI ( 'NCOEFF', NCOEFF,  '=', 3, 0, OK )

C
C     Check the  SCLK coefficients.
C
      COEFFS(1,1) = 0.D0

      CALL STR2ET ( '1-JAN-1980 TDB', COEFFS(2,1) )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL ZZPCTRCK ( POLCTR, UPDATE )

      COEFFS(3,1) = 1.D0

      CALL CHCKAD ( 'COEFFS', DPBUFF(COFBAS+1), '~/', COEFFS,
     .              3, VTIGHT, OK )

C
C     Check the number of clock fields.
C
      CALL CHCKSI ( 'NFIELD', NFIELD,  '=', 2, 0, OK )


C
C     Check the clock moduli.
C
      XMOD(1) = 1.D9
      XMOD(2) = 10000.D0

      CALL CHCKAD ( 'MODULI', DPBUFF(MODBAS+1),
     .              '=',      XMOD, NFIELD, 0.D0, OK )

C
C     Check the clock offsets.
C
      XOFF(1) = 0.D0
      XOFF(2) = 0.D0

      CALL CHCKAD( 'OFFSETS', DPBUFF(OFFBAS+1),
     .             '=',       XOFF, NFIELD, 0.D0, OK )

C
C     Check the time system code.
C
      CALL CHCKSI ( 'TIMSYS', TIMSYS, '=', 1, 0, OK )

C
C     Check the delimiter code.
C
      CALL CHCKSI ( 'DELCDE', DELCDE, '=', 1, 0, OK )

C
C     Check the first free integer and d.p. indices in the respective
C     data buffers.
C
C     All clocks use 10 integer buffer entries.
C
      CALL CHCKSI ( 'IFREE',  IFREE,  '=', 11, 0, OK )

      CALL CHCKSI ( 'DPFREE', DPFREE, '=',
     .              NCOEFF + (2*NPARTS) + (2*NFIELD) + 1,
     .              0, OK )

C
C     Save the free values for the next test case.
C
      ISTART = IFREE
      DSTART = DPFREE

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Add data for clock -99 to the database.' )

C
C     Do a conversion using clock -99. This will populate the database.
C
      SC = -99
      ET = 0.D0

      CALL SCE2C ( SC, ET, SCLKDP )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Look up information for clock -99.
C
      CALL ZZSCUP01 ( SC,     POLCTR, HDSCLK, SCPOOL, CLKLST,
     .                DPFREE, DPBUFF, IFREE,  INTBUF, SCBASE,
     .                PRVSC,  NFIELD, DELCDE, TIMSYS, NCOEFF,
     .                NPARTS, COFBAS, STRBAS, ENDBAS, MODBAS,
     .                OFFBAS                                  )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Check the number of partitions.
C
      CALL CHCKSI ( 'NPARTS', NPARTS, '=', 1001, 0, OK )

C
C     Check the partitions.
C
      DO I = 1, NPARTS
         XSTART(I) = DBLE(I)
         XSTOP (I) = 720000.D0 + I
      END DO

      CALL CHCKAD ( 'STARTS',  DPBUFF(STRBAS+1), '=',
     .               XSTART,   NPARTS,           0.D0, OK )

      CALL CHCKAD ( 'STOPS',   DPBUFF(ENDBAS+1), '=',
     .               XSTOP,    NPARTS,           0.D0, OK )


C
C     Check the number of individual SCLK coefficients.
C
      CALL CHCKSI ( 'NCOEFF', NCOEFF, '=', 12003, 0, OK )

C
C     Check the clock moduli.
C
      XMOD(1) = 1.D7
      XMOD(2) = 10.D0
      XMOD(3) = 9.D0
      XMOD(4) = 8.D0

      CALL CHCKAD ( 'MODULI', DPBUFF(MODBAS+1),
     .              '=',      XMOD, NFIELD, 0.D0, OK )
C
C     Check the  SCLK coefficients.
C
      MAJOR = 1.D0

      DO I = 2, 4
         MAJOR = MAJOR * XMOD(I)
      END DO

      DELTA = 1.D-9

      DO I = 1, NCOEFF/3

         COEFFS(1,I) = (I-1) * 180000.D0

         COEFFS(2,I) = COEFFS(1,I) * 2 / MAJOR

         COEFFS(3,I) = 2.D0  +  I*DELTA

      END DO

      CALL CHCKAD ( 'COEFFS', DPBUFF(COFBAS+1), '~/', COEFFS,
     .               NCOEFF,  VTIGHT,  OK )

C
C     Check the number of clock fields.
C
      CALL CHCKSI ( 'NFIELD', NFIELD,  '=', 4, 0, OK )

C
C     Check the clock offsets.
C
      CALL CLEARD ( NFIELD, XOFF )

      CALL CHCKAD( 'OFFSETS', DPBUFF(OFFBAS+1),
     .             '=',       XOFF, NFIELD, 0.D0, OK )

C
C     Check the time system code.
C
      CALL CHCKSI ( 'TIMSYS', TIMSYS, '=', 1, 0, OK )

C
C     Check the delimiter code.
C
      CALL CHCKSI ( 'DELCDE', DELCDE, '=', 2, 0, OK )

C
C     Check the first free integer and d.p. indices in the respective
C     data buffers.
C
C     All clocks use 10 integer buffer entries.
C
      CALL CHCKSI ( 'IFREE',  IFREE,  '=', ISTART+10, 0, OK )

      CALL CHCKSI ( 'DPFREE', DPFREE, '=',
     .              DSTART + NCOEFF + (2*NPARTS) + (2*NFIELD),
     .              0, OK )

C
C     Save the free values for the next test case.
C
      ISTART = IFREE
      DSTART = DPFREE


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Add data for clock -999 to the database.' )

C
C     Do a conversion using clock -999. This will populate the
C     database.
C
      SC = -999
      ET = 0.D0

      CALL ZZPCTRCK ( POLCTR, UPDATE )

      CALL SCE2C ( SC, ET, SCLKDP )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Look up information for clock -999.
C
      CALL ZZPCTRCK ( POLCTR, UPDATE )

      CALL ZZSCUP01 ( SC,     POLCTR, HDSCLK, SCPOOL, CLKLST,
     .                DPFREE, DPBUFF, IFREE,  INTBUF, SCBASE,
     .                PRVSC,  NFIELD, DELCDE, TIMSYS, NCOEFF,
     .                NPARTS, COFBAS, STRBAS, ENDBAS, MODBAS,
     .                OFFBAS                                  )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Check the number of partitions.
C
      CALL CHCKSI ( 'NPARTS', NPARTS,  '=', 4, 0, OK )

C
C     Check the partitions.
C
      DO I = 1, NPARTS
         XSTART(I) = DBLE(I)
         XSTOP (I) = 720000.D0 + I
      END DO
C
C     Make the second partition start with a forward jump.
C
      XSTART(2) = XSTART(2) + 1.D6
      XSTOP (2) = XSTOP (2) + 1.D6

      CALL CHCKAD ( 'STARTS',  DPBUFF(STRBAS+1), '~/',
     .               XSTART,   NPARTS,           VTIGHT, OK )

      CALL CHCKAD ( 'STOPS',   DPBUFF(ENDBAS+1), '~/',
     .               XSTOP,    NPARTS,           VTIGHT, OK )

C
C     Check the number of individual SCLK coefficients.
C
      CALL CHCKSI ( 'NCOEFF', NCOEFF, '=', 48, 0, OK )

C
C     Check the clock moduli.
C
      XMOD(1) = 1.D7
      XMOD(2) = 10.D0
      XMOD(3) = 9.D0
      XMOD(4) = 8.D0

      CALL CHCKAD ( 'MODULI', DPBUFF(MODBAS+1),
     .              '=',      XMOD, NFIELD, 0.D0, OK )
C
C     Check the  SCLK coefficients.
C
      MAJOR = 1.D0

      DO I = 2, 4
         MAJOR = MAJOR * XMOD(I)
      END DO

      DELTA = 1.D-9

      DO I = 1, NCOEFF/3

         COEFFS(1,I) = (I-1) * 180000.D0

         COEFFS(2,I) = COEFFS(1,I) * 2 / MAJOR

         COEFFS(3,I) = 2.D0  +  I*DELTA

      END DO

      CALL CHCKAD ( 'COEFFS', DPBUFF(COFBAS+1), '~/', COEFFS,
     .               NCOEFF,  VTIGHT,  OK )

C
C     Check the number of clock fields.
C
      CALL CHCKSI ( 'NFIELD', NFIELD, '=', 4, 0, OK )

C
C     Check the clock offsets.
C
      XOFF(1) = 4
      XOFF(2) = 3
      XOFF(3) = 2
      XOFF(4) = 1

      CALL CHCKAD( 'OFFSETS', DPBUFF(OFFBAS+1),
     .             '=',       XOFF, NFIELD, 0.D0, OK )
C
C     Check the time system code. This one is TDT.
C
      CALL CHCKSI ( 'TIMSYS', TIMSYS, '=', 2, 0, OK )

C
C     Check the delimiter code.
C
      CALL CHCKSI ( 'DELCDE', DELCDE, '=', 2, 0, OK )

C
C     Check the first free integer and d.p. indices in the respective
C     data buffers.
C
C     All clocks use 10 integer buffer entries.
C
      CALL CHCKSI ( 'IFREE',  IFREE,  '=', ISTART+10, 0, OK )

      CALL CHCKSI ( 'DPFREE', DPFREE, '=',
     .              DSTART + NCOEFF + (2*NPARTS) + (2*NFIELD),
     .              0, OK )

C
C     Interleaved clock reference tests follow.
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Interleave calls to SCE2C and SCT2E using '
     .//           'clocks -9, -99, and -999.' )

C
C     Use an ET value common to the time ranges of all three clocks.
C
C        - The time range for clock -9 is from
C
C                  1980 JAN 1, 00:00:00.000 (ET)
C             to   2011 SEP 9, 01:46:40.000 (ET)
C
C        - The ET range for the coefficient records of clock -99 is
C          from 0 to 2.e6 seconds past J2000 TDB.
C
C        - The ET range for the coefficient records of clock -999 is
C          from 0 to 7500 seconds past J2000 TDB.
C
      ET0   = 3600.0D0
      STEP  = 1.D-1
      NITER = MAXITR

C
C     Compute expected results using non-interleaved calls.
C
      CLOCKS(1) =   -9
      CLOCKS(2) =  -99
      CLOCKS(3) = -999

      DO I = 1, NCLOCK

         DO J = 1, NITER

            ET = ET0 + (J-1)*STEP

            CALL SCE2C( CLOCKS(I), ET, XTICKS(J,I) )
            CALL CHCKXC( .FALSE., ' ', OK )

         END DO

      END DO

C
C     Now perform interleaved computation.
C
      TOL = VTIGHT

      DO J = 1, NITER

         XET = ET0 + (J-1)*STEP

C
C        Halfway through, touch the kernel pool. Computations should
C        not be affected.
C
         IF ( J .EQ. NITER/2 ) THEN
            CALL PDPOOL( 'XYZ',   1,   1.D0 )
            CALL CHCKXC( .FALSE., ' ', OK   )
         END IF

         DO I = 1, NCLOCK

            CALL SCE2C( CLOCKS(I), XET, SCLKDP )
            CALL CHCKXC( .FALSE., ' ', OK )

            IF ( ABS( SCLKDP - XTICKS(J,I)) .GT. TOL ) THEN
C
C              This code is unreachable unless the SCE2C call above
C              failed.
C
               LABEL = 'SCLKDP(#,#)'
               CALL REPMI( LABEL, '#', J, LABEL )
               CALL REPMI( LABEL, '#', I, LABEL )

               CALL CHCKSD( LABEL, SCLKDP, '~/', XTICKS(J,I), TOL, OK )

            END IF

            CALL SCT2E ( CLOCKS(I), SCLKDP, ET )
            CALL CHCKXC( .FALSE., ' ', OK )

            IF ( ABS( ET - XET ) .GT. TOL ) THEN
C
C              This code is unreachable unless the SCT2E call above
C              failed.
C
               LABEL = 'ET(#,#)'
               CALL REPMI( LABEL, '#', J, LABEL )
               CALL REPMI( LABEL, '#', I, LABEL )

               CALL CHCKSD( LABEL, SCLKDP, '~/', XTICKS(J,I), TOL, OK )

            END IF

         END DO

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Interleave calls to SCE2T and SCT2E using '
     .//           'clocks -9, -99, and -999.' )

C
C     We'll use integral tick values produced by rounding the values
C     from the previous test case.
C
      DO I = 1, NCLOCK

         DO J = 1, NITER
            XTICKS(J,I) = ANINT( XTICKS(J,I) )
         END DO

      END DO

C
C     Now perform interleaved computation.
C
C     We expect exact tick matches.
C
      TOL = 0.D0

      DO J = 1, NITER
C
C        Halfway through, touch the kernel pool. Computations should
C        not be affected.
C
         IF ( J .EQ. NITER/2 ) THEN
            CALL PDPOOL( 'XYZ',   1,   1.D0 )
            CALL CHCKXC( .FALSE., ' ', OK   )
         END IF

         DO I = 1, NCLOCK

            CALL SCT2E( CLOCKS(I), XTICKS(J,I), ET )
            CALL CHCKXC( .FALSE., ' ', OK )

            CALL SCE2T( CLOCKS(I), ET, SCLKDP )

            IF ( SCLKDP - XTICKS(J,I) .NE. 0.D0 ) THEN
C
C              This code is unreachable unless the SCE2T call above
C              failed.
C
               LABEL = 'SCLKDP(#,#)'
               CALL REPMI( LABEL, '#', J, LABEL )
               CALL REPMI( LABEL, '#', I, LABEL )

               CALL CHCKSD( LABEL, SCLKDP, '~/', XTICKS(J,I), TOL, OK )

            END IF

         END DO

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Interleave calls to SCDECD and SCENCD using '
     .//           'clocks -9, -99, and -999.' )

C
C     Use the integral tick values for each clock produced by the
C     previous test case.
C
C     We expect exact tick matches.
C
      TOL = 0.D0

      DO J = 1, NITER
C
C        Halfway through, touch the kernel pool. Computations should
C        not be affected.
C
         IF ( J .EQ. NITER/2 ) THEN
            CALL PDPOOL( 'XYZ',   1,   1.D0 )
            CALL CHCKXC( .FALSE., ' ', OK   )
         END IF

         DO I = 1, NCLOCK

            CALL SCDECD( CLOCKS(I), XTICKS(J,I), CLKSTR )
            CALL CHCKXC( .FALSE., ' ', OK )

            CALL SCENCD( CLOCKS(I), CLKSTR, SCLKDP )
            CALL CHCKXC( .FALSE., ' ', OK )

            IF ( ABS( SCLKDP - XTICKS(J,I) ) .GT. TOL ) THEN
C
C              This code is unreachable unless the SCENCD or SCDECD
C              calls above failed.
C
               LABEL = 'TICKS(#,#)'
               CALL REPMI( LABEL, '#', J, LABEL )
               CALL REPMI( LABEL, '#', I, LABEL )

               CALL CHCKSD( LABEL, SCLKDP, '~/', XTICKS(J,I), TOL, OK )

            END IF

         END DO

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Interleave calls to SCS2E and SCE2S using '
     .//           'clocks -9, -99, and -999.' )

C
C     Use the integral tick values for each clock produced by the
C     previous test case.
C
C     We expect exact tick matches.
C
      TOL = 0.D0

      DO J = 1, NITER
C
C        Halfway through, touch the kernel pool. Computations should
C        not be affected.
C
         IF ( J .EQ. NITER/2 ) THEN
            CALL PDPOOL( 'XYZ',   1,   1.D0 )
            CALL CHCKXC( .FALSE., ' ', OK   )
         END IF

         DO I = 1, NCLOCK

            CALL SCDECD( CLOCKS(I), XTICKS(J,I), CLKSTR )
            CALL CHCKXC( .FALSE., ' ', OK )

            CALL SCS2E( CLOCKS(I), CLKSTR, ET )
            CALL CHCKXC( .FALSE., ' ', OK )

            CALL SCE2S( CLOCKS(I), ET, CLKSTR )
            CALL CHCKXC( .FALSE., ' ', OK )

            CALL SCENCD( CLOCKS(I), CLKSTR, SCLKDP )
            CALL CHCKXC( .FALSE., ' ', OK )


            IF ( ABS( SCLKDP - XTICKS(J,I) ) .GT. TOL ) THEN
C
C              This code is unreachable unless the sequence of calls
C              above failed.
C
               LABEL = 'TICKS(#,#)'
               CALL REPMI( LABEL, '#', J, LABEL )
               CALL REPMI( LABEL, '#', I, LABEL )

               CALL CHCKSD( LABEL, SCLKDP, '~/', XTICKS(J,I), TOL, OK )

            END IF

         END DO

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Buffer and use the maximum number of clocks.' )

C
C     The kernel pool data additions will initialize the database.
C

      DO I = 1, MXNCLK

         SC = -10000 * I

         KVNAME = 'SCLK_DATA_TYPE_#'
         CALL REPMI( KVNAME, '#', -SC, KVNAME )
         CALL CHCKXC( .FALSE., ' ', OK   )

         CALL PIPOOL ( KVNAME, 1, 1 )
         CALL CHCKXC( .FALSE., ' ', OK   )


         KVNAME = 'SCLK01_TIME_SYSTEM_#'
         CALL REPMI( KVNAME, '#', -SC, KVNAME )
         CALL CHCKXC( .FALSE., ' ', OK   )

         CALL PIPOOL ( KVNAME, 1, TDB )
         CALL CHCKXC( .FALSE., ' ', OK   )


         KVNAME = 'SCLK01_N_FIELDS_#'
         CALL REPMI( KVNAME, '#', -SC, KVNAME )
         CALL CHCKXC( .FALSE., ' ', OK   )

         CALL PIPOOL ( KVNAME, 1, 1 )
         CALL CHCKXC( .FALSE., ' ', OK   )


         MAXMOD = 1.D6

         KVNAME = 'SCLK01_MODULI_#'
         CALL REPMI( KVNAME, '#', -SC, KVNAME )
         CALL CHCKXC( .FALSE., ' ', OK   )

         CALL PDPOOL ( KVNAME, 1, MAXMOD )
         CALL CHCKXC( .FALSE., ' ', OK   )


         KVNAME = 'SCLK01_OFFSETS_#'
         CALL REPMI( KVNAME, '#', -SC, KVNAME )
         CALL CHCKXC( .FALSE., ' ', OK   )

         CALL PDPOOL ( KVNAME, 1, 0.D0 )
         CALL CHCKXC( .FALSE., ' ', OK   )


         KVNAME = 'SCLK01_OUTPUT_DELIM_#'
         CALL REPMI( KVNAME, '#', -SC, KVNAME )
         CALL CHCKXC( .FALSE., ' ', OK   )

         CALL PIPOOL ( KVNAME, 1, 1 )
         CALL CHCKXC( .FALSE., ' ', OK   )


         KVNAME = 'SCLK_PARTITION_START_#'
         CALL REPMI( KVNAME, '#', -SC, KVNAME )
         CALL CHCKXC( .FALSE., ' ', OK   )

         CALL PDPOOL ( KVNAME, 1, DBLE(I) )
         CALL CHCKXC( .FALSE., ' ', OK   )


         KVNAME = 'SCLK_PARTITION_END_#'
         CALL REPMI( KVNAME, '#', -SC, KVNAME )
         CALL CHCKXC( .FALSE., ' ', OK   )

         CALL PDPOOL ( KVNAME, 1, MAXMOD )
         CALL CHCKXC( .FALSE., ' ', OK   )


         KVNAME = 'SCLK01_COEFFICIENTS_#'
         CALL REPMI( KVNAME, '#', -SC, KVNAME )
         CALL CHCKXC( .FALSE., ' ', OK   )

         COEFFS(1,1) = 0.D0
         COEFFS(2,1) = DBLE(I)
         COEFFS(3,1) = 2.D0 * I

         CALL PDPOOL ( KVNAME, 3, COEFFS )
         CALL CHCKXC( .FALSE., ' ', OK   )

      END DO

C
C     Populate the database with data for each clock.
C
      DO I = 1, MXNCLK

         SC = -10000 * I

         CALL SCT2E ( SC, 0.D0, ET )
         CALL CHCKXC( .FALSE., ' ', OK   )

      END DO

C
C     Perform a sequence of time conversions for each clock.
C
      NITER = 10

      DO I = 1, NITER

         XCLKDP = DBLE(I)

         DO J = 1, MXNCLK

            SC = -10000 * I

            CALL SCDECD( SC, XCLKDP, CLKSTR )
            CALL CHCKXC( .FALSE., ' ', OK   )

            CALL SCS2E ( SC, CLKSTR,   ET   )
            CALL CHCKXC( .FALSE., ' ', OK   )

            CALL SCE2T ( SC, ET,   SCLKDP   )
            CALL CHCKXC( .FALSE., ' ', OK   )

            CALL CHCKSD( 'SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

         END DO

      END DO

C
C     Leave the database as is to simplify the following test case.
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Draw in data for another clock, when data for '
     .//           'the maximum number of clocks are already '
     .//           'in the database.' )

      SC = -9

      CALL SCDECD( SC, XCLKDP, CLKSTR )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL SCS2E ( SC, CLKSTR,   ET   )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL SCE2T ( SC, ET,   SCLKDP   )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL CHCKSD( 'SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Buffer and use data for a clock using more '
     .//           'than half the maximum number of d.p. values.' )

C
C     This is a simple test case for a clock with a large number of
C     coefficients. See the next test case for a more comprehensive,
C     but more complicated clock specification.
C
C     Use the first clock from the previous test case. Just
C     add a bunch of new coefficient records to its set.
C
      SC   = -10000

      NREC = MXCOEF/2 + 1

      DO I = 1, NREC

         COEFFS(1,I) = DBLE(I-1)
         COEFFS(2,I) = DBLE(I-1) * 2
         COEFFS(3,I) = 0.5D0

      END DO

      NCOEFF = 3 * NREC

      KVNAME = 'SCLK01_COEFFICIENTS_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PDPOOL ( KVNAME, NCOEFF, COEFFS )
      CALL CHCKXC( .FALSE., ' ', OK   )

C
C     Perform a sequence of time conversions for the clock.
C
      NITER = 10

      DO I = 1, NITER

         XCLKDP = DBLE(I)

         CALL SCDECD( SC, XCLKDP, CLKSTR )
         CALL CHCKXC( .FALSE., ' ', OK   )

         CALL SCS2E ( SC, CLKSTR,   ET   )
         CALL CHCKXC( .FALSE., ' ', OK   )

         CALL SCE2T ( SC, ET,   SCLKDP   )
         CALL CHCKXC( .FALSE., ' ', OK   )

         CALL CHCKSD( 'SCLKDP', SCLKDP, '=', XCLKDP, 0.D0, OK )

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Buffer and use data for clock that has the '
     .//           'maximum number of coefficients, partitions, '
     .//           'and fields.' )

C
C     Make sure we don't fill up the kernel pool.
C
      CALL KCLEAR
      CALL CHCKXC( .FALSE., ' ', OK   )

      SC   = -30000

C
C     Insert the clock definition into the kernel pool.
C
      KVNAME = 'SCLK_DATA_TYPE_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PIPOOL ( KVNAME, 1, 1 )
      CALL CHCKXC( .FALSE., ' ', OK   )

      KVNAME = 'SCLK01_TIME_SYSTEM_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PIPOOL ( KVNAME, 1, TDB )
      CALL CHCKXC( .FALSE., ' ', OK   )

C
C     The clock has 10 fields, each with modulus 2.
C
      KVNAME = 'SCLK01_N_FIELDS_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )


      NFIELD = 10
      CALL PIPOOL ( KVNAME, 1, NFIELD )
      CALL CHCKXC( .FALSE., ' ', OK   )


      KVNAME = 'SCLK01_MODULI_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL FILLD ( 2.D0, NFIELD-2, XMOD(3) )
      XMOD(1) = 10.D0
      XMOD(2) =  4.D0


      CALL PDPOOL ( KVNAME, NFIELD, XMOD )
      CALL CHCKXC( .FALSE., ' ', OK   )

C
C     The offsets are all zero.
C
      KVNAME = 'SCLK01_OFFSETS_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL CLEARD ( NFIELD, XOFF )

      CALL PDPOOL ( KVNAME, NFIELD, XOFF )
      CALL CHCKXC( .FALSE., ' ', OK      )


      KVNAME = 'SCLK01_OUTPUT_DELIM_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

C
C     Use the delimiter ':'
C
      CALL PIPOOL ( KVNAME, 1, 2 )
      CALL CHCKXC( .FALSE., ' ', OK   )

C
C     Create new coefficient records for this clock.
C
      NREC = MXCOEF

C
C     Coefficient records are 1024 ticks apart on the SCLK time scale.
C     Nominal tick duration is 1/32 of a second. Major counts are
C     2**10 ticks, or 32 seconds.
C
      DO I = 1, NREC

         COEFFS(1,I) = DBLE(I-1) * 1024.D0
         COEFFS(2,I) = DBLE(I-1) *   32.D0
         COEFFS(3,I) = 32.D0

      END DO

      NCOEFF = 3 * NREC

      KVNAME = 'SCLK01_COEFFICIENTS_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PDPOOL ( KVNAME, NCOEFF, COEFFS )
      CALL CHCKXC( .FALSE., ' ', OK   )

C
C     Create partition boundaries. Each partition will have just
C     10240 ticks. The last partition will have an end time past
C     the epoch of the last record.
C
      NPARTS = 9999

      DO I = 1, NPARTS

         XSTART(I) =     0.D0
         XSTOP(I)  = 10240.D0

      END DO

      KVNAME = 'SCLK_PARTITION_START_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PDPOOL ( KVNAME, NPARTS, XSTART )
      CALL CHCKXC( .FALSE., ' ', OK   )


      KVNAME = 'SCLK_PARTITION_END_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PDPOOL ( KVNAME, NPARTS, XSTOP )
      CALL CHCKXC( .FALSE., ' ', OK   )

C
C     Perform a sequence of time conversions for the clock.
C
      CLKSTR = '9999/9:3:1:1:1:1:1:1:1:1'

      CALL SCENCD ( SC, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'SCLKDP (A)', SCLKDP, '=',
     .              DBLE( (NPARTS-1)*10240 + 10239 ), 0.D0, OK )

      XCLKST = CLKSTR
      CALL SCDECD ( SC, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSC ( 'CLKSTR (A)', CLKSTR, '=', XCLKST, OK )

      CALL SCS2E  ( SC, CLKSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'ET (A)', ET, '~/', SCLKDP/32, VTIGHT, OK )

      XCLKDP = SCLKDP
      CALL SCE2C  ( SC, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'SCLKDP (AC)', SCLKDP, '~/', XCLKDP, VTIGHT, OK )

      CALL SCE2T  ( SC, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'SCLKDP (AT)', SCLKDP, '~/', XCLKDP, VTIGHT, OK )


      CLKSTR = '1/0:0:0:0:0:0:0:0:0:0'
      CALL SCENCD ( SC, CLKSTR, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'SCLKDP (B)', SCLKDP, '=', 0.D0, 0.D0, OK )

      XCLKST = CLKSTR
      CALL SCDECD ( SC, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSC ( 'CLKSTR (B)', CLKSTR, '=', XCLKST, OK )

      CALL SCS2E ( SC, CLKSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'ET (B)', ET, '~/', SCLKDP/32, VTIGHT, OK )

      XCLKDP = SCLKDP
      CALL SCE2C  ( SC, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'SCLKDP (BC)', SCLKDP, '~/', XCLKDP, VTIGHT, OK )

      CALL SCE2T  ( SC, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'SCLKDP (BT)', SCLKDP, '~/', XCLKDP, VTIGHT, OK )


      CLKSTR = '1/9:3:1:1:1:1:1:1:1:1'
      CALL SCENCD ( SC, CLKSTR, SCLKDP )
      CALL CHCKXC( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'SCLKDP (C)', SCLKDP, '~/', 10239.D0, VTIGHT, OK )

      XCLKST = CLKSTR
      CALL SCDECD ( SC, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSC ( 'CLKSTR (C)', CLKSTR, '=', XCLKST, OK )

      CALL SCS2E ( SC, CLKSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'ET (C)', ET, '~/', SCLKDP/32, VTIGHT, OK )

      XCLKDP = SCLKDP
      CALL SCE2C  ( SC, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'SCLKDP (CC)', SCLKDP, '~/', XCLKDP, VTIGHT, OK )

      CALL SCE2T  ( SC, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'SCLKDP (CT)', SCLKDP, '~/', XCLKDP, VTIGHT, OK )


      CLKSTR = '2/0:0:0:0:0:0:0:0:0:0'
      CALL SCENCD ( SC, CLKSTR, SCLKDP )
      CALL CHCKXC( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'SCLKDP (D)', SCLKDP, '~/', 10240.D0, VTIGHT, OK )

      XCLKST = CLKSTR
      CALL SCDECD ( SC, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSC ( 'CLKSTR (D)', CLKSTR, '=', XCLKST, OK )

      CALL SCS2E ( SC, CLKSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'ET (D)', ET, '~/', SCLKDP/32, VTIGHT, OK )

      XCLKDP = SCLKDP
      CALL SCE2C  ( SC, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'SCLKDP (DC)', SCLKDP, '~/', XCLKDP, VTIGHT, OK )

      CALL SCE2T  ( SC, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'SCLKDP (DT)', SCLKDP, '~/', XCLKDP, VTIGHT, OK )


      CLKSTR = '9999/0:0:0:0:0:0:0:0:0:0'
      CALL SCENCD ( SC, CLKSTR, SCLKDP )
      CALL CHCKXC( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'SCLKDP (E)', SCLKDP, '~/',
     .               9998 * 10240.D0, VTIGHT, OK )

      XCLKST = CLKSTR
      CALL SCDECD ( SC, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSC ( 'CLKSTR (E)', CLKSTR, '=', XCLKST, OK )

      CALL SCS2E ( SC, CLKSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'ET (E)', ET, '~/', SCLKDP/32, VTIGHT, OK )

      XCLKDP = SCLKDP
      CALL SCE2C  ( SC, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'SCLKDP (EC)', SCLKDP, '~/', XCLKDP, VTIGHT, OK )

      CALL SCE2T  ( SC, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'SCLKDP (ET)', SCLKDP, '~/', XCLKDP, VTIGHT, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Force ZZSCAD01 to make room in the d.p. buffer.' )

      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK   )

C
C     Populate the database for the "big" clock -30000.
C
      CLKSTR = '1/9:3:1:1:1:1:1:1:1:1'
      CALL SCENCD ( SC, CLKSTR, SCLKDP )
      CALL CHCKXC( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'SCLKDP (C)', SCLKDP, '~/', 10239.D0, VTIGHT, OK )

      XCLKST = CLKSTR
      CALL SCDECD ( SC, SCLKDP, CLKSTR )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSC ( 'CLKSTR (C)', CLKSTR, '=', XCLKST, OK )

      CALL SCS2E ( SC, CLKSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK   )
      CALL CHCKSD ( 'ET (C)', ET, '~/', SCLKDP/32, VTIGHT, OK )

C
C     Now do a time conversion for clock -99. The d.p. buffer will
C     need to be emptied in order to store data for this clock.
C
      SC = -99

      ET = 1.D0
      CALL SCE2C ( SC, ET, SCLKDP )
      CALL CHCKXC ( .FALSE., ' ', OK   )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Fetch type of non-type-1 SCLK.' )

      SC = -999999

      KVNAME = 'SCLK_DATA_TYPE_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      XTYPE = 7

      CALL PIPOOL ( KVNAME, 1, XTYPE )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CLKTYP = SCTYPE( SC )

      CALL CHCKSI( 'CLKTYP', CLKTYP, '=', XTYPE, 1, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZSCAD01: use a clock data set that lacks a time '
     .//           'system kernel variable. This is not an '
     .//           'error case.' )

C
C     Reload LSK to support STR2ET call.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The time system defaults to TDB. Use a clock for which TDB
C     is the actual associated time system.
C
      CALL LDPOOL ( SCLK )

      SC = -9

      KVNAME = 'SCLK01_TIME_SYSTEM_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL DVPOOL ( KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

C
C     Do a conversion that requires the associated time system.
C
      CALL STR2ET ( '1980 JAN 1 00:00:01 TDB', XET )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CLKSTR = '1/000000001:0000'

      CALL SCS2E ( SC, CLKSTR, ET )
      CALL CHCKXC( .FALSE., ' ', OK   )

      TOL = VTIGHT

      CALL CHCKSD ( 'ET', ET, '~/', XET, TOL, OK )

C
C     Restore the original clock data.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC( .FALSE., ' ', OK   )

C*****************************************************************
C
C     Error cases
C
C*****************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to use SCLK with too many coefficients.' )

C
C     Use the first clock from the previous test case.  We'll need
C     to clear the kernel pool so *it* doesn't overflow.
C
      CALL KCLEAR

      SC   = -20000

      NREC = MXCOEF+1

      DO I = 1, NREC

         COEFFS(1,I) = DBLE(I-1)
         COEFFS(2,I) = DBLE(I-1) * 4
         COEFFS(3,I) = 0.25D0

      END DO

      NCOEFF = 3 * NREC

      KVNAME = 'SCLK01_COEFFICIENTS_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PDPOOL ( KVNAME, NCOEFF, COEFFS )
      CALL CHCKXC( .FALSE., ' ', OK   )

C
C     Set the rest of the SCLK parameters.
C
      KVNAME = 'SCLK_DATA_TYPE_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PIPOOL ( KVNAME, 1, 1 )
      CALL CHCKXC( .FALSE., ' ', OK   )


      KVNAME = 'SCLK01_TIME_SYSTEM_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PIPOOL ( KVNAME, 1, TDB )
      CALL CHCKXC( .FALSE., ' ', OK   )


      KVNAME = 'SCLK01_N_FIELDS_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PIPOOL ( KVNAME, 1, 1 )
      CALL CHCKXC( .FALSE., ' ', OK   )


      MAXMOD = 1.D6

      KVNAME = 'SCLK01_MODULI_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PDPOOL ( KVNAME, 1, MAXMOD )
      CALL CHCKXC( .FALSE., ' ', OK   )


      KVNAME = 'SCLK01_OFFSETS_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PDPOOL ( KVNAME, 1, 0.D0 )
      CALL CHCKXC( .FALSE., ' ', OK   )


      KVNAME = 'SCLK01_OUTPUT_DELIM_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PIPOOL ( KVNAME, 1, 1 )
      CALL CHCKXC( .FALSE., ' ', OK   )


      KVNAME = 'SCLK_PARTITION_START_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PDPOOL ( KVNAME, 1, 0.D0 )
      CALL CHCKXC( .FALSE., ' ', OK   )


      KVNAME = 'SCLK_PARTITION_END_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PDPOOL ( KVNAME, 1, MAXMOD )
      CALL CHCKXC( .FALSE., ' ', OK   )

C
C     Now try a conversion.
C
C     The error will be trapped in ZZSCAD01.
C
      CALL SCDECD( SC, 0.D0, CLKSTR )
      CALL CHCKXC( .TRUE., 'SPICE(TOOMANYCOEFFS)', OK   )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to use SCLK with mismatched partition '
     .//           'start time and stop time counts.' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Just perturb the partitions of clock -9.
C
      CALL FURNSH ( SCLK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SC = -9

      NPARTS = 10

      DO I = 1, NPARTS

         XSTART(I) =     0.D0
         XSTOP(I)  = DBLE(I)

      END DO

      KVNAME = 'SCLK_PARTITION_START_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PDPOOL ( KVNAME, NPARTS, XSTART )
      CALL CHCKXC( .FALSE., ' ', OK   )


      KVNAME = 'SCLK_PARTITION_END_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PDPOOL ( KVNAME, NPARTS-1, XSTOP )
      CALL CHCKXC( .FALSE., ' ', OK   )

C
C     Now try a conversion.
C
C     The error will be trapped in ZZSCAD01.
C
      CALL SCDECD( SC, 0.D0, CLKSTR )
      CALL CHCKXC( .TRUE., 'SPICE(NUMPARTSUNEQUAL)', OK   )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZSCAD01: partition starts not found.' )

      SC = -99

      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      KVNAME = 'SCLK_PARTITION_START_99'

      CALL DVPOOL ( KVNAME )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSCAD01 ( SC,     HDSCLK, SCPOOL, CLKLST, DPFREE,
     .                DPBUFF, IFREE,  INTBUF, SCBASE, SCLKAT )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZSCAD01: partition stops not found.' )

      SC = -99

      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      KVNAME = 'SCLK_PARTITION_END_99'

      CALL DVPOOL ( KVNAME )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSCAD01 ( SC,     HDSCLK, SCPOOL, CLKLST, DPFREE,
     .                DPBUFF, IFREE,  INTBUF, SCBASE, SCLKAT )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZSCAD01: moduli not found.' )

      SC = -99

      KVNAME = 'SCLK01_MODULI_99'

      CALL DVPOOL ( KVNAME )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSCAD01 ( SC,     HDSCLK, SCPOOL, CLKLST, DPFREE,
     .                DPBUFF, IFREE,  INTBUF, SCBASE, SCLKAT )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZSCAD01: offsets not found.' )

      SC = -99

      KVNAME = 'SCLK01_OFFSETS_99'

      CALL DVPOOL ( KVNAME )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSCAD01 ( SC,     HDSCLK, SCPOOL, CLKLST, DPFREE,
     .                DPBUFF, IFREE,  INTBUF, SCBASE, SCLKAT )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZSCAD01: clock has too many partitions.' )

C
C     We'll just tweak the partitions for clock -9, since we
C     can easily restore the original kernel variables for that clock.
C
C
C     Create partition boundaries. Each partition will have just
C     10240 ticks. The last partition will have an end time past
C     the epoch of the last record.
C
      SC = -9

      NPARTS = 10000

      DO I = 1, NPARTS

         XSTART(I) =     0.D0
         XSTOP(I)  = 10240.D0

      END DO

      KVNAME = 'SCLK_PARTITION_START_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PDPOOL ( KVNAME, NPARTS, XSTART )
      CALL CHCKXC( .FALSE., ' ', OK   )


      KVNAME = 'SCLK_PARTITION_END_#'
      CALL REPMI( KVNAME, '#', -SC, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL PDPOOL ( KVNAME, NPARTS, XSTOP )
      CALL CHCKXC( .FALSE., ' ', OK   )

      CALL SCE2C ( SC, 1.D0, SCLKDP )
      CALL CHCKXC ( .TRUE., 'SPICE(TOOMANYPARTITIONS)', OK )

C
C     Restore the correct parameters for clock -9.
C
      CALL LDPOOL ( SCLK )
      CALL CHCKXC( .FALSE., ' ', OK   )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZSCAD01: moduli kernel variable too large.' )

      SC = -99

      KVNAME = 'SCLK01_MODULI_99'

      CALL FILLD ( 10.D0, DVALSZ, DVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( KVNAME, DVALSZ, DVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSCAD01 ( SC,     HDSCLK, SCPOOL, CLKLST, DPFREE,
     .                DPBUFF, IFREE,  INTBUF, SCBASE, SCLKAT )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARTOOLARGE)', OK )

      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZSCAD01: moduli kernel variable is non-numeric' )

      SC = -99

      KVNAME = 'SCLK01_MODULI_99'

      CALL PCPOOL ( KVNAME, 1, 'XYZ' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSCAD01 ( SC,     HDSCLK, SCPOOL, CLKLST, DPFREE,
     .                DPBUFF, IFREE,  INTBUF, SCBASE, SCLKAT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADKERNELVARTYPE)', OK )

      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZSCAD01: IFREE out of range' )

      SC = -99

      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Start with a clean system.
C
      CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )

      IFREE = IBUFSZ + 2

      CALL ZZSCAD01 ( SC,     HDSCLK, SCPOOL, CLKLST, DPFREE,
     .                DPBUFF, IFREE,  INTBUF, SCBASE, SCLKAT )
      CALL CHCKXC ( .TRUE., 'SPICE(BUG)', OK )


      IFREE = 0

      CALL ZZSCAD01 ( SC,     HDSCLK, SCPOOL, CLKLST, DPFREE,
     .                DPBUFF, IFREE,  INTBUF, SCBASE, SCLKAT )
      CALL CHCKXC ( .TRUE., 'SPICE(BUG)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZSCAD01: DPFREE out of range' )

      SC = -99

      CALL LDPOOL ( SCLK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Start with a clean system.
C
      CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )

      DPFREE = DBUFSZ + 2

      CALL ZZSCAD01 ( SC,     HDSCLK, SCPOOL, CLKLST, DPFREE,
     .                DPBUFF, IFREE,  INTBUF, SCBASE, SCLKAT )
      CALL CHCKXC ( .TRUE., 'SPICE(BUG)', OK )


      DPFREE = 0

      CALL ZZSCAD01 ( SC,     HDSCLK, SCPOOL, CLKLST, DPFREE,
     .                DPBUFF, IFREE,  INTBUF, SCBASE, SCLKAT )
      CALL CHCKXC ( .TRUE., 'SPICE(BUG)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZSCAD01: return on entry.' )

      CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )

      SHRTMS = 'SPICE(TESTERROR)'

      CALL SIGERR ( SHRTMS )

      CALL ZZSCAD01 ( SC,     HDSCLK, SCPOOL, CLKLST, DPFREE,
     .                DPBUFF, IFREE,  INTBUF, SCBASE, SCLKAT )

      CALL CHCKXC ( .TRUE., SHRTMS, OK )

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
