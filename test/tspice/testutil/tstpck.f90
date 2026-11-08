!$Procedure      TSTPCK (Create at test CK of type 3 and SCLK file)

      SUBROUTINE TSTPCK ( NAMEPC, LOADPC, KEEPPC )

! $ Abstract
!
!     Create and if appropriate load a test PCK kernel.
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
      CHARACTER*(*)         NAMEPC
      LOGICAL               LOADPC
      LOGICAL               KEEPPC


! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      NAMEPC     I   The name of the PC-kernel to create
!      LOADPC     I   Load the PC-kernel if TRUE
!      KEEPPC     I   Keep the PC-kernel if TRUE, else delete it.
!
! $ Detailed_Input
!
!     NAMEPC      is the name of a PC-kernel to create and load if
!                 LOADPC is set to TRUE.  If a PC-kernel of the same
!                 name already exists it is deleted.
!
!
!     LOADPC      is a logical that indicates whether or not the PCK
!                 file should be loaded after it is created.  If it
!                 has the value TRUE the PC-kernel is loaded after
!                 it is created.  Otherwise it is left un-opened.
!
!
!     KEEPPC      is a logical that indicates whether or not the PCK
!                 file should be deleted after it is loaded.  If KEEPPC
!                 is TRUE the file is not deleted.  If KEEPPC is FALSE
!                 the file is deleted after it is loaded.  NOTE that
!                 unless LOADSC is TRUE, the PCK file is not deleted
!                 by this routine.  This routine deletes the PC-kernel
!                 only if it LOADSC is TRUE and KEEPPC is FALSE.
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
!      This routine creates single PC-kernel. See Particulars
!      for more details.
!
! $ Particulars
!
!     This routine creates a planetary constants file for use in
!     testing.
!
!
!
! $ Examples
!
!     This is intended to be used in those instances when you
!     need a well defined PC-kernel for use in testing.  By using
!     this routine you can avoid having to know in advance where
!     a PCK file is on the system where you plan to do your testing.
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
!
! $ Version
!
!-    Test Utilities 1.1.1, 18-MAY-2010 (BVS)
!
!        Removed "C$" marker from text in the header.
!
!-    Test Utilities 1.1.0, 28-JUL-1999 (WLT)
!
!        Added code so that the PCK file (if not deleted at termination
!        of this routine) is registered with the Test Utility File
!        Registry (FILREG).  This way, the file will automatically
!        be deleted, after its test family is done.
!
!-    Test Utilities 1.0.0, 24-JAN-1995 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Create test CK and SCLK files.
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
      PARAMETER           ( NLINES = 466 )

      CHARACTER*(LNSIZE)    PCK  ( NLINES )

      INTEGER               I
      INTEGER               R
      INTEGER               UNIT

      CALL KILFIL ( NAMEPC )

      PCK (   1 ) = 'P_constants (PcK) SPICE kernel file '
      PCK (   2 ) = '======================================== ' &
      //            '=================================== '
      PCK (   3 ) = ' '
      PCK (   4 ) = 'This kernel is a condensation of the ' &
      //            'SPICE kernel complete.tpc '
      PCK (   5 ) = 'It is intended only for use in testing ' &
      //            'so that users do not '
      PCK (   6 ) = 'need to locate PCK kernels to use in ' &
      //            'testing software. '
      PCK (   7 ) = ' '
      PCK (   8 ) = 'This should not be your default PCK file ' &
      //            'as it is likely to '
      PCK (   9 ) = 'go out of date.  It will not be updated ' &
      //            'because it is '
      PCK (  10 ) = 'generated automatically at run time by ' &
      //            'the routine TSTPCK. '
      PCK (  11 ) = ' '
      PCK (  12 ) = BEGDAT()
      PCK (  13 ) = ' '
      PCK (  14 ) = 'BODY10_POLE_RA         = (  286.13 ' &
      //            '0.          0. ) '
      PCK (  15 ) = 'BODY10_POLE_DEC        = (   63.87 ' &
      //            '0.          0. ) '
      PCK (  16 ) = 'BODY10_PM              = (   84.10 ' &
      //            '+14.18440     0. ) '
      PCK (  17 ) = 'BODY10_LONG_AXIS       = (    0. ' &
      //            ') '
      PCK (  18 ) = 'BODY199_POLE_RA        = (  281.01, ' &
      //            '-0.003,      0. ) '
      PCK (  19 ) = 'BODY199_POLE_DEC       = (   61.45, ' &
      //            '-0.005,      0. ) '
      PCK (  20 ) = 'BODY199_PM             = (  329.71 ' &
      //            '+6.1385025   0. ) '
      PCK (  21 ) = 'BODY199_LONG_AXIS      = (    0. ' &
      //            ') '
      PCK (  22 ) = 'BODY299_POLE_RA        = (  272.69 ' &
      //            '0.          0. ) '
      PCK (  23 ) = 'BODY299_POLE_DEC       = (  +67.17 ' &
      //            '0.          0. ) '
      PCK (  24 ) = 'BODY299_PM             = (  160.39 ' &
      //            '-1.4813291   0. ) '
      PCK (  25 ) = 'BODY299_LONG_AXIS      = (    0. ' &
      //            ') '
      PCK (  26 ) = 'BODY399_POLE_RA        = (    0. ' &
      //            '-0.641         0. ) '
      PCK (  27 ) = 'BODY399_POLE_DEC       = (  +90. ' &
      //            '-0.557         0. ) '
      PCK (  28 ) = 'BODY399_PM             = (  190.16 ' &
      //            '+360.9856235     0. ) '
      PCK (  29 ) = 'BODY399_LONG_AXIS      = (    0. ' &
      //            ') '
      PCK (  30 ) = 'BODY399_MAG_NORTH_POLE_LON  =  ( -69.761 ' &
      //            ') '
      PCK (  31 ) = 'BODY399_MAG_NORTH_POLE_LAT  =  (  78.565 ' &
      //            ') '
      PCK (  32 ) = 'BODY3_NUT_PREC_ANGLES  = (  125.045 ' &
      //            '-1935.5328 '
      PCK (  33 ) = '249.390    -3871.0656 '
      PCK (  34 ) = '196.694  -475263.3 '
      PCK (  35 ) = '176.630  +487269.6519 '
      PCK (  36 ) = '358.219   -35999.04     ) '
      PCK (  37 ) = 'BODY499_POLE_RA          = (  317.681 ' &
      //            '-0.108       0.  ) '
      PCK (  38 ) = 'BODY499_POLE_DEC         = (  +52.886 ' &
      //            '-0.061       0.  ) '
      PCK (  39 ) = 'BODY499_PM               = (  176.868 ' &
      //            '+350.8919830   0.  ) '
      PCK (  40 ) = 'BODY4_NUT_PREC_ANGLES  = (  169.51 ' &
      //            '-15916.2801 '
      PCK (  41 ) = '192.93  +41215163.19675 '
      PCK (  42 ) = '53.47       -662.965275  ) '
      PCK (  43 ) = 'BODY599_POLE_RA        = (   268.05 ' &
      //            '-0.009  0.  ) '
      PCK (  44 ) = 'BODY599_POLE_DEC       = (   +64.49 ' &
      //            '+0.003  0.  ) '
      PCK (  45 ) = 'BODY599_PM             = (   284.95 ' &
      //            '+870.536  0.  ) '
      PCK (  46 ) = 'BODY599_LONG_AXIS      = (     0. ' &
      //            ') '
      PCK (  47 ) = 'BODY5_NUT_PREC_ANGLES  = (   73.32 ' &
      //            '+91472.9 '
      PCK (  48 ) = '198.54   +44243.8 '
      PCK (  49 ) = '283.90    +4850.7 '
      PCK (  50 ) = '355.80    +1191.3 '
      PCK (  51 ) = '119.90     +262.1 '
      PCK (  52 ) = '229.80      +64.3 '
      PCK (  53 ) = '352.25    +2382.6 '
      PCK (  54 ) = '113.35    +6070.0 '
      PCK (  55 ) = '146.64  +182945.8 '
      PCK (  56 ) = '397.08   +88487.6  ) '
      PCK (  57 ) = 'BODY699_POLE_RA        = (    40.58 ' &
      //            '-0.036      0.  ) '
      PCK (  58 ) = 'BODY699_POLE_DEC       = (   +83.54 ' &
      //            '-0.004      0.  ) '
      PCK (  59 ) = 'BODY699_PM             = (    38.90 ' &
      //            '+810.7939024  0.  ) '
      PCK (  60 ) = 'BODY699_LONG_AXIS      = (     0. ' &
      //            ') '
      PCK (  61 ) = 'BODY6_NUT_PREC_ANGLES  = (  177.40 ' &
      //            '-36505.5 '
      PCK (  62 ) = '300.00   -7225.9 '
      PCK (  63 ) = '345.20   -1016.3 '
      PCK (  64 ) = '29.80     -52.1 '
      PCK (  65 ) = '316.45    +506.2  ) '
      PCK (  66 ) = 'BODY799_POLE_RA        = (  257.43 ' &
      //            '0.         0.  ) '
      PCK (  67 ) = 'BODY799_POLE_DEC       = (  -15.10 ' &
      //            '0.         0.  ) '
      PCK (  68 ) = 'BODY799_PM             = (  203.81 ' &
      //            '-501.1600928  0.  ) '
      PCK (  69 ) = 'BODY799_LONG_AXIS      = (    0. ' &
      //            ') '
      PCK (  70 ) = 'BODY7_NUT_PREC_ANGLES  = (  115.75 ' &
      //            '+54991.87 '
      PCK (  71 ) = '141.69  +41887.66 '
      PCK (  72 ) = '135.03  +29927.35 '
      PCK (  73 ) = '61.77  +25733.59 '
      PCK (  74 ) = '249.32  +24471.46 '
      PCK (  75 ) = '43.86  +22278.41 '
      PCK (  76 ) = '77.66  +20289.42 '
      PCK (  77 ) = '157.36  +16652.76 '
      PCK (  78 ) = '101.81  +12872.63 '
      PCK (  79 ) = '138.64   +8061.81 '
      PCK (  80 ) = '102.23   -2024.22 '
      PCK (  81 ) = '316.41    2863.96 '
      PCK (  82 ) = '304.01     -51.94 '
      PCK (  83 ) = '308.71     -93.17 '
      PCK (  84 ) = '340.82     -75.32 '
      PCK (  85 ) = '259.14    -504.81 '
      PCK (  86 ) = '204.46   -4048.44 '
      PCK (  87 ) = '632.82    5727.92     ) '
      PCK (  88 ) = ' '
      PCK (  89 ) = 'BODY899_POLE_RA        = (  298.72 ' &
      //            '0.         0.  ) '
      PCK (  90 ) = 'BODY899_POLE_DEC       = (  +42.63 ' &
      //            '0.         0.  ) '
      PCK (  91 ) = 'BODY899_PM             = (  313.66 ' &
      //            '+483.7625981  0.  ) '
      PCK (  92 ) = 'BODY899_LONG_AXIS      = (    0. ' &
      //            ') '
      PCK (  93 ) = 'BODY8_NUT_PREC_ANGLES = (   179.280 ' &
      //            '54.3080 '
      PCK (  94 ) = '45.0600        3.65000 '
      PCK (  95 ) = '358.560       108.616 '
      PCK (  96 ) = '537.840       162.924 '
      PCK (  97 ) = '717.120       217.232 '
      PCK (  98 ) = '896.400       271.540 '
      PCK (  99 ) = '1075.68        325.848 '
      PCK ( 100 ) = '1254.96        380.156 '
      PCK ( 101 ) = '1434.24        434.464 '
      PCK ( 102 ) = '90.1200        7.30000 '
      PCK ( 103 ) = '135.180        10.9500 '
      PCK ( 104 ) = '180.240        14.6000 '
      PCK ( 105 ) = '225.300        18.2500    ) '
      PCK ( 106 ) = 'BODY999_POLE_RA        = (  311.63    0. ' &
      //            '0.  ) '
      PCK ( 107 ) = 'BODY999_POLE_DEC       = (   +4.18    0. ' &
      //            '0.  ) '
      PCK ( 108 ) = 'BODY999_PM             = (  252.66 ' &
      //            '-56.364  0.  ) '
      PCK ( 109 ) = 'BODY999_LONG_AXIS      = (    0. ' &
      //            ') '
      PCK ( 110 ) = 'BODY9511010_CONSTANTS_REF_FRAME  =   ( 2 ' &
      //            ') '
      PCK ( 111 ) = 'BODY9511010_CONSTANTS_JED_EPOCH  =   ( ' &
      //            '2433282.5 ) '
      PCK ( 112 ) = 'BODY9511010_POLE_RA              =   ( ' &
      //            '10.2        0           0  ) '
      PCK ( 113 ) = 'BODY9511010_POLE_DEC             =   ( ' &
      //            '26.2        0           0  ) '
      PCK ( 114 ) = 'BODY9511010_PM                   =   ( ' &
      //            '251.924  1226.906747    0  ) '
      PCK ( 115 ) = 'BODY301_POLE_RA        = (  270.000 ' &
      //            '0.           0. ) '
      PCK ( 116 ) = 'BODY301_POLE_DEC       = (  +66.534 ' &
      //            '0.           0. ) '
      PCK ( 117 ) = 'BODY301_PM             = (   38.314 ' &
      //            '+13.1763581    0. ) '
      PCK ( 118 ) = 'BODY301_LONG_AXIS      = (    0. ' &
      //            ') '
      PCK ( 119 ) = 'BODY301_NUT_PREC_RA  = (  -3.878  -0.120 ' &
      //            '+0.070  -0.017   0.    ) '
      PCK ( 120 ) = 'BODY301_NUT_PREC_DEC = (  +1.543  +0.024 ' &
      //            '-0.028  +0.007   0.    ) '
      PCK ( 121 ) = 'BODY301_NUT_PREC_PM  = (  +3.558  +0.121 ' &
      //            '-0.064  +0.016  +0.025 ) '
      PCK ( 122 ) = 'BODY401_POLE_RA       = (  317.68 ' &
      //            '-0.108       0.          ) '
      PCK ( 123 ) = 'BODY401_POLE_DEC      = (  +52.90 ' &
      //            '-0.061       0.          ) '
      PCK ( 124 ) = 'BODY401_PM            = (   35.06 ' &
      //            '+1128.8445850 6.644300D-09 ) '
      PCK ( 125 ) = 'BODY401_LONG_AXIS     = (    0. ' &
      //            ') '
      PCK ( 126 ) = 'BODY401_NUT_PREC_RA   = (   +1.79   0. ' &
      //            '0.  ) '
      PCK ( 127 ) = 'BODY401_NUT_PREC_DEC  = (   -1.08   0. ' &
      //            '0.  ) '
      PCK ( 128 ) = 'BODY401_NUT_PREC_PM   = (   -1.42  -0.78 ' &
      //            '0.  ) '
      PCK ( 129 ) = 'BODY402_POLE_RA       = (  316.65 ' &
      //            '-0.108       0.            ) '
      PCK ( 130 ) = 'BODY402_POLE_DEC      = (  +53.52 ' &
      //            '-0.061       0.            ) '
      PCK ( 131 ) = 'BODY402_PM            = (   79.41 ' &
      //            '+285.1618970  -3.897830D-10  ) '
      PCK ( 132 ) = 'BODY402_LONG_AXIS     = (    0. ' &
      //            ') '
      PCK ( 133 ) = 'BODY402_NUT_PREC_RA   = (    0.   0. ' &
      //            '+2.98  ) '
      PCK ( 134 ) = 'BODY402_NUT_PREC_DEC  = (    0.   0. ' &
      //            '-1.78  ) '
      PCK ( 135 ) = 'BODY402_NUT_PREC_PM   = (    0.   0. ' &
      //            '-2.58  ) '
      PCK ( 136 ) = 'BODY501_POLE_RA       = (  268.05 ' &
      //            '-0.009      0.  ) '
      PCK ( 137 ) = 'BODY501_POLE_DEC      = (  +64.50 ' &
      //            '+0.003      0.  ) '
      PCK ( 138 ) = 'BODY501_PM            = (  200.39 ' &
      //            '+203.4889538  0.  ) '
      PCK ( 139 ) = 'BODY501_LONG_AXIS     = (    0. ' &
      //            ') '
      PCK ( 140 ) = 'BODY501_NUT_PREC_RA   = (    0.   0. ' &
      //            '+0.094   +0.024   ) '
      PCK ( 141 ) = 'BODY501_NUT_PREC_DEC  = (    0.   0. ' &
      //            '+0.040   +0.011   ) '
      PCK ( 142 ) = 'BODY501_NUT_PREC_PM   = (    0.   0. ' &
      //            '-0.085   -0.022   ) '
      PCK ( 143 ) = 'BODY502_POLE_RA       = (  268.08 ' &
      //            '-0.009      0.   ) '
      PCK ( 144 ) = 'BODY502_POLE_DEC      = (  +64.51 ' &
      //            '+0.003      0.   ) '
      PCK ( 145 ) = 'BODY502_PM            = (   35.72 ' &
      //            '+101.3747235  0.   ) '
      PCK ( 146 ) = 'BODY502_LONG_AXIS     = (    0. ' &
      //            ') '
      PCK ( 147 ) = 'BODY502_NUT_PREC_RA   = ( 0. 0. 0. ' &
      //            '+1.086  +0.060  +0.015  +0.009 ) '
      PCK ( 148 ) = 'BODY502_NUT_PREC_DEC  = ( 0. 0. 0. ' &
      //            '+0.468  +0.026  +0.007  +0.002 ) '
      PCK ( 149 ) = 'BODY502_NUT_PREC_PM   = ( 0. 0. 0. ' &
      //            '-0.980  -0.054  -0.014  -0.008 ) '
      PCK ( 150 ) = 'BODY503_POLE_RA       = (  268.20 ' &
      //            '-0.009      0.   ) '
      PCK ( 151 ) = 'BODY503_POLE_DEC      = (  +64.57 ' &
      //            '+0.003      0.   ) '
      PCK ( 152 ) = 'BODY503_PM            = (   43.14 ' &
      //            '+50.3176081  0.   ) '
      PCK ( 153 ) = 'BODY503_LONG_AXIS     = (    0. ' &
      //            ') '
      PCK ( 154 ) = 'BODY503_NUT_PREC_RA   = ( 0. 0. 0. ' &
      //            '-0.037  +0.431  +0.091   ) '
      PCK ( 155 ) = 'BODY503_NUT_PREC_DEC  = ( 0. 0. 0. ' &
      //            '-0.016  +0.186  +0.039   ) '
      PCK ( 156 ) = 'BODY503_NUT_PREC_PM   = ( 0. 0. 0. ' &
      //            '+0.033  -0.389  -0.082   ) '
      PCK ( 157 ) = 'BODY504_POLE_RA       = (   268.72 ' &
      //            '-0.009      0.  ) '
      PCK ( 158 ) = 'BODY504_POLE_DEC      = (   +64.83 ' &
      //            '+0.003      0.  ) '
      PCK ( 159 ) = 'BODY504_PM            = (   259.67 ' &
      //            '+21.5710715  0.  ) '
      PCK ( 160 ) = 'BODY504_LONG_AXIS     = (     0. ' &
      //            ') '
      PCK ( 161 ) = 'BODY504_NUT_PREC_RA   = ( 0. 0. 0. 0. ' &
      //            '-0.068 +0.590  0. +0.010 ) '
      PCK ( 162 ) = 'BODY504_NUT_PREC_DEC  = ( 0. 0. 0. 0. ' &
      //            '-0.029 +0.254  0. -0.004 ) '
      PCK ( 163 ) = 'BODY504_NUT_PREC_PM   = ( 0. 0. 0. 0. ' &
      //            '+0.061 -0.533  0. -0.009 ) '
      PCK ( 164 ) = 'BODY505_POLE_RA       = (   268.05 ' &
      //            '-0.009      0.  ) '
      PCK ( 165 ) = 'BODY505_POLE_DEC      = (   +64.49 ' &
      //            '+0.003      0.  ) '
      PCK ( 166 ) = 'BODY505_PM            = (   231.67 ' &
      //            '+722.6314560  0.  ) '
      PCK ( 167 ) = 'BODY505_LONG_AXIS     = (     0. ' &
      //            ') '
      PCK ( 168 ) = 'BODY505_NUT_PREC_RA  = ( -0.84  0. 0. 0. ' &
      //            '0. 0. 0. 0.  +0.01  ) '
      PCK ( 169 ) = 'BODY505_NUT_PREC_DEC = ( -0.36  0. 0. 0. ' &
      //            '0. 0. 0. 0.   0.    ) '
      PCK ( 170 ) = 'BODY505_NUT_PREC_PM  = ( +0.76  0. 0. 0. ' &
      //            '0. 0. 0. 0.  -0.01  ) '
      PCK ( 171 ) = 'BODY514_POLE_RA       = (  268.05 ' &
      //            '-0.009       0.  ) '
      PCK ( 172 ) = 'BODY514_POLE_DEC      = (   64.49 ' &
      //            '+0.003       0.  ) '
      PCK ( 173 ) = 'BODY514_PM            = (    9.91 ' &
      //            '+533.7005330   0.  ) '
      PCK ( 174 ) = 'BODY514_LONG_AXIS     = (    0. ' &
      //            ') '
      PCK ( 175 ) = 'BODY514_NUT_PREC_RA  = ( 0. -2.12  0. 0. ' &
      //            '0. 0. 0. 0. 0.  +0.04  ) '
      PCK ( 176 ) = 'BODY514_NUT_PREC_DEC = ( 0. -0.91  0. 0. ' &
      //            '0. 0. 0. 0. 0.  +0.01  ) '
      PCK ( 177 ) = 'BODY514_NUT_PREC_PM  = ( 0. +1.91  0. 0. ' &
      //            '0. 0. 0. 0. 0.  -0.04  ) '
      PCK ( 178 ) = 'BODY515_POLE_RA       = (  268.05 ' &
      //            '-0.009       0.  ) '
      PCK ( 179 ) = 'BODY515_POLE_DEC      = (   64.49 ' &
      //            '+0.003       0.  ) '
      PCK ( 180 ) = 'BODY515_PM            = (    5.75 ' &
      //            '+1206.9950400   0.  ) '
      PCK ( 181 ) = 'BODY515_LONG_AXIS     = (    0. ' &
      //            ') '
      PCK ( 182 ) = 'BODY516_POLE_RA       = (  268.05 ' &
      //            '-0.009       0.  ) '
      PCK ( 183 ) = 'BODY516_POLE_DEC      = (   64.49 ' &
      //            '+0.003       0.  ) '
      PCK ( 184 ) = 'BODY516_PM            = (  302.24 ' &
      //            '+1221.2489660   0.  ) '
      PCK ( 185 ) = 'BODY516_LONG_AXIS     = (    0. ' &
      //            ') '
      PCK ( 186 ) = 'BODY601_POLE_RA       = (    40.66 ' &
      //            '-0.036      0.  ) '
      PCK ( 187 ) = 'BODY601_POLE_DEC      = (   +83.52 ' &
      //            '-0.004      0.  ) '
      PCK ( 188 ) = 'BODY601_PM            = (   337.46 ' &
      //            '+381.9945550  0.  ) '
      PCK ( 189 ) = 'BODY601_LONG_AXIS     = (     0. ' &
      //            ') '
      PCK ( 190 ) = 'BODY601_NUT_PREC_RA   = (   +13.56  0. ' &
      //            '0.  0.    0.     ) '
      PCK ( 191 ) = 'BODY601_NUT_PREC_DEC  = (    -1.53  0. ' &
      //            '0.  0.    0.     ) '
      PCK ( 192 ) = 'BODY601_NUT_PREC_PM   = (   -13.48  0. ' &
      //            '0.  0.  -44.85   ) '
      PCK ( 193 ) = 'BODY602_POLE_RA       = (   40.66 ' &
      //            '-0.036      0.  ) '
      PCK ( 194 ) = 'BODY602_POLE_DEC      = (  +83.52 ' &
      //            '-0.004      0.  ) '
      PCK ( 195 ) = 'BODY602_PM            = (    2.82 ' &
      //            '+262.7318996  0.  ) '
      PCK ( 196 ) = 'BODY602_LONG_AXIS     = (    0. ' &
      //            ') '
      PCK ( 197 ) = 'BODY603_POLE_RA       = (    40.66 ' &
      //            '-0.036      0.  ) '
      PCK ( 198 ) = 'BODY603_POLE_DEC      = (   +83.52 ' &
      //            '-0.004      0.  ) '
      PCK ( 199 ) = 'BODY603_PM            = (    10.45 ' &
      //            '+190.6979085  0.  ) '
      PCK ( 200 ) = 'BODY603_LONG_AXIS     = (     0. ' &
      //            ') '
      PCK ( 201 ) = 'BODY603_NUT_PREC_RA   = (   0.  +9.66 ' &
      //            '0.  0.   0.    ) '
      PCK ( 202 ) = 'BODY603_NUT_PREC_DEC  = (   0.  -1.09 ' &
      //            '0.  0.   0.    ) '
      PCK ( 203 ) = 'BODY603_NUT_PREC_PM   = (   0.  -9.60 ' &
      //            '0.  0.  +2.23  ) '
      PCK ( 204 ) = 'BODY604_POLE_RA       = (    40.66 ' &
      //            '-0.036      0.  ) '
      PCK ( 205 ) = 'BODY604_POLE_DEC      = (   +83.52 ' &
      //            '-0.004      0.  ) '
      PCK ( 206 ) = 'BODY604_PM            = (   357.00 ' &
      //            '+131.5349316  0.  ) '
      PCK ( 207 ) = 'BODY604_LONG_AXIS     = (     0. ' &
      //            ') '
      PCK ( 208 ) = 'BODY605_POLE_RA       = (   40.38 ' &
      //            '-0.036      0.   ) '
      PCK ( 209 ) = 'BODY605_POLE_DEC      = (  +83.55 ' &
      //            '-0.004      0.   ) '
      PCK ( 210 ) = 'BODY605_PM            = (  235.16 ' &
      //            '+79.6900478  0.   ) '
      PCK ( 211 ) = 'BODY605_LONG_AXIS     = (    0. ' &
      //            ') '
      PCK ( 212 ) = 'BODY605_NUT_PREC_RA   = (   0.  0. ' &
      //            '+3.10   ) '
      PCK ( 213 ) = 'BODY605_NUT_PREC_DEC  = (   0.  0. ' &
      //            '-0.35   ) '
      PCK ( 214 ) = 'BODY605_NUT_PREC_PM   = (   0.  0. ' &
      //            '-3.08   ) '
      PCK ( 215 ) = 'BODY606_POLE_RA       = (    36.41 ' &
      //            '-0.036      0.   ) '
      PCK ( 216 ) = 'BODY606_POLE_DEC      = (   +83.94 ' &
      //            '-0.004      0.   ) '
      PCK ( 217 ) = 'BODY606_PM            = (   189.64 ' &
      //            '+22.5769768  0.   ) '
      PCK ( 218 ) = 'BODY606_LONG_AXIS     = (     0. ' &
      //            ') '
      PCK ( 219 ) = 'BODY606_NUT_PREC_RA   = (   0.  0.  0. ' &
      //            '+2.66  ) '
      PCK ( 220 ) = 'BODY606_NUT_PREC_DEC  = (   0.  0.  0. ' &
      //            '-0.30  ) '
      PCK ( 221 ) = 'BODY606_NUT_PREC_PM   = (   0.  0.  0. ' &
      //            '-2.64  ) '
      PCK ( 222 ) = 'BODY608_POLE_RA       = (   318.16 ' &
      //            '-3.949      0.  ) '
      PCK ( 223 ) = 'BODY608_POLE_DEC      = (   +75.03 ' &
      //            '-1.143      0.  ) '
      PCK ( 224 ) = 'BODY608_PM            = (   350.20 ' &
      //            '+4.5379572  0.  ) '
      PCK ( 225 ) = 'BODY608_LONG_AXIS     = (     0. ' &
      //            ') '
      PCK ( 226 ) = 'BODY613_POLE_RA       = (    50.50 ' &
      //            '-0.036      0.  ) '
      PCK ( 227 ) = 'BODY613_POLE_DEC      = (    84.06 ' &
      //            '-0.004      0.  ) '
      PCK ( 228 ) = 'BODY613_PM            = (    56.88 ' &
      //            '+190.6979330  0.  ) '
      PCK ( 229 ) = 'BODY613_LONG_AXIS     = (     0. ' &
      //            ') '
      PCK ( 230 ) = 'BODY615_POLE_RA       = (    40.58 ' &
      //            '-0.036      0.  ) '
      PCK ( 231 ) = 'BODY615_POLE_DEC      = (    83.53 ' &
      //            '-0.004      0.  ) '
      PCK ( 232 ) = 'BODY615_PM            = (   137.88 ' &
      //            '+598.3060000  0.  ) '
      PCK ( 233 ) = 'BODY615_LONG_AXIS     = (     0. ' &
      //            ') '
      PCK ( 234 ) = 'BODY616_POLE_RA       = (    40.58 ' &
      //            '-0.036      0.  ) '
      PCK ( 235 ) = 'BODY616_POLE_DEC      = (    83.53 ' &
      //            '-0.004      0.  ) '
      PCK ( 236 ) = 'BODY616_PM            = (   296.14 ' &
      //            '+587.2890000  0.  ) '
      PCK ( 237 ) = 'BODY616_LONG_AXIS     = (     0. ' &
      //            ') '
      PCK ( 238 ) = 'BODY617_POLE_RA       = (    40.58 ' &
      //            '-0.036      0.  ) '
      PCK ( 239 ) = 'BODY617_POLE_DEC      = (    83.53 ' &
      //            '-0.004      0.  ) '
      PCK ( 240 ) = 'BODY617_PM            = (   162.92 ' &
      //            '+572.7891000  0.  ) '
      PCK ( 241 ) = 'BODY617_LONG_AXIS     = (     0. ' &
      //            ') '
      PCK ( 242 ) = 'BODY701_POLE_RA       = (   257.43 ' &
      //            '0.         0.  ) '
      PCK ( 243 ) = 'BODY701_POLE_DEC      = (   -15.10 ' &
      //            '0.         0.  ) '
      PCK ( 244 ) = 'BODY701_PM            = (   156.22 ' &
      //            '-142.8356681  0.  ) '
      PCK ( 245 ) = 'BODY701_LONG_AXIS     = (     0. ' &
      //            ') '
      PCK ( 246 ) = 'BODY701_NUT_PREC_RA   = (  0. 0. 0. 0. ' &
      //            '0. '
      PCK ( 247 ) = '0. 0. 0. 0. 0.    0.   0.    +0.29 ) '
      PCK ( 248 ) = 'BODY701_NUT_PREC_DEC  = (  0. 0. 0. 0. ' &
      //            '0. '
      PCK ( 249 ) = '0. 0. 0. 0. 0.    0.   0.    +0.28 ) '
      PCK ( 250 ) = 'BODY701_NUT_PREC_PM   = (  0. 0. 0. 0. ' &
      //            '0. '
      PCK ( 251 ) = '0. 0. 0. 0. 0.    0.  +0.05  +0.08 ) '
      PCK ( 252 ) = 'BODY702_POLE_RA       = (   257.43    0. ' &
      //            '0.   ) '
      PCK ( 253 ) = 'BODY702_POLE_DEC      = (   -15.10    0. ' &
      //            '0.   ) '
      PCK ( 254 ) = 'BODY702_PM            = (   108.05 ' &
      //            '-86.8688923  0.   ) '
      PCK ( 255 ) = 'BODY702_LONG_AXIS     = (     0. ' &
      //            ') '
      PCK ( 256 ) = 'BODY702_NUT_PREC_RA   = (  0. 0. 0. 0. ' &
      //            '0. '
      PCK ( 257 ) = '0. 0. 0. 0. 0.   0.   0.    0.  +0.21 ) '
      PCK ( 258 ) = 'BODY702_NUT_PREC_DEC  = (  0. 0. 0. 0. ' &
      //            '0. '
      PCK ( 259 ) = '0. 0. 0. 0. 0.   0.   0.    0.  +0.20 ) '
      PCK ( 260 ) = 'BODY702_NUT_PREC_PM   = (  0. 0. 0. 0. ' &
      //            '0. '
      PCK ( 261 ) = '0. 0. 0. 0. 0.   0.  -0.09  0.  +0.06 ) '
      PCK ( 262 ) = 'BODY703_POLE_RA       = (   257.43    0. ' &
      //            '0.   ) '
      PCK ( 263 ) = 'BODY703_POLE_DEC      = (   -15.10    0. ' &
      //            '0.   ) '
      PCK ( 264 ) = 'BODY703_PM            = (    77.74 ' &
      //            '-41.3514316  0.   ) '
      PCK ( 265 ) = 'BODY703_LONG_AXIS     = (     0. ' &
      //            ') '
      PCK ( 266 ) = 'BODY703_NUT_PREC_RA   = (  0. 0. 0. 0. ' &
      //            '0. '
      PCK ( 267 ) = '0. 0. 0. 0. 0.   0. 0. 0. 0.  +0.29 ) '
      PCK ( 268 ) = 'BODY703_NUT_PREC_DEC  = (  0. 0. 0. 0. ' &
      //            '0. '
      PCK ( 269 ) = '0. 0. 0. 0. 0.   0. 0. 0. 0.  +0.28 ) '
      PCK ( 270 ) = 'BODY703_NUT_PREC_PM   = (  0. 0. 0. 0. ' &
      //            '0. '
      PCK ( 271 ) = '0. 0. 0. 0. 0.   0. 0. 0. 0.  +0.08 ) '
      PCK ( 272 ) = 'BODY704_POLE_RA       = (   257.43    0. ' &
      //            '0.   ) '
      PCK ( 273 ) = 'BODY704_POLE_DEC      = (   -15.10    0. ' &
      //            '0.   ) '
      PCK ( 274 ) = 'BODY704_PM            = (     6.77 ' &
      //            '-26.7394932  0.   ) '
      PCK ( 275 ) = 'BODY704_LONG_AXIS     = (     0. ' &
      //            ') '
      PCK ( 276 ) = 'BODY704_NUT_PREC_RA   = (  0. 0. 0. 0. ' &
      //            '0. '
      PCK ( 277 ) = '0. 0. 0. 0. 0. '
      PCK ( 278 ) = '0. 0. 0. 0. 0.  +0.16 ) '
      PCK ( 279 ) = 'BODY704_NUT_PREC_DEC  = (  0. 0. 0. 0. ' &
      //            '0. '
      PCK ( 280 ) = '0. 0. 0. 0. 0. '
      PCK ( 281 ) = '0. 0. 0. 0. 0.  +0.16 ) '
      PCK ( 282 ) = 'BODY704_NUT_PREC_PM   = (  0. 0. 0. 0. ' &
      //            '0. '
      PCK ( 283 ) = '0. 0. 0. 0. 0. '
      PCK ( 284 ) = '0. 0. 0. 0. 0.  +0.04 ) '
      PCK ( 285 ) = 'BODY705_POLE_RA      = (   257.43     0. ' &
      //            '0.   ) '
      PCK ( 286 ) = 'BODY705_POLE_DEC     = (   -15.08     0. ' &
      //            '0.   ) '
      PCK ( 287 ) = 'BODY705_PM           = (    30.70 ' &
      //            '-254.6906892  0.   ) '
      PCK ( 288 ) = 'BODY705_LONG_AXIS    = (     0. ' &
      //            ') '
      PCK ( 289 ) = 'BODY705_NUT_PREC_RA  = (  0.     0. ' &
      //            '0.    0.    0. '
      PCK ( 290 ) = '0.     0.     0.    0.    0. '
      PCK ( 291 ) = '4.41   0.     0.    0.    0. '
      PCK ( 292 ) = '0.    -0.04                   ) '
      PCK ( 293 ) = 'BODY705_NUT_PREC_DEC = (  0.     0. ' &
      //            '0.    0.    0. '
      PCK ( 294 ) = '0.     0.     0.    0.    0. '
      PCK ( 295 ) = '4.25   0.     0.    0.    0. '
      PCK ( 296 ) = '0.    -0.02                   ) '
      PCK ( 297 ) = ' '
      PCK ( 298 ) = 'BODY705_NUT_PREC_PM  = (  0.     0. ' &
      //            '0.    0.    0. '
      PCK ( 299 ) = '0.     0.     0.    0.    0. '
      PCK ( 300 ) = '1.15  -1.27   0.    0.    0. '
      PCK ( 301 ) = '0.    -0.09   0.15            ) '
      PCK ( 302 ) = 'BODY706_POLE_RA      = (   257.31 ' &
      //            '0.         0.   ) '
      PCK ( 303 ) = 'BODY706_POLE_DEC     = (   -15.18 ' &
      //            '0.         0.   ) '
      PCK ( 304 ) = 'BODY706_PM           = (   127.69 ' &
      //            '-1074.5205730  0.   ) '
      PCK ( 305 ) = 'BODY706_LONG_AXIS    = (     0. ' &
      //            ') '
      PCK ( 306 ) = 'BODY706_NUT_PREC_RA  = (  -0.15  ) '
      PCK ( 307 ) = 'BODY706_NUT_PREC_DEC = (  +0.14  ) '
      PCK ( 308 ) = 'BODY706_NUT_PREC_PM  = (  -0.04  ) '
      PCK ( 309 ) = 'BODY707_POLE_RA      = (   257.31 ' &
      //            '0.         0.   ) '
      PCK ( 310 ) = 'BODY707_POLE_DEC     = (   -15.18 ' &
      //            '0.         0.   ) '
      PCK ( 311 ) = 'BODY707_PM           = (   130.35 ' &
      //            '-956.4068150  0.   ) '
      PCK ( 312 ) = 'BODY707_LONG_AXIS    = (     0. ' &
      //            ') '
      PCK ( 313 ) = 'BODY707_NUT_PREC_RA  = ( 0.   -0.09 ) '
      PCK ( 314 ) = 'BODY707_NUT_PREC_DEC = ( 0.   +0.09 ) '
      PCK ( 315 ) = 'BODY707_NUT_PREC_PM  = ( 0.   -0.03 ) '
      PCK ( 316 ) = 'BODY708_POLE_RA      = (   257.31 ' &
      //            '0.         0.   ) '
      PCK ( 317 ) = 'BODY708_POLE_DEC     = (   -15.18 ' &
      //            '0.         0.   ) '
      PCK ( 318 ) = 'BODY708_PM           = (   105.46 ' &
      //            '-828.3914760  0.   ) '
      PCK ( 319 ) = 'BODY708_LONG_AXIS    = (     0. ' &
      //            ') '
      PCK ( 320 ) = 'BODY708_NUT_PREC_RA  = ( 0. 0.  -0.16 ) '
      PCK ( 321 ) = 'BODY708_NUT_PREC_DEC = ( 0. 0.  +0.16 ) '
      PCK ( 322 ) = 'BODY708_NUT_PREC_PM  = ( 0. 0.  -0.04 ) '
      PCK ( 323 ) = 'BODY709_POLE_RA      = (   257.31 ' &
      //            '0.         0.   ) '
      PCK ( 324 ) = 'BODY709_POLE_DEC     = (   -15.18 ' &
      //            '0.         0.   ) '
      PCK ( 325 ) = 'BODY709_PM           = (    59.16 ' &
      //            '-776.5816320  0.   ) '
      PCK ( 326 ) = 'BODY709_LONG_AXIS    = (     0. ' &
      //            ') '
      PCK ( 327 ) = 'BODY709_NUT_PREC_RA  = ( 0. 0. 0. -0.04 ' &
      //            ') '
      PCK ( 328 ) = 'BODY709_NUT_PREC_DEC = ( 0. 0. 0. +0.04 ' &
      //            ') '
      PCK ( 329 ) = 'BODY709_NUT_PREC_PM  = ( 0. 0. 0. -0.01 ' &
      //            ') '
      PCK ( 330 ) = 'BODY710_POLE_RA      = (   257.31 ' &
      //            '0.         0.   ) '
      PCK ( 331 ) = 'BODY710_POLE_DEC     = (   -15.18 ' &
      //            '0.         0.   ) '
      PCK ( 332 ) = 'BODY710_PM           = (    95.08 ' &
      //            '-760.0531690  0.   ) '
      PCK ( 333 ) = 'BODY710_LONG_AXIS    = (     0. ' &
      //            ') '
      PCK ( 334 ) = 'BODY710_NUT_PREC_RA  = ( 0. 0. 0.  0. ' &
      //            '-0.17 ) '
      PCK ( 335 ) = 'BODY710_NUT_PREC_DEC = ( 0. 0. 0.  0. ' &
      //            '+0.16 ) '
      PCK ( 336 ) = 'BODY710_NUT_PREC_PM  = ( 0. 0. 0.  0. ' &
      //            '-0.04 ) '
      PCK ( 337 ) = 'BODY711_POLE_RA      = (   257.31 ' &
      //            '0.         0.   ) '
      PCK ( 338 ) = 'BODY711_POLE_DEC     = (   -15.18 ' &
      //            '0.         0.   ) '
      PCK ( 339 ) = 'BODY711_PM           = (   302.56 ' &
      //            '-730.1253660  0.   ) '
      PCK ( 340 ) = 'BODY711_LONG_AXIS    = (     0. ' &
      //            ') '
      PCK ( 341 ) = 'BODY711_NUT_PREC_RA  = ( 0. 0. 0. 0. 0. ' &
      //            '-0.06 ) '
      PCK ( 342 ) = 'BODY711_NUT_PREC_DEC = ( 0. 0. 0. 0. 0. ' &
      //            '+0.06 ) '
      PCK ( 343 ) = 'BODY711_NUT_PREC_PM  = ( 0. 0. 0. 0. 0. ' &
      //            '-0.02 ) '
      PCK ( 344 ) = 'BODY712_POLE_RA      = (   257.31 ' &
      //            '0.         0.   ) '
      PCK ( 345 ) = 'BODY712_POLE_DEC     = (   -15.18 ' &
      //            '0.         0.   ) '
      PCK ( 346 ) = 'BODY712_PM           = (    25.03 ' &
      //            '-701.4865870  0.   ) '
      PCK ( 347 ) = 'BODY712_LONG_AXIS    = (     0. ' &
      //            ') '
      PCK ( 348 ) = 'BODY712_NUT_PREC_RA  = ( 0. 0. 0. 0. 0. ' &
      //            '0.  -0.09 ) '
      PCK ( 349 ) = 'BODY712_NUT_PREC_DEC = ( 0. 0. 0. 0. 0. ' &
      //            '0.  +0.09 ) '
      PCK ( 350 ) = 'BODY712_NUT_PREC_PM  = ( 0. 0. 0. 0. 0. ' &
      //            '0.  -0.02 ) '
      PCK ( 351 ) = 'BODY713_POLE_RA      = (   257.31 ' &
      //            '0.         0.   ) '
      PCK ( 352 ) = 'BODY713_POLE_DEC     = (   -15.18 ' &
      //            '0.         0.   ) '
      PCK ( 353 ) = 'BODY713_PM           = (   314.90 ' &
      //            '-644.6311260  0.   ) '
      PCK ( 354 ) = 'BODY713_LONG_AXIS    = (     0. ' &
      //            ') '
      PCK ( 355 ) = 'BODY713_NUT_PREC_RA  = ( 0. 0. 0. 0. 0. ' &
      //            '0. 0.   -0.29 ) '
      PCK ( 356 ) = 'BODY713_NUT_PREC_DEC = ( 0. 0. 0. 0. 0. ' &
      //            '0. 0.   +0.28 ) '
      PCK ( 357 ) = 'BODY713_NUT_PREC_PM  = ( 0. 0. 0. 0. 0. ' &
      //            '0. 0.   -0.08 ) '
      PCK ( 358 ) = 'BODY714_POLE_RA      = (   257.31 ' &
      //            '0.         0.   ) '
      PCK ( 359 ) = 'BODY714_POLE_DEC     = (   -15.18 ' &
      //            '0.         0.   ) '
      PCK ( 360 ) = 'BODY714_PM           = (   297.46 ' &
      //            '-577.3628170  0.   ) '
      PCK ( 361 ) = 'BODY714_LONG_AXIS    = (     0. ' &
      //            ') '
      PCK ( 362 ) = 'BODY714_NUT_PREC_RA  = ( 0. 0. 0. 0. 0. ' &
      //            '0. 0. 0.   -0.03 ) '
      PCK ( 363 ) = 'BODY714_NUT_PREC_DEC = ( 0. 0. 0. 0. 0. ' &
      //            '0. 0. 0.   +0.03 ) '
      PCK ( 364 ) = 'BODY714_NUT_PREC_PM  = ( 0. 0. 0. 0. 0. ' &
      //            '0. 0. 0.   -0.01 ) '
      PCK ( 365 ) = 'BODY715_POLE_RA      = (   257.31 ' &
      //            '0.         0.   ) '
      PCK ( 366 ) = 'BODY715_POLE_DEC     = (   -15.18 ' &
      //            '0.         0.   ) '
      PCK ( 367 ) = 'BODY715_PM           = (    91.24 ' &
      //            '-472.5450690  0.   ) '
      PCK ( 368 ) = 'BODY715_LONG_AXIS    = (     0. ' &
      //            ') '
      PCK ( 369 ) = 'BODY715_NUT_PREC_RA  = ( 0. 0. 0. 0. 0. ' &
      //            '0. 0. 0. 0.   -0.33 ) '
      PCK ( 370 ) = 'BODY715_NUT_PREC_DEC = ( 0. 0. 0. 0. 0. ' &
      //            '0. 0. 0. 0.   +0.31 ) '
      PCK ( 371 ) = 'BODY715_NUT_PREC_PM  = ( 0. 0. 0. 0. 0. ' &
      //            '0. 0. 0. 0.   -0.99 ) '
      PCK ( 372 ) = 'BODY801_POLE_RA       = (   298.72    0. ' &
      //            '0.  ) '
      PCK ( 373 ) = 'BODY801_POLE_DEC      = (   +40.59    0. ' &
      //            '0.  ) '
      PCK ( 374 ) = 'BODY801_PM            = (   297.14 ' &
      //            '-61.2572675  0.  ) '
      PCK ( 375 ) = 'BODY801_LONG_AXIS     = (     0. ' &
      //            ') '
      PCK ( 376 ) = 'BODY801_NUT_PREC_RA   = (  -30.72    0. ' &
      //            '-5.58   -1.75 '
      PCK ( 377 ) = '-0.58   -0.21   -0.08   -0.03  -0.01  ) '
      PCK ( 378 ) = 'BODY801_NUT_PREC_DEC  = (  +21.79    0. ' &
      //            '+1.91   +0.48 '
      PCK ( 379 ) = '+0.13   +0.04   +0.01            ) '
      PCK ( 380 ) = 'BODY801_NUT_PREC_PM   = (  +20.81    0. ' &
      //            '+6.01   +1.73  +0.59 '
      PCK ( 381 ) = '+0.21   +0.08   +0.03   +0.01         ) '
      PCK ( 382 ) = 'BODY802_POLE_RA       = (    273.48 ' &
      //            '0.        0.  ) '
      PCK ( 383 ) = 'BODY802_POLE_DEC      = (     67.22 ' &
      //            '0.        0.  ) '
      PCK ( 384 ) = 'BODY802_PM            = (    237.22 ' &
      //            '+0.9996465 0.  ) '
      PCK ( 385 ) = 'BODY802_LONG_AXIS     = (      0. ' &
      //            ') '
      PCK ( 386 ) = 'BODY802_NUT_PREC_RA   = (  0.    -17.81 '
      PCK ( 387 ) = '0.      0.     0.      0. '
      PCK ( 388 ) = '0.      0.     0. '
      PCK ( 389 ) = '+2.56   -0.51  +0.11   -0.03  ) '
      PCK ( 390 ) = 'BODY802_NUT_PREC_DEC  = (  0.     -6.67 '
      PCK ( 391 ) = '0.      0.     0.      0. '
      PCK ( 392 ) = '0.      0.     0. '
      PCK ( 393 ) = '+0.47   -0.07  +0.01          ) '
      PCK ( 394 ) = 'BODY802_NUT_PREC_PM   = (  0.     16.48 '
      PCK ( 395 ) = '0.      0.     0.      0. '
      PCK ( 396 ) = '0.      0.     0. '
      PCK ( 397 ) = '-2.57   +0.51 -0.11   +0.02  ) '
      PCK ( 398 ) = 'BODY901_POLE_RA       = (    312.98 ' &
      //            '0.         0.  ) '
      PCK ( 399 ) = 'BODY901_POLE_DEC      = (     +8.49 ' &
      //            '0.         0.  ) '
      PCK ( 400 ) = 'BODY901_PM            = (     56.11 ' &
      //            '-56.3624607  0.  ) '
      PCK ( 401 ) = 'BODY901_LONG_AXIS     = (      0. ' &
      //            ') '
      PCK ( 402 ) = 'BODY10_RADII      = (   696000. ' &
      //            '696000.      696000.     ) '
      PCK ( 403 ) = 'BODY199_RADII     = (     2439.7 ' &
      //            '2439.7      2439.7     ) '
      PCK ( 404 ) = 'BODY299_RADII     = (     6051.9 ' &
      //            '6051.9      6051.9     ) '
      PCK ( 405 ) = 'BODY399_RADII     = (     6378.140 ' &
      //            '6378.140     6356.75   ) '
      PCK ( 406 ) = 'BODY499_RADII       = (     3397. ' &
      //            '3397.         3375.     ) '
      PCK ( 407 ) = 'BODY599_RADII     = (    71492. ' &
      //            '71492.       66854.     ) '
      PCK ( 408 ) = 'BODY699_RADII     = (    60268. ' &
      //            '60268.       54364.     ) '
      PCK ( 409 ) = 'BODY799_RADII     = (    25559. ' &
      //            '25559.       24973.     ) '
      PCK ( 410 ) = 'BODY899_RADII   = (    25269. ' &
      //            '25269.       24800      ) '
      PCK ( 411 ) = 'BODY999_RADII     = (     1162. ' &
      //            '1162.        1162.     ) '
      PCK ( 412 ) = 'BODY9511010_RADII     =   (  9 ' &
      //            '5.5         5  ) '
      PCK ( 413 ) = 'BODY301_RADII     = (     1737.4 ' &
      //            '1737.4       1737.4    ) '
      PCK ( 414 ) = 'BODY401_RADII     = (       13.4 ' &
      //            '11.2          9.2    ) '
      PCK ( 415 ) = 'BODY402_RADII     = (        7.5 ' &
      //            '6.1          5.2    ) '
      PCK ( 416 ) = 'BODY501_RADII     = (     1830. ' &
      //            '1818.7       1815.3    ) '
      PCK ( 417 ) = 'BODY502_RADII     = (     1565. ' &
      //            '1565.        1565.     ) '
      PCK ( 418 ) = 'BODY503_RADII     = (     2634. ' &
      //            '2634.        2634.     ) '
      PCK ( 419 ) = 'BODY504_RADII     = (     2403. ' &
      //            '2403.        2403.     ) '
      PCK ( 420 ) = 'BODY505_RADII     = (      131. ' &
      //            '73.          67.     ) '
      PCK ( 421 ) = 'BODY506_RADII    = (  85      85      85 ' &
      //            ') '
      PCK ( 422 ) = 'BODY507_RADII    = (  40      40      40 ' &
      //            ') '
      PCK ( 423 ) = 'BODY508_RADII    = (  18      18      18 ' &
      //            ') '
      PCK ( 424 ) = 'BODY509_RADII    = (  14      14      14 ' &
      //            ') '
      PCK ( 425 ) = 'BODY510_RADII    = (  12      12      12 ' &
      //            ') '
      PCK ( 426 ) = 'BODY511_RADII    = (  15      15      15 ' &
      //            ') '
      PCK ( 427 ) = 'BODY512_RADII    = (  10      10      10 ' &
      //            ') '
      PCK ( 428 ) = 'BODY513_RADII    = (   5       5       5 ' &
      //            ') '
      PCK ( 429 ) = 'BODY514_RADII    = (  55      55    45 ' &
      //            ') '
      PCK ( 430 ) = 'BODY515_RADII    = (  13      10     8 ' &
      //            ') '
      PCK ( 431 ) = 'BODY516_RADII    = (  20      20    20 ' &
      //            ') '
      PCK ( 432 ) = 'BODY601_RADII     = (      210.3 ' &
      //            '197.4        192.6    ) '
      PCK ( 433 ) = 'BODY602_RADII     = (      256.2 ' &
      //            '247.3        244.0    ) '
      PCK ( 434 ) = 'BODY603_RADII     = (      523. ' &
      //            '523.         523.     ) '
      PCK ( 435 ) = 'BODY604_RADII     = (      560. ' &
      //            '560.         560.     ) '
      PCK ( 436 ) = 'BODY605_RADII     = (      764. ' &
      //            '764.         764.     ) '
      PCK ( 437 ) = 'BODY606_RADII     = (     2575. ' &
      //            '2575.        2575.     ) '
      PCK ( 438 ) = 'BODY607_RADII     = (      180. ' &
      //            '140.         112.5    ) '
      PCK ( 439 ) = 'BODY608_RADII     = (      718. ' &
      //            '718.         718.     ) '
      PCK ( 440 ) = 'BODY609_RADII     = (      115. ' &
      //            '110.         105.     ) '
      PCK ( 441 ) = 'BODY610_RADII     = (       97. ' &
      //            '95.          77.     ) '
      PCK ( 442 ) = 'BODY611_RADII     = (       69. ' &
      //            '55.          55.     ) '
      PCK ( 443 ) = 'BODY612_RADII     = (       17.5 ' &
      //            '16.          15.     ) '
      PCK ( 444 ) = 'BODY613_RADII     = (       15 ' &
      //            '12.5          7.5    ) '
      PCK ( 445 ) = 'BODY614_RADII     = (       15 ' &
      //            '8            8      ) '
      PCK ( 446 ) = 'BODY615_RADII     = (       18.5 ' &
      //            '17.2         13.5    ) '
      PCK ( 447 ) = 'BODY616_RADII     = (       74 ' &
      //            '50           34      ) '
      PCK ( 448 ) = 'BODY617_RADII     = (       55 ' &
      //            '44           31      ) '
      PCK ( 449 ) = 'BODY701_RADII     = (      581.1 ' &
      //            '577.9        577.7    ) '
      PCK ( 450 ) = 'BODY702_RADII     = (      584.7 ' &
      //            '584.7        584.7    ) '
      PCK ( 451 ) = 'BODY703_RADII     = (      788.9 ' &
      //            '788.9        788.9    ) '
      PCK ( 452 ) = 'BODY704_RADII     = (      761.4 ' &
      //            '761.4        761.4    ) '
      PCK ( 453 ) = 'BODY705_RADII     = (      240.4 ' &
      //            '234.2        232.9    ) '
      PCK ( 454 ) = 'BODY706_RADII     = (      13. ' &
      //            '13.          13.    ) '
      PCK ( 455 ) = 'BODY707_RADII     = (      15. ' &
      //            '15.          15.    ) '
      PCK ( 456 ) = 'BODY708_RADII     = (      21. ' &
      //            '21.          21.    ) '
      PCK ( 457 ) = 'BODY709_RADII     = (      31. ' &
      //            '31.          31.    ) '
      PCK ( 458 ) = 'BODY710_RADII     = (      27. ' &
      //            '27.          27.    ) '
      PCK ( 459 ) = 'BODY711_RADII     = (      42. ' &
      //            '42.          42.    ) '
      PCK ( 460 ) = 'BODY712_RADII     = (      54. ' &
      //            '54.          54.    ) '
      PCK ( 461 ) = 'BODY713_RADII     = (      27. ' &
      //            '27.          27.    ) '
      PCK ( 462 ) = 'BODY714_RADII     = (      33. ' &
      //            '33.          33.    ) '
      PCK ( 463 ) = 'BODY715_RADII     = (      77. ' &
      //            '77.          77.    ) '
      PCK ( 464 ) = 'BODY801_RADII     = (     1750. ' &
      //            '1750.        1750.     ) '
      PCK ( 465 ) = 'BODY802_RADII     = (      345. ' &
      //            '345.         345.     ) '
      PCK ( 466 ) = 'BODY901_RADII     = (      606. ' &
      //            '606.         606.     ) '


!
!     Create the SCLK kernel.
!
      CALL TXTOPN ( NAMEPC, UNIT )

      DO I = 1, NLINES
         R =     RTRIM ( PCK(I) )
         WRITE (UNIT,*)  PCK(I)(1:R)
      END DO

      CLOSE ( UNIT )
!
!     If this file needs to be loaded.  Do it now.  If not we are
!     done and can return.
!
      IF ( LOADPC ) THEN
         CALL LDPOOL ( NAMEPC )

         IF ( KEEPPC ) THEN
!
!           If we are keeping this file, we need to register it
!           with FILREG.
!
            CALL TFILES ( NAMEPC )
            RETURN
         END IF

      ELSE
!
!        We are keeping this file, so we need to register it
!        with FILREG.
!
         CALL TFILES ( NAMEPC )
         RETURN
      END IF


      CALL KILFIL ( NAMEPC )

      RETURN
      END
