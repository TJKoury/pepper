!$Procedure ZZSGP4 ( SGP4 wrapper )

      SUBROUTINE ZZSGP4 ( GEOPHS, ELEMS, OPMODE, T, STATE )

! $ Abstract
!
!     Umbrella for the SGP4 initializer and evaluator routines.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzsgp4.inc'

      DOUBLE PRECISION         GEOPHS    ( 8 )
      DOUBLE PRECISION         ELEMS     ( 10 )
      INTEGER                  OPMODE
      DOUBLE PRECISION         T
      DOUBLE PRECISION         STATE     ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     GEOPHS     I   XXSGP4I
!     ELEMS      I   XXSGP4I
!     OPMODE     I   XXSGP4I
!     T          I   XXSGP4E
!     STATE      O   XXSGP4E
!
! $ Detailed_Input
!
!     See Individual Entry points.
!
! $ Detailed_Output
!
!     See Individual Entry points.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If ZZSGP4 is called, the error SPICE(BOGUSENTRY) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine wraps XXSGP4E and XXSGP4I. As entry points to
!     this routine, they share the local memory space.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     Use a set of TLEs to calculate a collection of states for a
!     time interval centered at the TLE set epoch.
!
!           PROGRAM ZZSGP4_T
!
!     C
!     C     Read a data file containing sets of TLEs, then calculate
!     C     states at -1440 to 1440 minutes from the epoch of each
!     C     TLE set in steps of 10 minutes.
!     C
!     C     Example cases listed in sgp4-ver.tle.
!     C
!     C     1 00005U 58002B   00179.78495062  .00000023
!     C       00000-0  28098-4 0  4753
!     C     2 00005  34.2682 348.7242 1859667 331.7664
!     C       19.3264 10.82419157413667
!     C
!     C     1 04632U 70093B   04031.91070959 -.00000084
!     C       00000-0  10000-3 0  9955
!     C     2 04632  11.4628 273.1101 1450506 207.6000
!     C       143.9350  1.20231981 44145
!     C
!     C     1 06251U 62025E   06176.82412014  .00008885
!     C       00000-0  12808-3 0  3985
!     C     2 06251  58.0579  54.0425 0030035 139.1568
!     C       221.1854 15.56387291  6774
!     C
!     C     1 08195U 75081A   06176.33215444  .00000099
!     C       00000-0  11873-3 0   813
!     C     2 08195  64.1586 279.0717 6877146 264.7651
!     C       20.2257  2.00491383225656
!     C
!     C     1 09880U 77021A   06176.56157475  .00000421
!     C       00000-0  10000-3 0  9814
!     C     2 09880  64.5968 349.3786 7069051 270.0229
!     C       16.3320  2.00813614112380
!     C
!     C     1 09998U 74033F   05148.79417928 -.00000112
!     C       00000-0  00000+0 0  4480
!     C     2 09998   9.4958 313.1750 0270971 327.5225
!     C       30.8097  1.16186785 45878
!     C
!     C     1 11801U          80230.29629788  .01431103
!     C       00000-0  14311-1      13
!     C     2 11801  46.7916 230.4354 7318036  47.4722
!     C       10.4117  2.28537848    13
!     C
!     C     1 14128U 83058A   06176.02844893 -.00000158
!     C       00000-0  10000-3 0  9627
!     C     2 14128  11.4384  35.2134 0011562  26.4582
!     C       333.5652  0.98870114 46093
!     C
!     C     1 16925U 86065D   06151.67415771  .02550794
!     C       -30915-6  18784-3 0  4486
!     C     2 16925  62.0906 295.0239 5596327 245.1593
!     C       47.9690  4.88511875148616
!     C
!     C     1 20413U 83020D   05363.79166667  .00000000
!     C       00000-0  00000+0 0  7041
!     C     2 20413  12.3514 187.4253 7864447 196.3027
!     C       356.5478  0.24690082  7978
!     C
!     C     1 21897U 92011A   06176.02341244 -.00001273
!     C       00000-0 -13525-3 0  3044
!     C     2 21897  62.1749 198.0096 7421690 253.0462
!     C       20.1561  2.01269994104880
!     C
!     C     1 22312U 93002D   06094.46235912  .99999999
!     C       81888-5  49949-3 0  3953
!     C     2 22312  62.1486  77.4698 0308723 267.9229
!     C       88.7392 15.95744531 98783
!     C
!     C     1 22674U 93035D   06176.55909107  .00002121
!     C       00000-0  29868-3 0  6569
!     C     2 22674  63.5035 354.4452 7541712 253.3264
!     C       18.7754  1.96679808 93877
!     C
!     C     1 23177U 94040C   06175.45752052  .00000386
!     C       00000-0  76590-3 0    95
!     C     2 23177   7.0496 179.8238 7258491 296.0482
!     C       8.3061  2.25906668 97438
!     C
!     C     1 23333U 94071A   94305.49999999 -.00172956
!     C       26967-3  10000-3 0    15
!     C     2 23333  28.7490   2.3720 9728298  30.4360
!     C       1.3500  0.07309491    70
!     C
!     C     1 23599U 95029B   06171.76535463  .00085586
!     C       12891-6  12956-2 0  2905
!     C     2 23599   6.9327   0.2849 5782022 274.4436
!     C       25.2425  4.47796565123555
!     C
!     C     1 24208U 96044A   06177.04061740 -.00000094
!     C       00000-0  10000-3 0  1600
!     C     2 24208   3.8536  80.0121 0026640 311.0977
!     C       48.3000  1.00778054 36119
!     C
!     C     1 25954U 99060A   04039.68057285 -.00000108
!     C       00000-0  00000-0 0  6847
!     C     2 25954   0.0004 243.8136 0001765  15.5294
!     C       22.7134  1.00271289 15615
!     C
!     C     1 26900U 01039A   06106.74503247  .00000045
!     C       00000-0  10000-3 0  8290
!     C     2 26900   0.0164 266.5378 0003319  86.1794
!     C       182.2590  1.00273847 16981
!     C
!     C     1 26975U 78066F   06174.85818871  .00000620
!     C       00000-0  10000-3 0  6809
!     C     2 26975  68.4714 236.1303 5602877 123.7484
!     C       302.5767  2.05657553 67521
!     C
!     C     1 28057U 03049A   06177.78615833  .00000060
!     C       00000-0  35940-4 0  1836
!     C     2 28057  98.4283 247.6961 0000884  88.1964
!     C       271.9322 14.35478080140550
!     C
!     C     1 28129U 03058A   06175.57071136 -.00000104
!     C       00000-0  10000-3 0   459
!     C     2 28129  54.7298 324.8098 0048506 266.2640
!     C       93.1663  2.00562768 18443
!     C
!     C     1 28350U 04020A   06167.21788666  .16154492
!     C       76267-5  18678-3 0  8894
!     C     2 28350  64.9977 345.6130 0024870 260.7578
!     C       99.9590 16.47856722116490
!     C
!     C     1 28623U 05006B   06177.81079184  .00637644
!     C       69054-6  96390-3 0  6000
!     C     2 28623  28.5200 114.9834 6249053 170.2550
!     C       212.8965  3.79477162 12753
!     C
!     C     1 28626U 05008A   06176.46683397 -.00000205
!     C       00000-0  10000-3 0  2190
!     C     2 28626   0.0019 286.9433 0000335  13.7918
!     C       55.6504  1.00270176  4891
!     C
!     C     1 28872U 05037B   05333.02012661  .25992681
!     C       00000-0  24476-3 0  1534
!     C     2 28872  96.4736 157.9986 0303955 244.0492
!     C       110.6523 16.46015938 10708
!     C
!     C     1 29141U 85108AA  06170.26783845  .99999999
!     C       00000-0  13519-0 0   718
!     C     2 29141  82.4288 273.4882 0015848 277.2124
!     C       83.9133 15.93343074  6828
!     C
!     C     1 29238U 06022G   06177.28732010  .00766286
!     C       10823-4  13334-2 0   101
!     C     2 29238  51.5595 213.7903 0202579  95.2503
!     C       267.9010 15.73823839  1061
!     C
!     C     1 88888U          80275.98708465  .00073094
!     C       13844-3  66816-4 0    87
!     C     2 88888  72.8435 115.9689 0086731  52.6988
!     C       110.5714 16.05824518  1058
!     C
!
!           IMPLICIT NONE
!
!           INCLUDE 'zzsgp4.inc'
!
!           CHARACTER*(72)           LINES  ( 2 )
!           CHARACTER*(72)           TLEDAT
!
!           INTEGER                  FRSTYR
!           INTEGER                  I
!           INTEGER                  OPMODE
!
!           DOUBLE PRECISION         DELT
!           DOUBLE PRECISION         ELEMS  ( 10 )
!           DOUBLE PRECISION         EPOCH
!           DOUBLE PRECISION         GEOPHS ( 8 )
!           DOUBLE PRECISION         STATE  ( 6 )
!           DOUBLE PRECISION         TF
!           DOUBLE PRECISION         TIME
!           DOUBLE PRECISION         TS
!
!           LOGICAL                  EOF
!
!     C
!     C     SPICELIB routines.
!     C
!           LOGICAL                  FAILED
!
!     C
!     C     Load a leapseconds kernel for time conversion. Required
!     C     by the SPK 10 evaluator.
!     C
!           CALL FURNSH ( '/kernels/gen/lsk/naif0011.tls' )
!
!     C
!     C     Define the geophysical quantities using the values
!     C     from geophysical.ker.
!     C
!           GEOPHS( K_J2 ) =    1.082616D-3
!           GEOPHS( K_J3 ) =   -2.53881D-6
!           GEOPHS( K_J4 ) =   -1.65597D-6
!           GEOPHS( K_KE ) =    7.43669161D-2
!           GEOPHS( K_QO ) =  120.0D0
!           GEOPHS( K_SO ) =   78.0D0
!           GEOPHS( K_ER ) = 6378.135D0
!           GEOPHS( K_AE ) =    1.0D0
!
!           TLEDAT = 'sgp4-ver1.tle'
!
!     C
!     C     Error subsystem to report to ensure execution continues
!     C     if an error signals.
!     C
!           CALL ERRACT( 'SET', 'REPORT')
!
!     C
!     C     Use Spacetrack #3 algorithm to calculate sidereal time.
!     C
!           OPMODE = AFSPC
!
!     C
!     C     Identify the earliest year for the elements.
!     C
!           FRSTYR = 1958
!
!     C
!     C     Start and final offsets from TLE epochs. [-1440, 1400]
!     C     minutes.
!     C
!           TS     =  -1440.0D0
!           TF     =   1440.0D0
!
!     C
!     C     Step size for elements output 10 minutes.
!     C
!           DELT   = 10.D0
!
!     C
!     C     Read the TLE data file.
!     C
!           CALL RDTEXT ( TLEDAT, LINES(1), EOF )
!           CALL RDTEXT ( TLEDAT, LINES(2), EOF )
!
!     C
!     C     Loop over data file until end-of-file.
!     C
!           DO WHILE ( .NOT. EOF )
!
!     C
!     C        Parse the elements to something SPICE can use.
!     C
!              CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
!
!              WRITE(*, FMT='(A72)') LINES(1)
!              WRITE(*, FMT='(A72)') LINES(2)
!              WRITE(*,*) ' '
!
!     C
!     C        Initialize SGP4 calculations based on values in
!     C        GEOPHS, ELEMS, and AFSPC.
!     C
!              CALL XXSGP4I ( GEOPHS, ELEMS, OPMODE )
!
!     C
!     C        Start time keyed in minutes from TLE epoch.
!     C
!              TIME   = TS
!
!              DO WHILE ( TIME .LE. DABS(TF) .AND. (.NOT. FAILED()) )
!
!     C
!     C           Calculate the STATE at TIME.
!     C
!                 CALL XXSGP4E ( TIME, STATE )
!
!     C
!     C           If the propagation succeeded, output the STATE.
!     C
!                 IF ( .NOT. FAILED() ) THEN
!
!                    WRITE(*, FMT ='(7F17.8)' ) TIME,
!          .                                    (STATE(I),I=1,6)
!
!                 END IF
!
!     C
!     C           Increment the evaluation time by one step.
!     C
!                 TIME = TIME + DELT
!
!              END DO
!
!              WRITE(*,*) ' '
!
!     C
!     C        reset the error subsystem for the next loop.
!     C
!              CALL RESET()
!
!     C
!     C        Read the next two lines (if any) from the TLE
!     C        data file.
!     C
!              CALL RDTEXT ( TLEDAT, LINES(1), EOF )
!              CALL RDTEXT ( TLEDAT, LINES(2), EOF )
!
!           END DO
!
!           END
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for
!         Propagation of the NORAD Element Sets." Spacetrack Report #3.
!         U.S. Air Force: Aerospace Defense Command.
!
!     [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation
!         of Space Command Element Sets." Space Command,
!         U. S. Air Force, CO.
!
!     [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004.
!         History of Analytical Orbit Modeling in the U. S. Space
!         Surveillance System. Journal of Guidance, Control, and
!         Dynamics. 27(2):174-185.
!
!     [4] Vallado, David, Crawford, Paul, Hujsak, Richard,
!         and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper
!         AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics
!         Specialist Conference, August 21-24, 2006. Keystone, CO.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 30-MAY-2021 (EDW) (JDR)
!
!        Correction of documentation error in listing of GEOPHS
!        constants. The definition of index 5, "High altitude bound
!        for atmospheric model in km.", and index 6, "Low altitude
!        bound for atmospheric model in km." were switched.
!
!-    SPICELIB Version 1.0.0, 15-SEP-2014 (EDW)
!
!        Based on routine SGP4, 28-JUN-2005, Vallado 2006 [4].
!
!-&


! $ Index_Entries
!
!     SGP4
!
!-&


!
!     Local Variables
!

      DOUBLE PRECISION         A
      DOUBLE PRECISION         ALTA
      DOUBLE PRECISION         ALTP
      DOUBLE PRECISION         ARGPDOT
      DOUBLE PRECISION         AYCOF
      DOUBLE PRECISION         CC1
      DOUBLE PRECISION         CC4
      DOUBLE PRECISION         CC5
      DOUBLE PRECISION         CON41
      DOUBLE PRECISION         ETA
      DOUBLE PRECISION         INCLO
      DOUBLE PRECISION         MDOT
      DOUBLE PRECISION         NODEDOT
      DOUBLE PRECISION         OMGCOF
      DOUBLE PRECISION         SINMAO
      DOUBLE PRECISION         T2COF
      DOUBLE PRECISION         T3COF
      DOUBLE PRECISION         T4COF
      DOUBLE PRECISION         T5COF
      DOUBLE PRECISION         X1MTH2
      DOUBLE PRECISION         X7THM1
      DOUBLE PRECISION         XLCOF
      DOUBLE PRECISION         XMCOF
      DOUBLE PRECISION         XNODCF

!
!     DS values
!

      DOUBLE PRECISION         E3
      DOUBLE PRECISION         EE2
      DOUBLE PRECISION         PEO
      DOUBLE PRECISION         PGHO
      DOUBLE PRECISION         PHO
      DOUBLE PRECISION         PINCO
      DOUBLE PRECISION         PLO
      DOUBLE PRECISION         GSTO
      DOUBLE PRECISION         XFACT
      DOUBLE PRECISION         XGH2
      DOUBLE PRECISION         XGH3
      DOUBLE PRECISION         XGH4
      DOUBLE PRECISION         XH2
      DOUBLE PRECISION         XH3
      DOUBLE PRECISION         XI2
      DOUBLE PRECISION         XI3
      DOUBLE PRECISION         XL2
      DOUBLE PRECISION         XL3
      DOUBLE PRECISION         XL4
      DOUBLE PRECISION         XLAMO
      DOUBLE PRECISION         ZMOL
      DOUBLE PRECISION         ZMOS
      DOUBLE PRECISION         ATIME
      DOUBLE PRECISION         XLI
      DOUBLE PRECISION         XNI

      DOUBLE PRECISION         AINV
      DOUBLE PRECISION         AM
      DOUBLE PRECISION         AO
      DOUBLE PRECISION         ARGPM
      DOUBLE PRECISION         ARGPO
      DOUBLE PRECISION         ARGPP
      DOUBLE PRECISION         AXNL
      DOUBLE PRECISION         AYNL
      DOUBLE PRECISION         BETAL
      DOUBLE PRECISION         BSTAR
      DOUBLE PRECISION         CC1SQ
      DOUBLE PRECISION         CC2
      DOUBLE PRECISION         CC3
      DOUBLE PRECISION         CNOD
      DOUBLE PRECISION         CNODM
      DOUBLE PRECISION         COEF
      DOUBLE PRECISION         COEF1
      DOUBLE PRECISION         CON42
      DOUBLE PRECISION         COS2U
      DOUBLE PRECISION         COSEO1
      DOUBLE PRECISION         COSI
      DOUBLE PRECISION         COSIM
      DOUBLE PRECISION         COSIO
      DOUBLE PRECISION         COSIO2
      DOUBLE PRECISION         COSIO4
      DOUBLE PRECISION         COSIP
      DOUBLE PRECISION         COSISQ
      DOUBLE PRECISION         COSOMM
      DOUBLE PRECISION         COSSU
      DOUBLE PRECISION         COSU
      DOUBLE PRECISION         D2
      DOUBLE PRECISION         D2201
      DOUBLE PRECISION         D2211
      DOUBLE PRECISION         D3
      DOUBLE PRECISION         D3210
      DOUBLE PRECISION         D3222
      DOUBLE PRECISION         D4
      DOUBLE PRECISION         D4410
      DOUBLE PRECISION         D4422
      DOUBLE PRECISION         D5220
      DOUBLE PRECISION         D5232
      DOUBLE PRECISION         D5421
      DOUBLE PRECISION         D5433
      DOUBLE PRECISION         DAY
      DOUBLE PRECISION         DEDT
      DOUBLE PRECISION         DEL1
      DOUBLE PRECISION         DEL2
      DOUBLE PRECISION         DEL3
      DOUBLE PRECISION         DELM
      DOUBLE PRECISION         DELMO
      DOUBLE PRECISION         DELOMG
      DOUBLE PRECISION         DIDT
      DOUBLE PRECISION         DMDT
      DOUBLE PRECISION         DNDT
      DOUBLE PRECISION         DNODT
      DOUBLE PRECISION         DOMDT
      DOUBLE PRECISION         ECCM
      DOUBLE PRECISION         ECCO
      DOUBLE PRECISION         ECCP
      DOUBLE PRECISION         ECCSQ
      DOUBLE PRECISION         ECOSE
      DOUBLE PRECISION         EETA
      DOUBLE PRECISION         EL2
      DOUBLE PRECISION         EMSQ
      DOUBLE PRECISION         EO1
      DOUBLE PRECISION         EPOCH
      DOUBLE PRECISION         ER
      DOUBLE PRECISION         ESINE
      DOUBLE PRECISION         ETASQ
      DOUBLE PRECISION         GAM
      DOUBLE PRECISION         INCLM
      DOUBLE PRECISION         J2
      DOUBLE PRECISION         J3
      DOUBLE PRECISION         J3OJ2
      DOUBLE PRECISION         J4
      DOUBLE PRECISION         KPS
      DOUBLE PRECISION         MM
      DOUBLE PRECISION         MO
      DOUBLE PRECISION         MP
      DOUBLE PRECISION         MR
      DOUBLE PRECISION         MV
      DOUBLE PRECISION         NO
      DOUBLE PRECISION         NODEM
      DOUBLE PRECISION         NODEO
      DOUBLE PRECISION         NODEP
      DOUBLE PRECISION         OMEOSQ
      DOUBLE PRECISION         OMGADF
      DOUBLE PRECISION         PERIGE
      DOUBLE PRECISION         PINVSQ
      DOUBLE PRECISION         PL
      DOUBLE PRECISION         POSQ
      DOUBLE PRECISION         PSISQ
      DOUBLE PRECISION         QZMS24
      DOUBLE PRECISION         QZMS2T
      DOUBLE PRECISION         RDOTL
      DOUBLE PRECISION         RL
      DOUBLE PRECISION         RP
      DOUBLE PRECISION         RTEMSQ
      DOUBLE PRECISION         RTEOSQ
      DOUBLE PRECISION         RVDOT
      DOUBLE PRECISION         RVDOTL
      DOUBLE PRECISION         S1
      DOUBLE PRECISION         S2
      DOUBLE PRECISION         S3
      DOUBLE PRECISION         S4
      DOUBLE PRECISION         S5
      DOUBLE PRECISION         S6
      DOUBLE PRECISION         S7
      DOUBLE PRECISION         SE2
      DOUBLE PRECISION         SE3
      DOUBLE PRECISION         SFOUR
      DOUBLE PRECISION         SGH2
      DOUBLE PRECISION         SGH3
      DOUBLE PRECISION         SGH4
      DOUBLE PRECISION         SH2
      DOUBLE PRECISION         SH3
      DOUBLE PRECISION         SI2
      DOUBLE PRECISION         SI3
      DOUBLE PRECISION         SIN2U
      DOUBLE PRECISION         SINEO1
      DOUBLE PRECISION         SINI
      DOUBLE PRECISION         SINIM
      DOUBLE PRECISION         SINIO
      DOUBLE PRECISION         SINIP
      DOUBLE PRECISION         SINOMM
      DOUBLE PRECISION         SINSU
      DOUBLE PRECISION         SINU
      DOUBLE PRECISION         SL2
      DOUBLE PRECISION         SL3
      DOUBLE PRECISION         SL4
      DOUBLE PRECISION         SNOD
      DOUBLE PRECISION         SNODM
      DOUBLE PRECISION         SS
      DOUBLE PRECISION         SS1
      DOUBLE PRECISION         SS2
      DOUBLE PRECISION         SS3
      DOUBLE PRECISION         SS4
      DOUBLE PRECISION         SS5
      DOUBLE PRECISION         SS6
      DOUBLE PRECISION         SS7
      DOUBLE PRECISION         SU
      DOUBLE PRECISION         SZ1
      DOUBLE PRECISION         SZ11
      DOUBLE PRECISION         SZ12
      DOUBLE PRECISION         SZ13
      DOUBLE PRECISION         SZ2
      DOUBLE PRECISION         SZ21
      DOUBLE PRECISION         SZ22
      DOUBLE PRECISION         SZ23
      DOUBLE PRECISION         SZ3
      DOUBLE PRECISION         SZ31
      DOUBLE PRECISION         SZ32
      DOUBLE PRECISION         SZ33
      DOUBLE PRECISION         T2
      DOUBLE PRECISION         T3
      DOUBLE PRECISION         T4
      DOUBLE PRECISION         TC
      DOUBLE PRECISION         TEM5
      DOUBLE PRECISION         TEMP
      DOUBLE PRECISION         TEMP1
      DOUBLE PRECISION         TEMP2
      DOUBLE PRECISION         TEMP3
      DOUBLE PRECISION         TEMP4
      DOUBLE PRECISION         TEMPA
      DOUBLE PRECISION         TEMPE
      DOUBLE PRECISION         TEMPL
      DOUBLE PRECISION         TSI
      DOUBLE PRECISION         TUMIN
      DOUBLE PRECISION         TVEC   ( 8 )
      DOUBLE PRECISION         TZERO
      DOUBLE PRECISION         U
      DOUBLE PRECISION         UX
      DOUBLE PRECISION         UY
      DOUBLE PRECISION         UZ
      DOUBLE PRECISION         VX
      DOUBLE PRECISION         VY
      DOUBLE PRECISION         VZ
      DOUBLE PRECISION         X2O3
      DOUBLE PRECISION         XHDOT1
      DOUBLE PRECISION         XINC
      DOUBLE PRECISION         XINCP
      DOUBLE PRECISION         XKE
      DOUBLE PRECISION         XL
      DOUBLE PRECISION         XLM
      DOUBLE PRECISION         XMDF
      DOUBLE PRECISION         XMX
      DOUBLE PRECISION         XMY
      DOUBLE PRECISION         XN
      DOUBLE PRECISION         XNODDF
      DOUBLE PRECISION         XNODE
      DOUBLE PRECISION         XPIDOT
      DOUBLE PRECISION         Z1
      DOUBLE PRECISION         Z11
      DOUBLE PRECISION         Z12
      DOUBLE PRECISION         Z13
      DOUBLE PRECISION         Z2
      DOUBLE PRECISION         Z21
      DOUBLE PRECISION         Z22
      DOUBLE PRECISION         Z23
      DOUBLE PRECISION         Z3
      DOUBLE PRECISION         Z31
      DOUBLE PRECISION         Z32
      DOUBLE PRECISION         Z33

      INTEGER                  ITER
      INTEGER                  SVMODE
      INTEGER                  IREZ

      LOGICAL                  DOINIT
      LOGICAL                  DOSIMP
      LOGICAL                  DODEEP

!
!     SPICELIB routines.
!

      DOUBLE PRECISION         PI
      DOUBLE PRECISION         TWOPI

      LOGICAL                  FAILED
      LOGICAL                  RETURN

      SAVE                     A
      SAVE                     ALTA
      SAVE                     ALTP
      SAVE                     ARGPDOT
      SAVE                     ARGPO
      SAVE                     ATIME
      SAVE                     AYCOF
      SAVE                     BSTAR
      SAVE                     CC1
      SAVE                     CC4
      SAVE                     CC5
      SAVE                     CON41
      SAVE                     D2
      SAVE                     D2201
      SAVE                     D2211
      SAVE                     D3
      SAVE                     D3210
      SAVE                     D3222
      SAVE                     D4
      SAVE                     D4410
      SAVE                     D4422
      SAVE                     D5220
      SAVE                     D5232
      SAVE                     D5421
      SAVE                     D5433
      SAVE                     DEDT
      SAVE                     DEL1
      SAVE                     DEL2
      SAVE                     DEL3
      SAVE                     DELMO
      SAVE                     DIDT
      SAVE                     DMDT
      SAVE                     DNODT
      SAVE                     DODEEP
      SAVE                     DOMDT
      SAVE                     DOSIMP
      SAVE                     E3
      SAVE                     ECCO
      SAVE                     EE2
      SAVE                     ER
      SAVE                     ETA
      SAVE                     GSTO
      SAVE                     INCLO
      SAVE                     IREZ
      SAVE                     J2
      SAVE                     J3OJ2
      SAVE                     MDOT
      SAVE                     MO
      SAVE                     NO
      SAVE                     NODEDOT
      SAVE                     NODEO
      SAVE                     OMGCOF
      SAVE                     PEO
      SAVE                     PGHO
      SAVE                     PHO
      SAVE                     PINCO
      SAVE                     PLO
      SAVE                     SE2
      SAVE                     SE3
      SAVE                     SGH2
      SAVE                     SGH3
      SAVE                     SGH4
      SAVE                     SH2
      SAVE                     SH3
      SAVE                     SI2
      SAVE                     SI3
      SAVE                     SINMAO
      SAVE                     SL2
      SAVE                     SL3
      SAVE                     SL4
      SAVE                     SVMODE
      SAVE                     T2COF
      SAVE                     T3COF
      SAVE                     T4COF
      SAVE                     T5COF
      SAVE                     X1MTH2
      SAVE                     X7THM1
      SAVE                     XFACT
      SAVE                     XGH2
      SAVE                     XGH3
      SAVE                     XGH4
      SAVE                     XH2
      SAVE                     XH3
      SAVE                     XI2
      SAVE                     XI3
      SAVE                     XKE
      SAVE                     XL2
      SAVE                     XL3
      SAVE                     XL4
      SAVE                     XLAMO
      SAVE                     XLCOF
      SAVE                     XLI
      SAVE                     XMCOF
      SAVE                     XNI
      SAVE                     XNODCF
      SAVE                     ZMOL
      SAVE                     ZMOS

      CALL CHKIN  ( 'ZZSGP4' )
      CALL SETMSG ( 'The routine ZZSGP4 is an umbrella for ' &
      //            'the SGP4 initializer and propagator entry ' &
      //            'points. Do not call ZZSGP4. It is ' &
      //            'likely that a programming error has ' &
      //            'been made.' )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)'  )
      CALL CHKOUT ( 'ZZSGP4' )

      RETURN



!$Procedure XXSGP4I ( SGP4 initializer )

      ENTRY XXSGP4I ( GEOPHS, ELEMS, OPMODE )

! $ Abstract
!
!     This subroutine initializes variables for SGP4.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations
!
!     DOUBLE PRECISION         GEOPHS    ( 8 )
!     DOUBLE PRECISION         ELEMS     ( 10 )
!     INTEGER                  OPMODE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     GEOPHS     I   Geophysical constants.
!     ELEMS      I   Two-line element data.
!     OPMODE     I   Flag to indicate operation mode for GMST.
!
! $ Detailed_Input
!
!     GEOPHS   is a collection of 8 geophysical constants needed
!              for computing a state. The order of these
!              constants must be:
!
!              GEOPHS(1) = J2 gravitational harmonic for earth
!              GEOPHS(2) = J3 gravitational harmonic for earth
!              GEOPHS(3) = J4 gravitational harmonic for earth
!
!              These first three constants are dimensionless.
!
!              GEOPHS(4) = KE: Square root of the GM for earth where
!                          GM is expressed in earth radii cubed per
!                          minutes squared.
!
!              GEOPHS(5) = QO: High altitude bound for atmospheric
!                          model in km.
!
!              GEOPHS(6) = SO: Low altitude bound for atmospheric
!                          model in km.
!
!              GEOPHS(7) = RE: Equatorial radius of the earth in km.
!
!              GEOPHS(8) = AE: Distance units/earth radius
!                          (normally 1)
!
!              Below are currently recommended values for these
!              items:
!
!                J2 =    1.082616D-3
!                J3 =   -2.53881D-6
!                J4 =   -1.65597D-6
!
!              The next item is the square root of GM for the
!              earth given in units of earth-radii**1.5/Minute
!
!                KE =    7.43669161D-2
!
!              The next two items give the top and
!              bottom of the atmospheric drag model
!              used by the type 10 ephemeris type.
!              Don't adjust these unless you understand
!              the full implications of such changes.
!
!                QO =  120.0D0
!                SO =   78.0D0
!
!              The following is the equatorial radius
!              of the earth as used by NORAD in km.
!
!                ER = 6378.135D0
!
!              The value of AE is the number of
!              distance units per earth radii used by
!              the NORAD state propagation software.
!              The value should be 1 unless you've got
!              a very good understanding of the NORAD
!              routine SGP4 and the affect of changing
!              this value..
!
!                AE =    1.0D0
!
!     ELEMS    is an array containing two-line element data
!              as prescribed below. The elements XNDD6O and BSTAR
!              must already be scaled by the proper exponent stored
!              in the two line elements set. Moreover, the
!              various items must be converted to the units shown
!              here.
!
!                 ELEMS (  1 ) = XNDT2O in radians/minute**2
!                 ELEMS (  2 ) = XNDD6O in radians/minute**3
!                 ELEMS (  3 ) = BSTAR
!                 ELEMS (  4 ) = XINCL  in radians
!                 ELEMS (  5 ) = XNODEO in radians
!                 ELEMS (  6 ) = EO
!                 ELEMS (  7 ) = OMEGAO in radians
!                 ELEMS (  8 ) = XMO    in radians
!                 ELEMS (  9 ) = XNO    in radians/minute
!                 ELEMS ( 10 ) = EPOCH of the elements in seconds
!                                past ephemeris epoch J2000.
!
!     OPMODE   Flag indicating which technique
!              to use to calculate sidereal time.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If radius of perigee has value less-than 1, the error
!         SPICE(SUBORBITAL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is based on the SGP4INIT code by David Vallado
!     corresponding to "Revisiting Spacetrack Report #3".
!     The intent is to maintain the original Vallado algorithm,
!     changing code only to meet NAIF format standards and to
!     integrate with SPICELIB.
!
!        1) Implemented error checks using SPICE error subsystem.
!           On detecting an error, control returns to the calling
!           routine. This behavior differs from the original
!           version.
!
!        2) Comments prefixed with "SGP4FIX" indicate a comment
!           from the code by Vallado et. al concerning a correction
!           to the STR#3 code.
!
!        3) Eliminated the use of COMMON blocks.
!
!        Removed getgravconst call, replaced with GEOPHS array.
!
!        xBStar,
!        xEcco,
!        Epoch,
!        xArgpo,
!        xInclo,
!        xMo,
!        xNo,
!        xnodeo replaced with ELEMS array.
!        radiusearthkm replaced with ER
!
! $ Examples
!
!     Refer to $Examples section in ZZSGP4.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for
!         Propagation of the NORAD Element Sets." Spacetrack Report #3.
!         U.S. Air Force: Aerospace Defense Command.
!
!     [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation
!         of Space Command Element Sets." Space Command,
!         U. S. Air Force, CO.
!
!     [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004.
!         History of Analytical Orbit Modeling in the U. S. Space
!         Surveillance System. Journal of Guidance, Control, and
!         Dynamics. 27(2):174-185.
!
!     [4] Vallado, David, Crawford, Paul, Hujsak, Richard,
!         and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper
!         AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics
!         Specialist Conference, August 21-24, 2006. Keystone, CO.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 30-MAY-2021 (EDW) (JDR)
!
!        Correction of documentation error in listing of GEOPHS
!        constants. The definition of index 5, "High altitude bound
!        for atmospheric model in km.", and index 6, "Low altitude
!        bound for atmospheric model in km." were switched.
!
!-    SPICELIB Version 1.0.0, 11-DEC-2014 (EDW)
!
!        Based on routine SGP4INIT, 28-JUN-2005, Vallado 2006 [4].
!
!-&


! $ Index_Entries
!
!     SGP4
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'XXSGP4I' )

!
!     Initialize.
!
      DODEEP = .FALSE.
      DOSIMP = .FALSE.
      SVMODE = OPMODE

!
!     This code block replaces the call:
!
!     sgp4fix - note the following variables are also passed directly
!     via sgp4 common. It is possible to streamline the XXSGP4I call
!     by deleting the "x" variables, but the user would need to set
!     the common values first. we include the additional assignment
!     in case twoline2rv is not used.
!
!        bstar  = xbstar
!        ecco   = xecco
!        argpo  = xargpo
!        inclo  = xinclo
!        mo     = xmo
!        no     = xno
!        nodeo  = xnodeo

      BSTAR = ELEMS ( KBSTAR )
      INCLO = ELEMS ( KINCL  )
      NODEO = ELEMS ( KNODE0 )
      ECCO  = ELEMS ( KECC   )
      ARGPO = ELEMS ( KOMEGA )
      MO    = ELEMS ( KMO    )
      NO    = ELEMS ( KNO    )

!
!       Remember that sgp4 uses units of days from 0 jan 1950
!       (sgp4epoch) and minutes from the epoch (time)
!
!       2433281.5 JD TDB = 1949-12-31 00:00:00.000000 TDB
!       2400000.5 JD TDB = 1858-11-17 00:00:00.000000 TDB
!
!       2433281.5 - 2400000.5 = 33281.0
!

!
!     Convert the J2000 TDB representation of the epoch to
!     JD UTC then calculate the offset from the JD 2433281.5 UTC
!     reference.
!

      TVEC(1) = ELEMS ( KEPOCH )
      CALL TTRANS ( 'TDB', 'JDUTC', TVEC )
      EPOCH = TVEC(1) - 2433281.5D0

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'XXSGP4I' )
         RETURN
      END IF

!
!     This code block replaces the call:
!
!     CALL getgravconst( whichconst, tumin,
!     .                  mu, radiusearthkm, xke,
!     .                  j2, j3, j4, j3oj2 )
!
      J2    = GEOPHS(K_J2)
      J3    = GEOPHS(K_J3)
      J4    = GEOPHS(K_J4)
      ER    = GEOPHS(K_ER)
      XKE   = GEOPHS(K_KE)
      TUMIN = 1.D0/GEOPHS(K_KE)
      J3OJ2 = J3/J2

!
!     The following assignment and IF block is taken
!     from TWOLINE2RVSGP4.
!
      A = (NO*TUMIN)**(-2.0D0/3.0D0)

      IF (DABS(ECCO-1.0D0) .GT. 0.000001D0) THEN
         ALTP= (A*(1.0D0-ECCO))-1.0D0
         ALTA= (A*(1.0D0+ECCO))-1.0D0
      ELSE
         ALTA= 999999.9D0
         ALTP= 2.0D0* (4.0D0/(NO*NO)**(1.0D0/3.0D0))
      END IF

      SS     = 78.0D0/ER + 1.0D0
      QZMS2T = ((120.0D0-78.0D0)/ER) ** 4
      X2O3   =  2.0D0 / 3.0D0

!
!     sgp4fix divisor for divide by zero check on inclination
!     the old check used 1.0D0 + cos(pi-1.0D-9), but then compared
!     it to 1.5D-12, so the threshold was changed to 1.5D-12 for
!     consistency.
!
        TEMP4 =   1.5D-12
        TZERO = 0.0D0
        DOINIT= .TRUE.

        CALL ZZINIL( GEOPHS, OPMODE, ECCO,  EPOCH, INCLO,  NO, &
                     AINV,   AO,     CON41, CON42, COSIO,  COSIO2, &
                     ECCSQ,  OMEOSQ, POSQ,  RP,    RTEOSQ, SINIO, &
                     GSTO )

        IF ( FAILED() ) THEN
           CALL CHKOUT ( 'XXSGP4I' )
           RETURN
        END IF


!
!       Check RP for a reasonable value. The propagator may not
!       calculate correct state values for RP < 1.
!
        IF (RP .LT. 1.0D0) THEN

           CALL SETMSG ( 'TLE elements suborbital.'     )
           CALL SIGERR ( 'SPICE(SUBORBITAL)'            )
           CALL CHKOUT ( 'XXSGP4I'                      )
           RETURN

        END IF

!
!       If nodeo and No are gtr 0
!
        IF( (OMEOSQ .GE. 0.0D0) .OR. (NO .GE. 0.0D0) ) THEN

            DOSIMP = .FALSE.

            IF (RP .LT. (220.0D0/ER+1.0D0)) THEN
                DOSIMP = .TRUE.
            END IF

            SFOUR  = SS
            QZMS24 = QZMS2T
            PERIGE = (RP-1.0D0)*ER

!
!           For perigees below 156 km, S and Qoms2t are altered.
!
            IF(PERIGE .LT. 156.0D0) THEN
                SFOUR = PERIGE-78.0D0

                IF(PERIGE .LE. 98.0D0) THEN
                    SFOUR = 20.0D0
                END IF

                QZMS24 = ( (120.0D0-SFOUR)/ER )**4
                SFOUR  = SFOUR/ER + 1.0D0

            END IF

            PINVSQ = 1.0D0/POSQ

            TSI    = 1.0D0/(AO-SFOUR)
            ETA    = AO*ECCO*TSI
            ETASQ  = ETA*ETA
            EETA   = ECCO*ETA
            PSISQ  = DABS(1.0D0-ETASQ)
            COEF   = QZMS24*TSI**4
            COEF1  = COEF/PSISQ**3.5D0
            CC2    = COEF1*NO* (AO* (1.0D0+1.5D0*ETASQ+EETA* &
                     (4.0D0+ETASQ) )+0.375D0* &
               J2*TSI/PSISQ*CON41*(8.0D0+3.0D0*ETASQ*(8.0D0+ETASQ)))
            CC1    = BSTAR*CC2
            CC3    = 0.0D0

            IF(ECCO .GT. 1.0D-4) THEN
                CC3 = -2.0D0*COEF*TSI*J3OJ2*NO*SINIO/ECCO
            END IF

            X1MTH2 = 1.0D0-COSIO2
            CC4    = 2.0D0*NO*COEF1*AO*OMEOSQ*(ETA*(2.0D0+0.5D0*ETASQ) &
                    +ECCO*(0.5D0 + 2.0D0*ETASQ) - J2*TSI / (AO*PSISQ)* &
                    (-3.0D0*CON41*(1.0D0-2.0D0* &
             EETA+ETASQ*(1.5D0-0.5D0*EETA))+0.75D0*X1MTH2*(2.0D0*ETASQ &
             -EETA*(1.0D0+ETASQ))*DCOS(2.0D0*ARGPO)))
            CC5    = 2.0D0*COEF1*AO*OMEOSQ* (1.0D0 + 2.75D0* &
                     (ETASQ + EETA) + EETA*ETASQ )
            COSIO4 = COSIO2*COSIO2
            TEMP1  = 1.5D0*J2*PINVSQ*NO
            TEMP2  = 0.5D0*TEMP1*J2*PINVSQ
            TEMP3  = -0.46875D0*J4*PINVSQ*PINVSQ*NO
            MDOT   = NO + 0.5D0*TEMP1*RTEOSQ*CON41 + 0.0625D0*TEMP2* &
                     RTEOSQ*(13.0D0 - 78.0D0*COSIO2 + 137.0D0*COSIO4)
            ARGPDOT= -0.5D0*TEMP1*CON42 + 0.0625D0*TEMP2* &
                     (7.0D0 - 114.0D0*COSIO2 + &
              395.0D0*COSIO4)+TEMP3*(3.0D0-36.0D0*COSIO2+49.0D0*COSIO4)
            XHDOT1 = -TEMP1*COSIO
            NODEDOT = XHDOT1+(0.5D0*TEMP2*(4.0D0-19.0D0*COSIO2)+ &
                       2.0D0*TEMP3*(3.0D0 - 7.0D0*COSIO2))*COSIO
            XPIDOT = ARGPDOT+NODEDOT
            OMGCOF = BSTAR*CC3*DCOS(ARGPO)
            XMCOF  = 0.0D0

            IF(ECCO .GT. 1.0D-4) THEN
                XMCOF = -X2O3*COEF*BSTAR/EETA
            END IF

            XNODCF = 3.5D0*OMEOSQ*XHDOT1*CC1
            T2COF  = 1.5D0*CC1

!
!           sgp4fix for divide by zero with xinco = 180 deg.
!
            IF (DABS(COSIO+1.D0).GT. 1.5D-12) THEN
                XLCOF  = -0.25D0*J3OJ2*SINIO* &
                         (3.0D0+5.0D0*COSIO)/(1.0D0+COSIO)
            ELSE
                XLCOF  = -0.25D0*J3OJ2*SINIO* &
                         (3.0D0+5.0D0*COSIO)/TEMP4
            END IF

            AYCOF  = -0.5D0*J3OJ2*SINIO
            DELMO  = (1.0D0+ETA*DCOS(MO))**3
            SINMAO = DSIN(MO)
            X7THM1 = 7.0D0*COSIO2-1.0D0

!
!           Deep Space Initialization
!
            IF ((TWOPI()/NO) .GE. 225.0D0) THEN

                DODEEP = .TRUE.
                DOSIMP = .TRUE.
                TC     = 0.0D0
                INCLM  = INCLO

!
!               Common.
!
                CALL ZZDSCM ( EPOCH, ECCO, ARGPO, TC, INCLO, &
                        NODEO,  NO, &
                        SNODM,  CNODM, SINIM, COSIM, SINOMM, COSOMM, &
                        DAY,    E3,    EE2,   ECCM,  EMSQ,   GAM, &
                        PEO,    PGHO,  PHO,   PINCO, PLO, &
                        RTEMSQ, SE2,   SE3,   SGH2,  SGH3,   SGH4, &
                        SH2,    SH3,   SI2,   SI3,   SL2,    SL3, &
                        SL4,    S1,    S2,    S3,    S4,     S5, &
                        S6,     S7,    SS1,   SS2,   SS3,    SS4, &
                        SS5,    SS6,   SS7,   SZ1,   SZ2,    SZ3, &
                        SZ11,   SZ12,  SZ13,  SZ21,  SZ22,   SZ23, &
                        SZ31,   SZ32,  SZ33,  XGH2,  XGH3,   XGH4, &
                        XH2,    XH3,   XI2,   XI3,   XL2,    XL3, &
                        XL4,    XN,    Z1,    Z2,    Z3,     Z11, &
                        Z12,    Z13,   Z21,   Z22,   Z23,    Z31, &
                        Z32,    Z33,   ZMOL,  ZMOS )

!
!               Long period perturbations.
!
                CALL ZZDSPR( OPMODE, E3, EE2, PEO, PGHO, PHO, &
                        PINCO, &
                        PLO,  SE2,   SE3,   SGH2, SGH3, SGH4, &
                        SH2,  SH3,   SI2,   SI3,  SL2,  SL3, &
                        SL4,  TZERO, XGH2,  XGH3, XGH4, XH2, &
                        XH3,  XI2,   XI3,   XL2,  XL3,  XL4, &
                        ZMOL, ZMOS,  INCLM, DOINIT, &
                        ECCO, INCLO, NODEO, ARGPO, MO )

                ARGPM  = 0.0D0
                NODEM  = 0.0D0
                MM     = 0.0D0

!
!               Initialization
!
                CALL ZZDSIN ( GEOPHS, &
                         COSIM,  EMSQ,  ARGPO, S1,    S2,    S3, &
                         S4,     S5,    SINIM, SS1,   SS2,   SS3, &
                         SS4,    SS5,   SZ1,   SZ3,   SZ11,  SZ13, &
                         SZ21,   SZ23,  SZ31,  SZ33,  TZERO, TC, &
                         GSTO,   MO,    MDOT,  NO,    NODEO, NODEDOT, &
                         XPIDOT, Z1,    Z3,    Z11,   Z13,   Z21, &
                         Z23,    Z31,   Z33,   ECCO,  ECCSQ, &
                         ECCM,   ARGPM, INCLM, MM,    XN,    NODEM, &
                         IREZ,   ATIME, D2201, D2211, D3210, D3222, &
                         D4410,  D4422, D5220, D5232, D5421, D5433, &
                         DEDT,   DIDT,  DMDT,  DNDT,  DNODT, DOMDT, &
                         DEL1,   DEL2,  DEL3,  XFACT, XLAMO, XLI, &
                         XNI )

            END IF

!
!           Set variables if not deep space or rp < 220
!
            IF ( .NOT. DOSIMP ) THEN
               CC1SQ = CC1*CC1
               D2    = 4.0D0*AO*TSI*CC1SQ
               TEMP  = D2*TSI*CC1 / 3.0D0
               D3    = (17.0D0*AO + SFour) * TEMP
               D4    = 0.5D0*TEMP*AO*TSI* &
                        (221.0D0*AO + 31.0D0*SFour)*CC1
               T3COF = D2 + 2.0D0*CC1SQ
               T4COF = 0.25D0* (3.0D0*D3+CC1*(12.0D0*D2+10.0D0*CC1SQ) )
               T5COF = 0.2D0* (3.0D0*D4 + 12.0D0*CC1*D3 + 6.0D0*D2*D2 + &
                        15.0D0*CC1SQ* (2.0D0*D2 + CC1SQ) )

            END IF

         END IF

      DOINIT = .FALSE.

      CALL CHKOUT ( 'XXSGP4I' )
      RETURN



!$Procedure XXSGP4E ( SGP4 evaluator )

      ENTRY XXSGP4E ( T, STATE )

! $ Abstract
!
!     This procedure is the SGP4 prediction model from Space Command.
!     This is an updated and combined version of SGP4 and SDP4
!     originally published separately in Spacetrack report #3 [1].
!     This version follows the methodology from the 2006 AIAA
!     Vallado paper [4] describing the history and development of
!     the code.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TLE
!     TWO LINE ELEMENTS
!
! $ Declarations
!
!     DOUBLE PRECISION         T
!     DOUBLE PRECISION         STATE     ( 6 )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     T          I   Time to evaluate state in minutes from epoch.
!     STATE      O   Evaluated state.
!
! $ Detailed_Input
!
!     T        is the time in minutes from the elements epoch
!              at which to calculate a state from the
!              TLE set.
!
! $ Detailed_Output
!
!     STATE    is the state produced by evaluating the input elements
!              at the input epoch T. Units are km and km/sec.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the mean motion is less than 0, the error
!         SPICE(BADMEANMOTION) is signaled.0D0.
!
!     2)  If the mean eccentricity is not bounded by [-0, the error
!         SPICE(BADMECCENTRICITY) is signaled.001, 1].
!
!     3)  If the mean semimajor axis has value less-than ,, the error
!         SPICE(BADMSEMIMAJOR) is signaled.95.
!
!     4)  If the perturbed eccentricity is not bounded by [0, 1], the
!         error SPICE(BADPECCENTRICITY) is signaled.
!
!     5)  If the semi-latus rectum has value less-than 0, the error
!         SPICE(BADSEMILATUS) is signaled.
!
!     6)  If the scaled orbit radial distance has value less than 1, the
!         error SPICE(ORBITDECAY) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is based on the SGP4 code by David Vallado
!     corresponding to "Revisiting Spacetrack Report #3."
!     The intent is to maintain the original Vallado algorithm,
!     changing code only to meet NAIF format standards and to
!     integrate with SPICELIB.
!
!        1) Implemented error checks using SPICE error subsystem.
!           On detecting an error, control returns to the calling
!           routine. This behavior differs from the original
!           version.
!
!        2) Comments prefixed with "SGP4FIX" indicate a comment
!           from the code by Vallado et. al concerning a correction
!           to the STR#3 code.
!
!        3) Eliminated the use of COMMON blocks.
!
!        Removed getgravconst call, replaced with GEOPHS array.
!
!        Capitalize all variables.
!
!        radiusearthkm replaced with ER
!        VKmPerSec     replaced with KPS
!        r, v          replaced with STATE
!        method        replaced with DODEEP
!        isimp         replaced with DOSIMP
!        Error         eliminated
!        whichconst    eliminated, function provided by GEOPHS
!
! $ Examples
!
!     Refer to $Examples section in ZZSGP4.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for
!         Propagation of the NORAD Element Sets." Spacetrack Report #3.
!         U.S. Air Force: Aerospace Defense Command.
!
!     [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation
!         of Space Command Element Sets." Space Command,
!         U. S. Air Force, CO.
!
!     [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004.
!         History of Analytical Orbit Modeling in the U. S. Space
!         Surveillance System. Journal of Guidance, Control, and
!         Dynamics. 27(2):174-185.
!
!     [4] Vallado, David, Crawford, Paul, Hujsak, Richard,
!         and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper
!         AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics
!         Specialist Conference, August 21-24, 2006. Keystone, CO.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 30-MAY-2021 (BVS) (JDR)
!
!        Added missing 'D' to literal DP constants.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 15-SEP-2014 (EDW)
!
!        Based on routine SGP4, 28-JUN-2005 [4].
!
!-&


! $ Index_Entries
!
!     SGP4
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'XXSGP4E' )

!
!     Local constants. Keep compiler ok for warnings on
!     uninitialized variables
!
      X2O3   = 2.0D0/3.0D0
      MR     = 0.0D0
      COSEO1 = 1.0D0
      SINEO1 = 0.0D0

!
!     Set mathematical constants.
!
!     This code block replaces the call:
!
!     sgp4fix identify constants and allow alternate values.
!
!     CALL getgravconst( whichconst, tumin,
!     .                  mu, radiusearthkm, xke,
!     .                  j2, j3, j4, j3oj2 )
!

!
!     sgp4fix divisor for divide by zero check on inclination
!     the old check used 1.0D0 + cos(pi-1.0D-9), but then compared it to
!     1.5D-12, so the threshold was changed to 1.5D-12 for consistency.
!
      TEMP4 =  1.5D-12
      KPS   =  ER * XKE/60.0D0

!
!     UPDATE FOR SECULAR GRAVITY AND ATMOSPHERIC DRAG
!
      XMDF   = MO + MDOT*T
      OMGADF = ARGPO + ARGPDOT*T
      XNODDF = NODEO + NODEDOT*T
      ARGPM  = OMGADF
      MM     = XMDF
      T2     = T*T
      NODEM  = XNODDF + XNODCF*T2
      TEMPA  = 1.0D0 - CC1*T
      TEMPE  = BSTAR*CC4*T
      TEMPL  = T2COF*T2


      IF ( .NOT. DOSIMP ) THEN

         DELOMG = OMGCOF*T
         DELM   = XMCOF*(( 1.0D0+ETA*DCOS(XMDF) )**3-DELMO)
         TEMP   = DELOMG + DELM
         MM     = XMDF + TEMP
         ARGPM  = OMGADF - TEMP
         T3     = T2*T
         T4     = T3*T
         TEMPA  = TEMPA - D2*T2 - D3*T3 - D4*T4
         TEMPE  = TEMPE + BSTAR*CC5*(DSIN(MM) - SINMAO)
         TEMPL  = TEMPL + T3COF*T3 + T4*(T4COF + T*T5COF)

      END IF

      XN    = NO
      ECCM  = ECCO
      INCLM = INCLO

      IF (DODEEP) THEN
         TC     = T

         CALL ZZDSPC (  IREZ,  D2201, D2211,   D3210, D3222, D4410, &
                        D4422, D5220, D5232,   D5421, D5433, DEDT, &
                        DEL1,  DEL2,  DEL3,    DIDT,  DMDT,  DNODT, &
                        DOMDT, ARGPO, ARGPDOT, T,     TC,    GSTO, &
                        XFACT, XLAMO, NO,      ATIME, ECCM, &
                        ARGPM, INCLM, XLI,     MM,    XNI, &
                        NODEM, DNDT,  XN  )

      END IF

!
!     Mean motion less than 0.0.
!
      IF(XN .LE. 0.0D0) THEN

         CALL SETMSG ( 'Mean motion less-than zero. ' &
          //           'This error may indicate a bad TLE set.'     )
         CALL SIGERR ( 'SPICE(BADMEANMOTION)'                       )
         CALL CHKOUT ( 'XXSGP4E'                                    )
         RETURN

      END IF

      AM   = (XKE/XN)**X2O3*TEMPA**2
      XN   = XKE/AM**1.5D0
      ECCM = ECCM-TEMPE

!
!     Fix tolerance for error recognition. Vallado code used
!     a lower limit of -0.001. This value apparently prevents
!     an error signal due to roundoff error.
!
      IF (ECCM .GE. 1.0D0 .OR. ECCM .LT. -0.001D0) THEN

         CALL SETMSG ( 'Mean eccentricity value, #, beyond ' &
          //           'allowed bounds [-0.001,1.0). This ' &
          //           'error may indicate a bad TLE set.'          )
         CALL ERRDP  ( '#', Eccm                                    )
         CALL SIGERR ( 'SPICE(BADMECCENTRICITY)'                     )
         CALL CHKOUT ( 'XXSGP4E'                                    )
         RETURN

      END IF

      IF ( AM .LT. 0.95D0 ) THEN

         CALL SETMSG ( 'Mean semi-major axis value, #, below allowed ' &
          //           'minimum of 0.95. This error may indicate ' &
          //           'a bad TLE set or a decayed orbit.'          )
         CALL ERRDP  ( '#', Eccm                                    )
         CALL SIGERR ( 'SPICE(BADMSEMIMAJOR)'                        )
         CALL CHKOUT ( 'XXSGP4E'                                    )
         RETURN

      END IF


!
!     sgp4fix change test condition for eccentricity
!
      IF (ECCM .LT. 1.0D-6) ECCM = 1.0D-6

      MM     = MM+NO*TEMPL
      XLM    = MM+ARGPM+NODEM
      EMSQ   = ECCM*ECCM
      TEMP   = 1.0D0 - EMSQ
      NODEM  = DMOD( NODEM, TWOPI())
      ARGPM  = DMOD( ARGPM, TWOPI())
      XLM    = DMOD( XLM,   TWOPI())
      MM     = DMOD( XLM - ARGPM - NODEM, TWOPI())

!
!     Compute extra mean quantities
!
      SINIM  = DSIN(INCLM)
      COSIM  = DCOS(INCLM)

!
!     Add lunar-solar periodics
!
      ECCP   = ECCM
      XINCP  = INCLM
      ARGPP  = ARGPM
      NODEP  = NODEM
      MP     = MM
      SINIP  = SINIM
      COSIP  = COSIM

!
!     Use deep space perturbation if indicated.
!

      IF (DODEEP) THEN

         CALL ZZDSPR ( SVMODE, E3,  EE2,   PEO,    PGHO, PHO, PINCO, &
                       PLO,  SE2,   SE3,   SGH2,   SGH3, SGH4, &
                       SH2,  SH3,   SI2,   SI3,    SL2,  SL3, &
                       SL4,  T,     XGH2,  XGH3,   XGH4, XH2, &
                       XH3,  XI2,   XI3,   XL2,    XL3,  XL4, &
                       ZMOL, ZMOS,  INCLO, .FALSE., &
                       ECCP, XINCP, NODEP, ARGPP, MP )

         IF(XINCP .LT. 0.0D0) THEN
            XINCP  = -XINCP
            NODEP  = NODEP + PI()
            ARGPP  = ARGPP - PI()
         END IF

         IF ( (ECCP .LT. 0.0D0) .OR. &
              (ECCP .GT. 1.0D0) ) THEN

            CALL SETMSG ( 'Perturbed eccentricity value, #, beyond ' &
             //           'allowed bounds [0,1]. This error may ' &
             //           'indicate a bad TLE set.'                )
            CALL ERRDP  ( '#', ECCP                                )
            CALL SIGERR ( 'SPICE(BADPECCENTRICITY)'                )
            CALL CHKOUT ( 'XXSGP4E'                                )
            RETURN

         END IF

      END IF

!
!     Update for long period periodics if a deep space trajectory.
!

      IF (DODEEP) THEN

          SINIP =  DSIN(XINCP)
          COSIP =  DCOS(XINCP)
          AYCOF = -0.5D0*J3OJ2*SINIP

!
!         sgp4fix for divide by zero with xincp = 180 deg
!

          IF (DABS(COSIP+1.D0).GT. 1.5D-12) THEN
             XLCOF  = -0.25D0*J3OJ2*SINIP* &
                       (3.0D0+5.0D0*COSIP)/(1.0D0+COSIP)
          ELSE
             XLCOF  = -0.25D0*J3OJ2*SINIP* &
                       (3.0D0+5.0D0*COSIP)/TEMP4
          END IF

      END IF

      AXNL = ECCP*DCOS(ARGPP)
      TEMP = 1.0D0 / (AM*(1.0D0-ECCP*ECCP))
      AYNL = ECCP*DSIN(ARGPP) + TEMP*AYCOF
      XL   = MP + ARGPP + NODEP + TEMP*XLCOF*AXNL

!
!     Solve Kepler's equation.
!
      U    = DMOD(XL-NODEP,TWOPI())
      EO1  = U
      ITER = 0

!
!     sgp4fix for Kepler iteration the following iteration needs
!     better limits on corrections
!

      TEMP = 9999.9D0

      DO WHILE ((TEMP.GE.1.0D-12).AND.(ITER.LT.10))

         ITER  = ITER+1
         SINEO1= DSIN(EO1)
         COSEO1= DCOS(EO1)
         TEM5  = 1.0D0 - COSEO1*AXNL - SINEO1*AYNL
         TEM5  = (U - AYNL*COSEO1 + AXNL*SINEO1 - EO1) / TEM5
         TEMP  = DABS(TEM5)

!
!        Stop excessive correction.
!
         IF(TEMP.GT.1.0D0) TEM5=TEM5/TEMP

         EO1   = EO1+TEM5

      END DO

!
!     Short period preliminary quantities.
!
      ECOSE = AXNL*COSEO1+AYNL*SINEO1
      ESINE = AXNL*SINEO1-AYNL*COSEO1
      EL2   = AXNL*AXNL+AYNL*AYNL
      PL    = AM*(1.0D0-EL2)

!
!     Error check for semi-latus rectum < 0.0
!
      IF ( PL .LT. 0.0D0 ) THEN

         CALL SETMSG ( 'Semi-latus rectum less-than zero.'          )
         CALL SIGERR ( 'SPICE(BADSEMILATUS)'                        )
         CALL CHKOUT ( 'XXSGP4E'                                    )
         RETURN

      END IF

      RL    = AM*(1.0D0-ECOSE)
      RDOTL = DSQRT(AM)*ESINE/RL
      RVDOTL= DSQRT(PL)/RL
      BETAL = DSQRT(1.0D0-EL2)
      TEMP  = ESINE/(1.0D0+BETAL)
      SINU  = AM/RL*(SINEO1-AYNL-AXNL*TEMP)
      COSU  = AM/RL*(COSEO1-AXNL+AYNL*TEMP)
      SU    = DATAN2(SINU,COSU)
      SIN2U = (COSU+COSU)*SINU
      COS2U = 1.0D0-2.0D0*SINU*SINU
      TEMP  = 1.0D0/PL
      TEMP1 = 0.5D0*J2*TEMP
      TEMP2 = TEMP1*TEMP

!
!     Update for short period periodics if a deep space trajectory.
!
      IF (DODEEP) THEN
          COSISQ = COSIP*COSIP
          CON41  = 3.0D0*COSISQ - 1.0D0
          X1MTH2 = 1.0D0 - COSISQ
          X7THM1 = 7.0D0*COSISQ - 1.0D0
      END IF

      MR    = RL*(1.0D0 - 1.5D0*TEMP2*BETAL*CON41) + &
                 0.5D0*TEMP1*X1MTH2*COS2U
      SU    = SU - 0.25D0*TEMP2*X7THM1*SIN2U
      XNODE = NODEP + 1.5D0*TEMP2*COSIP*SIN2U
      XINC  = XINCP + 1.5D0*TEMP2*COSIP*SINIP*COS2U
      MV    = RDOTL - XN*TEMP1*X1MTH2*SIN2U / XKE
      RVDOT = RVDOTL + XN*TEMP1* (X1MTH2*COS2U+1.5D0*CON41) / XKE

!
!     Orientation vectors.
!

      SINSU=  DSIN(SU)
      COSSU=  DCOS(SU)
      SNOD =  DSIN(XNODE)
      CNOD =  DCOS(XNODE)
      SINI =  DSIN(XINC)
      COSI =  DCOS(XINC)
      XMX  = -SNOD*COSI
      XMY  =  CNOD*COSI
      UX   =  XMX*SINSU + CNOD*COSSU
      UY   =  XMY*SINSU + SNOD*COSSU
      UZ   =  SINI*SINSU
      VX   =  XMX*COSSU - CNOD*SINSU
      VY   =  XMY*COSSU - SNOD*SINSU
      VZ   =  SINI*COSSU

!
!     Position and velocity.
!

      STATE(1) = MR*UX * ER
      STATE(2) = MR*UY * ER
      STATE(3) = MR*UZ * ER
      STATE(4) = (MV*UX + RVDOT*VX) * KPS
      STATE(5) = (MV*UY + RVDOT*VY) * KPS
      STATE(6) = (MV*UZ + RVDOT*VZ) * KPS

!
!     sgp4fix for decaying satellites
!
!     Place this test here to ensure evaluation of STATE.
!     The result may be physically invalid.
!
      IF (MR .LT. 1.0D0) THEN

         CALL SETMSG ( 'Satellite has decayed.' )
         CALL SIGERR ( 'SPICE(ORBITDECAY)'      )
         CALL CHKOUT ( 'XXSGP4E'                 )
         RETURN

      END IF

      CALL CHKOUT ( 'XXSGP4E' )

      RETURN

      END

