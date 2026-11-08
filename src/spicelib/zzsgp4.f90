!$Procedure ZZSGP4 ( SGP4 wrapper )

      subroutine zzsgp4 ( geophs, elems, opmode, t, state )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Umbrella for the SGP4 initializer and evaluator routines.
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

      implicit none

      include 'zzsgp4.inc'

      real(wp)                 geophs    ( 8 )
      real(wp)                 elems     ( 10 )
      integer(ip)              opmode
      real(wp)                 t
      real(wp)                 state     ( 6 )

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

      real(wp)                 a
      real(wp)                 alta
      real(wp)                 altp
      real(wp)                 argpdot
      real(wp)                 aycof
      real(wp)                 cc1
      real(wp)                 cc4
      real(wp)                 cc5
      real(wp)                 con41
      real(wp)                 eta
      real(wp)                 inclo
      real(wp)                 mdot
      real(wp)                 nodedot
      real(wp)                 omgcof
      real(wp)                 sinmao
      real(wp)                 t2cof
      real(wp)                 t3cof
      real(wp)                 t4cof
      real(wp)                 t5cof
      real(wp)                 x1mth2
      real(wp)                 x7thm1
      real(wp)                 xlcof
      real(wp)                 xmcof
      real(wp)                 xnodcf

!
!     DS values
!

      real(wp)                 e3
      real(wp)                 ee2
      real(wp)                 peo
      real(wp)                 pgho
      real(wp)                 pho
      real(wp)                 pinco
      real(wp)                 plo
      real(wp)                 gsto
      real(wp)                 xfact
      real(wp)                 xgh2
      real(wp)                 xgh3
      real(wp)                 xgh4
      real(wp)                 xh2
      real(wp)                 xh3
      real(wp)                 xi2
      real(wp)                 xi3
      real(wp)                 xl2
      real(wp)                 xl3
      real(wp)                 xl4
      real(wp)                 xlamo
      real(wp)                 zmol
      real(wp)                 zmos
      real(wp)                 atime
      real(wp)                 xli
      real(wp)                 xni

      real(wp)                 ainv
      real(wp)                 am
      real(wp)                 ao
      real(wp)                 argpm
      real(wp)                 argpo
      real(wp)                 argpp
      real(wp)                 axnl
      real(wp)                 aynl
      real(wp)                 betal
      real(wp)                 bstar
      real(wp)                 cc1sq
      real(wp)                 cc2
      real(wp)                 cc3
      real(wp)                 cnod
      real(wp)                 cnodm
      real(wp)                 coef
      real(wp)                 coef1
      real(wp)                 con42
      real(wp)                 cos2u
      real(wp)                 coseo1
      real(wp)                 cosi
      real(wp)                 cosim
      real(wp)                 cosio
      real(wp)                 cosio2
      real(wp)                 cosio4
      real(wp)                 cosip
      real(wp)                 cosisq
      real(wp)                 cosomm
      real(wp)                 cossu
      real(wp)                 cosu
      real(wp)                 d2
      real(wp)                 d2201
      real(wp)                 d2211
      real(wp)                 d3
      real(wp)                 d3210
      real(wp)                 d3222
      real(wp)                 d4
      real(wp)                 d4410
      real(wp)                 d4422
      real(wp)                 d5220
      real(wp)                 d5232
      real(wp)                 d5421
      real(wp)                 d5433
      real(wp)                 day
      real(wp)                 dedt
      real(wp)                 del1
      real(wp)                 del2
      real(wp)                 del3
      real(wp)                 delm
      real(wp)                 delmo
      real(wp)                 delomg
      real(wp)                 didt
      real(wp)                 dmdt
      real(wp)                 dndt
      real(wp)                 dnodt
      real(wp)                 domdt
      real(wp)                 eccm
      real(wp)                 ecco
      real(wp)                 eccp
      real(wp)                 eccsq
      real(wp)                 ecose
      real(wp)                 eeta
      real(wp)                 el2
      real(wp)                 emsq
      real(wp)                 eo1
      real(wp)                 epoch
      real(wp)                 er
      real(wp)                 esine
      real(wp)                 etasq
      real(wp)                 gam
      real(wp)                 inclm
      real(wp)                 j2
      real(wp)                 j3
      real(wp)                 j3oj2
      real(wp)                 j4
      real(wp)                 kps
      real(wp)                 mm
      real(wp)                 mo
      real(wp)                 mp
      real(wp)                 mr
      real(wp)                 mv
      real(wp)                 no
      real(wp)                 nodem
      real(wp)                 nodeo
      real(wp)                 nodep
      real(wp)                 omeosq
      real(wp)                 omgadf
      real(wp)                 perige
      real(wp)                 pinvsq
      real(wp)                 pl
      real(wp)                 posq
      real(wp)                 psisq
      real(wp)                 qzms24
      real(wp)                 qzms2t
      real(wp)                 rdotl
      real(wp)                 rl
      real(wp)                 rp
      real(wp)                 rtemsq
      real(wp)                 rteosq
      real(wp)                 rvdot
      real(wp)                 rvdotl
      real(wp)                 s1
      real(wp)                 s2
      real(wp)                 s3
      real(wp)                 s4
      real(wp)                 s5
      real(wp)                 s6
      real(wp)                 s7
      real(wp)                 se2
      real(wp)                 se3
      real(wp)                 sfour
      real(wp)                 sgh2
      real(wp)                 sgh3
      real(wp)                 sgh4
      real(wp)                 sh2
      real(wp)                 sh3
      real(wp)                 si2
      real(wp)                 si3
      real(wp)                 sin2u
      real(wp)                 sineo1
      real(wp)                 sini
      real(wp)                 sinim
      real(wp)                 sinio
      real(wp)                 sinip
      real(wp)                 sinomm
      real(wp)                 sinsu
      real(wp)                 sinu
      real(wp)                 sl2
      real(wp)                 sl3
      real(wp)                 sl4
      real(wp)                 snod
      real(wp)                 snodm
      real(wp)                 ss
      real(wp)                 ss1
      real(wp)                 ss2
      real(wp)                 ss3
      real(wp)                 ss4
      real(wp)                 ss5
      real(wp)                 ss6
      real(wp)                 ss7
      real(wp)                 su
      real(wp)                 sz1
      real(wp)                 sz11
      real(wp)                 sz12
      real(wp)                 sz13
      real(wp)                 sz2
      real(wp)                 sz21
      real(wp)                 sz22
      real(wp)                 sz23
      real(wp)                 sz3
      real(wp)                 sz31
      real(wp)                 sz32
      real(wp)                 sz33
      real(wp)                 t2
      real(wp)                 t3
      real(wp)                 t4
      real(wp)                 tc
      real(wp)                 tem5
      real(wp)                 temp
      real(wp)                 temp1
      real(wp)                 temp2
      real(wp)                 temp3
      real(wp)                 temp4
      real(wp)                 tempa
      real(wp)                 tempe
      real(wp)                 templ
      real(wp)                 tsi
      real(wp)                 tumin
      real(wp)                 tvec   ( 8 )
      real(wp)                 tzero
      real(wp)                 u
      real(wp)                 ux
      real(wp)                 uy
      real(wp)                 uz
      real(wp)                 vx
      real(wp)                 vy
      real(wp)                 vz
      real(wp)                 x2o3
      real(wp)                 xhdot1
      real(wp)                 xinc
      real(wp)                 xincp
      real(wp)                 xke
      real(wp)                 xl
      real(wp)                 xlm
      real(wp)                 xmdf
      real(wp)                 xmx
      real(wp)                 xmy
      real(wp)                 xn
      real(wp)                 xnoddf
      real(wp)                 xnode
      real(wp)                 xpidot
      real(wp)                 z1
      real(wp)                 z11
      real(wp)                 z12
      real(wp)                 z13
      real(wp)                 z2
      real(wp)                 z21
      real(wp)                 z22
      real(wp)                 z23
      real(wp)                 z3
      real(wp)                 z31
      real(wp)                 z32
      real(wp)                 z33

      integer(ip)              iter
      integer(ip)              svmode
      integer(ip)              irez

      logical                  doinit
      logical                  dosimp
      logical                  dodeep

!
!     SPICELIB routines.
!

      real(wp)                 pi
      real(wp)                 twopi

      logical                  failed
      logical                  return

      save                     a
      save                     alta
      save                     altp
      save                     argpdot
      save                     argpo
      save                     atime
      save                     aycof
      save                     bstar
      save                     cc1
      save                     cc4
      save                     cc5
      save                     con41
      save                     d2
      save                     d2201
      save                     d2211
      save                     d3
      save                     d3210
      save                     d3222
      save                     d4
      save                     d4410
      save                     d4422
      save                     d5220
      save                     d5232
      save                     d5421
      save                     d5433
      save                     dedt
      save                     del1
      save                     del2
      save                     del3
      save                     delmo
      save                     didt
      save                     dmdt
      save                     dnodt
      save                     dodeep
      save                     domdt
      save                     dosimp
      save                     e3
      save                     ecco
      save                     ee2
      save                     er
      save                     eta
      save                     gsto
      save                     inclo
      save                     irez
      save                     j2
      save                     j3oj2
      save                     mdot
      save                     mo
      save                     no
      save                     nodedot
      save                     nodeo
      save                     omgcof
      save                     peo
      save                     pgho
      save                     pho
      save                     pinco
      save                     plo
      save                     se2
      save                     se3
      save                     sgh2
      save                     sgh3
      save                     sgh4
      save                     sh2
      save                     sh3
      save                     si2
      save                     si3
      save                     sinmao
      save                     sl2
      save                     sl3
      save                     sl4
      save                     svmode
      save                     t2cof
      save                     t3cof
      save                     t4cof
      save                     t5cof
      save                     x1mth2
      save                     x7thm1
      save                     xfact
      save                     xgh2
      save                     xgh3
      save                     xgh4
      save                     xh2
      save                     xh3
      save                     xi2
      save                     xi3
      save                     xke
      save                     xl2
      save                     xl3
      save                     xl4
      save                     xlamo
      save                     xlcof
      save                     xli
      save                     xmcof
      save                     xni
      save                     xnodcf
      save                     zmol
      save                     zmos

      call chkin  ( 'ZZSGP4' )
      call setmsg ( 'The routine ZZSGP4 is an umbrella for ' &
      //            'the SGP4 initializer and propagator entry ' &
      //            'points. Do not call ZZSGP4. It is ' &
      //            'likely that a programming error has ' &
      //            'been made.' )
      call sigerr ( 'SPICE(BOGUSENTRY)'  )
      call chkout ( 'ZZSGP4' )

      return



!$Procedure XXSGP4I ( SGP4 initializer )

      entry xxsgp4i ( geophs, elems, opmode )

! $ Abstract
!
!     This subroutine initializes variables for SGP4.
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
      if ( return() ) then
         return
      end if

      call chkin ( 'XXSGP4I' )

!
!     Initialize.
!
      dodeep = .false.
      dosimp = .false.
      svmode = opmode

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

      bstar = elems ( kbstar )
      inclo = elems ( kincl  )
      nodeo = elems ( knode0 )
      ecco  = elems ( kecc   )
      argpo = elems ( komega )
      mo    = elems ( kmo    )
      no    = elems ( kno    )

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

      tvec(1) = elems ( kepoch )
      call ttrans ( 'TDB', 'JDUTC', tvec )
      epoch = tvec(1) - 2433281.5_wp

      if ( failed() ) then
         call chkout ( 'XXSGP4I' )
         return
      end if

!
!     This code block replaces the call:
!
!     CALL getgravconst( whichconst, tumin,
!     .                  mu, radiusearthkm, xke,
!     .                  j2, j3, j4, j3oj2 )
!
      j2    = geophs(k_j2)
      j3    = geophs(k_j3)
      j4    = geophs(k_j4)
      er    = geophs(k_er)
      xke   = geophs(k_ke)
      tumin = 1.0_wp/geophs(k_ke)
      j3oj2 = j3/j2

!
!     The following assignment and IF block is taken
!     from TWOLINE2RVSGP4.
!
      a = (no*tumin)**(-2.0_wp/3.0_wp)

      if (dabs(ecco-1.0_wp) .gt. 0.000001_wp) then
         altp= (a*(1.0_wp-ecco))-1.0_wp
         alta= (a*(1.0_wp+ecco))-1.0_wp
      else
         alta= 999999.9_wp
         altp= 2.0_wp* (4.0_wp/(no*no)**(1.0_wp/3.0_wp))
      end if

      ss     = 78.0_wp/er + 1.0_wp
      qzms2t = ((120.0_wp-78.0_wp)/er) ** 4
      x2o3   =  2.0_wp / 3.0_wp

!
!     sgp4fix divisor for divide by zero check on inclination
!     the old check used 1.0D0 + cos(pi-1.0D-9), but then compared
!     it to 1.5D-12, so the threshold was changed to 1.5D-12 for
!     consistency.
!
        temp4 =   1.5e-12_wp
        tzero = 0.0_wp
        doinit= .true.

        call zzinil( geophs, opmode, ecco,  epoch, inclo,  no, &
                     ainv,   ao,     con41, con42, cosio,  cosio2, &
                     eccsq,  omeosq, posq,  rp,    rteosq, sinio, &
                     gsto )

        if ( failed() ) then
           call chkout ( 'XXSGP4I' )
           return
        end if


!
!       Check RP for a reasonable value. The propagator may not
!       calculate correct state values for RP < 1.
!
        if (rp .lt. 1.0_wp) then

           call setmsg ( 'TLE elements suborbital.'     )
           call sigerr ( 'SPICE(SUBORBITAL)'            )
           call chkout ( 'XXSGP4I'                      )
           return

        end if

!
!       If nodeo and No are gtr 0
!
        if( (omeosq .ge. 0.0_wp) .or. (no .ge. 0.0_wp) ) then

            dosimp = .false.

            if (rp .lt. (220.0_wp/er+1.0_wp)) then
                dosimp = .true.
            end if

            sfour  = ss
            qzms24 = qzms2t
            perige = (rp-1.0_wp)*er

!
!           For perigees below 156 km, S and Qoms2t are altered.
!
            if(perige .lt. 156.0_wp) then
                sfour = perige-78.0_wp

                if(perige .le. 98.0_wp) then
                    sfour = 20.0_wp
                end if

                qzms24 = ( (120.0_wp-sfour)/er )**4
                sfour  = sfour/er + 1.0_wp

            end if

            pinvsq = 1.0_wp/posq

            tsi    = 1.0_wp/(ao-sfour)
            eta    = ao*ecco*tsi
            etasq  = eta*eta
            eeta   = ecco*eta
            psisq  = dabs(1.0_wp-etasq)
            coef   = qzms24*tsi**4
            coef1  = coef/psisq**3.5_wp
            cc2    = coef1*no* (ao* (1.0_wp+1.5_wp*etasq+eeta* &
                     (4.0_wp+etasq) )+0.375_wp* &
               j2*tsi/psisq*con41*(8.0_wp+3.0_wp*etasq*(8.0_wp+etasq)))
            cc1    = bstar*cc2
            cc3    = 0.0_wp

            if(ecco .gt. 1.0e-4_wp) then
                cc3 = -2.0_wp*coef*tsi*j3oj2*no*sinio/ecco
            end if

            x1mth2 = 1.0_wp-cosio2
            cc4    = 2.0_wp*no*coef1*ao*omeosq*(eta*(2.0_wp+0.5_wp*etasq) &
                    +ecco*(0.5_wp + 2.0_wp*etasq) - j2*tsi / (ao*psisq)* &
                    (-3.0_wp*con41*(1.0_wp-2.0_wp* &
             eeta+etasq*(1.5_wp-0.5_wp*eeta))+0.75_wp*x1mth2*(2.0_wp*etasq &
             -eeta*(1.0_wp+etasq))*dcos(2.0_wp*argpo)))
            cc5    = 2.0_wp*coef1*ao*omeosq* (1.0_wp + 2.75_wp* &
                     (etasq + eeta) + eeta*etasq )
            cosio4 = cosio2*cosio2
            temp1  = 1.5_wp*j2*pinvsq*no
            temp2  = 0.5_wp*temp1*j2*pinvsq
            temp3  = -0.46875_wp*j4*pinvsq*pinvsq*no
            mdot   = no + 0.5_wp*temp1*rteosq*con41 + 0.0625_wp*temp2* &
                     rteosq*(13.0_wp - 78.0_wp*cosio2 + 137.0_wp*cosio4)
            argpdot= -0.5_wp*temp1*con42 + 0.0625_wp*temp2* &
                     (7.0_wp - 114.0_wp*cosio2 + &
              395.0_wp*cosio4)+temp3*(3.0_wp-36.0_wp*cosio2+49.0_wp*cosio4)
            xhdot1 = -temp1*cosio
            nodedot = xhdot1+(0.5_wp*temp2*(4.0_wp-19.0_wp*cosio2)+ &
                       2.0_wp*temp3*(3.0_wp - 7.0_wp*cosio2))*cosio
            xpidot = argpdot+nodedot
            omgcof = bstar*cc3*dcos(argpo)
            xmcof  = 0.0_wp

            if(ecco .gt. 1.0e-4_wp) then
                xmcof = -x2o3*coef*bstar/eeta
            end if

            xnodcf = 3.5_wp*omeosq*xhdot1*cc1
            t2cof  = 1.5_wp*cc1

!
!           sgp4fix for divide by zero with xinco = 180 deg.
!
            if (dabs(cosio+1.0_wp).gt. 1.5e-12_wp) then
                xlcof  = -0.25_wp*j3oj2*sinio* &
                         (3.0_wp+5.0_wp*cosio)/(1.0_wp+cosio)
            else
                xlcof  = -0.25_wp*j3oj2*sinio* &
                         (3.0_wp+5.0_wp*cosio)/temp4
            end if

            aycof  = -0.5_wp*j3oj2*sinio
            delmo  = (1.0_wp+eta*dcos(mo))**3
            sinmao = dsin(mo)
            x7thm1 = 7.0_wp*cosio2-1.0_wp

!
!           Deep Space Initialization
!
            if ((twopi()/no) .ge. 225.0_wp) then

                dodeep = .true.
                dosimp = .true.
                tc     = 0.0_wp
                inclm  = inclo

!
!               Common.
!
                call zzdscm ( epoch, ecco, argpo, tc, inclo, &
                        nodeo,  no, &
                        snodm,  cnodm, sinim, cosim, sinomm, cosomm, &
                        day,    e3,    ee2,   eccm,  emsq,   gam, &
                        peo,    pgho,  pho,   pinco, plo, &
                        rtemsq, se2,   se3,   sgh2,  sgh3,   sgh4, &
                        sh2,    sh3,   si2,   si3,   sl2,    sl3, &
                        sl4,    s1,    s2,    s3,    s4,     s5, &
                        s6,     s7,    ss1,   ss2,   ss3,    ss4, &
                        ss5,    ss6,   ss7,   sz1,   sz2,    sz3, &
                        sz11,   sz12,  sz13,  sz21,  sz22,   sz23, &
                        sz31,   sz32,  sz33,  xgh2,  xgh3,   xgh4, &
                        xh2,    xh3,   xi2,   xi3,   xl2,    xl3, &
                        xl4,    xn,    z1,    z2,    z3,     z11, &
                        z12,    z13,   z21,   z22,   z23,    z31, &
                        z32,    z33,   zmol,  zmos )

!
!               Long period perturbations.
!
                call zzdspr( opmode, e3, ee2, peo, pgho, pho, &
                        pinco, &
                        plo,  se2,   se3,   sgh2, sgh3, sgh4, &
                        sh2,  sh3,   si2,   si3,  sl2,  sl3, &
                        sl4,  tzero, xgh2,  xgh3, xgh4, xh2, &
                        xh3,  xi2,   xi3,   xl2,  xl3,  xl4, &
                        zmol, zmos,  inclm, doinit, &
                        ecco, inclo, nodeo, argpo, mo )

                argpm  = 0.0_wp
                nodem  = 0.0_wp
                mm     = 0.0_wp

!
!               Initialization
!
                call zzdsin ( geophs, &
                         cosim,  emsq,  argpo, s1,    s2,    s3, &
                         s4,     s5,    sinim, ss1,   ss2,   ss3, &
                         ss4,    ss5,   sz1,   sz3,   sz11,  sz13, &
                         sz21,   sz23,  sz31,  sz33,  tzero, tc, &
                         gsto,   mo,    mdot,  no,    nodeo, nodedot, &
                         xpidot, z1,    z3,    z11,   z13,   z21, &
                         z23,    z31,   z33,   ecco,  eccsq, &
                         eccm,   argpm, inclm, mm,    xn,    nodem, &
                         irez,   atime, d2201, d2211, d3210, d3222, &
                         d4410,  d4422, d5220, d5232, d5421, d5433, &
                         dedt,   didt,  dmdt,  dndt,  dnodt, domdt, &
                         del1,   del2,  del3,  xfact, xlamo, xli, &
                         xni )

            end if

!
!           Set variables if not deep space or rp < 220
!
            if ( .not. dosimp ) then
               cc1sq = cc1*cc1
               d2    = 4.0_wp*ao*tsi*cc1sq
               temp  = d2*tsi*cc1 / 3.0_wp
               d3    = (17.0_wp*ao + sfour) * temp
               d4    = 0.5_wp*temp*ao*tsi* &
                        (221.0_wp*ao + 31.0_wp*sfour)*cc1
               t3cof = d2 + 2.0_wp*cc1sq
               t4cof = 0.25_wp* (3.0_wp*d3+cc1*(12.0_wp*d2+10.0_wp*cc1sq) )
               t5cof = 0.2_wp* (3.0_wp*d4 + 12.0_wp*cc1*d3 + 6.0_wp*d2*d2 + &
                        15.0_wp*cc1sq* (2.0_wp*d2 + cc1sq) )

            end if

         end if

      doinit = .false.

      call chkout ( 'XXSGP4I' )
      return



!$Procedure XXSGP4E ( SGP4 evaluator )

      entry xxsgp4e ( t, state )

! $ Abstract
!
!     This procedure is the SGP4 prediction model from Space Command.
!     This is an updated and combined version of SGP4 and SDP4
!     originally published separately in Spacetrack report #3 [1].
!     This version follows the methodology from the 2006 AIAA
!     Vallado paper [4] describing the history and development of
!     the code.
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
      if ( return() ) then
         return
      end if

      call chkin ( 'XXSGP4E' )

!
!     Local constants. Keep compiler ok for warnings on
!     uninitialized variables
!
      x2o3   = 2.0_wp/3.0_wp
      mr     = 0.0_wp
      coseo1 = 1.0_wp
      sineo1 = 0.0_wp

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
      temp4 =  1.5e-12_wp
      kps   =  er * xke/60.0_wp

!
!     UPDATE FOR SECULAR GRAVITY AND ATMOSPHERIC DRAG
!
      xmdf   = mo + mdot*t
      omgadf = argpo + argpdot*t
      xnoddf = nodeo + nodedot*t
      argpm  = omgadf
      mm     = xmdf
      t2     = t*t
      nodem  = xnoddf + xnodcf*t2
      tempa  = 1.0_wp - cc1*t
      tempe  = bstar*cc4*t
      templ  = t2cof*t2


      if ( .not. dosimp ) then

         delomg = omgcof*t
         delm   = xmcof*(( 1.0_wp+eta*dcos(xmdf) )**3-delmo)
         temp   = delomg + delm
         mm     = xmdf + temp
         argpm  = omgadf - temp
         t3     = t2*t
         t4     = t3*t
         tempa  = tempa - d2*t2 - d3*t3 - d4*t4
         tempe  = tempe + bstar*cc5*(dsin(mm) - sinmao)
         templ  = templ + t3cof*t3 + t4*(t4cof + t*t5cof)

      end if

      xn    = no
      eccm  = ecco
      inclm = inclo

      if (dodeep) then
         tc     = t

         call zzdspc (  irez,  d2201, d2211,   d3210, d3222, d4410, &
                        d4422, d5220, d5232,   d5421, d5433, dedt, &
                        del1,  del2,  del3,    didt,  dmdt,  dnodt, &
                        domdt, argpo, argpdot, t,     tc,    gsto, &
                        xfact, xlamo, no,      atime, eccm, &
                        argpm, inclm, xli,     mm,    xni, &
                        nodem, dndt,  xn  )

      end if

!
!     Mean motion less than 0.0.
!
      if(xn .le. 0.0_wp) then

         call setmsg ( 'Mean motion less-than zero. ' &
          //           'This error may indicate a bad TLE set.'     )
         call sigerr ( 'SPICE(BADMEANMOTION)'                       )
         call chkout ( 'XXSGP4E'                                    )
         return

      end if

      am   = (xke/xn)**x2o3*tempa**2
      xn   = xke/am**1.5_wp
      eccm = eccm-tempe

!
!     Fix tolerance for error recognition. Vallado code used
!     a lower limit of -0.001. This value apparently prevents
!     an error signal due to roundoff error.
!
      if (eccm .ge. 1.0_wp .or. eccm .lt. -0.001_wp) then

         call setmsg ( 'Mean eccentricity value, #, beyond ' &
          //           'allowed bounds [-0.001,1.0). This ' &
          //           'error may indicate a bad TLE set.'          )
         call errdp  ( '#', eccm                                    )
         call sigerr ( 'SPICE(BADMECCENTRICITY)'                     )
         call chkout ( 'XXSGP4E'                                    )
         return

      end if

      if ( am .lt. 0.95_wp ) then

         call setmsg ( 'Mean semi-major axis value, #, below allowed ' &
          //           'minimum of 0.95. This error may indicate ' &
          //           'a bad TLE set or a decayed orbit.'          )
         call errdp  ( '#', eccm                                    )
         call sigerr ( 'SPICE(BADMSEMIMAJOR)'                        )
         call chkout ( 'XXSGP4E'                                    )
         return

      end if


!
!     sgp4fix change test condition for eccentricity
!
      if (eccm .lt. 1.0e-6_wp) eccm = 1.0e-6_wp

      mm     = mm+no*templ
      xlm    = mm+argpm+nodem
      emsq   = eccm*eccm
      temp   = 1.0_wp - emsq
      nodem  = dmod( nodem, twopi())
      argpm  = dmod( argpm, twopi())
      xlm    = dmod( xlm,   twopi())
      mm     = dmod( xlm - argpm - nodem, twopi())

!
!     Compute extra mean quantities
!
      sinim  = dsin(inclm)
      cosim  = dcos(inclm)

!
!     Add lunar-solar periodics
!
      eccp   = eccm
      xincp  = inclm
      argpp  = argpm
      nodep  = nodem
      mp     = mm
      sinip  = sinim
      cosip  = cosim

!
!     Use deep space perturbation if indicated.
!

      if (dodeep) then

         call zzdspr ( svmode, e3,  ee2,   peo,    pgho, pho, pinco, &
                       plo,  se2,   se3,   sgh2,   sgh3, sgh4, &
                       sh2,  sh3,   si2,   si3,    sl2,  sl3, &
                       sl4,  t,     xgh2,  xgh3,   xgh4, xh2, &
                       xh3,  xi2,   xi3,   xl2,    xl3,  xl4, &
                       zmol, zmos,  inclo, .false., &
                       eccp, xincp, nodep, argpp, mp )

         if(xincp .lt. 0.0_wp) then
            xincp  = -xincp
            nodep  = nodep + pi()
            argpp  = argpp - pi()
         end if

         if ( (eccp .lt. 0.0_wp) .or. &
              (eccp .gt. 1.0_wp) ) then

            call setmsg ( 'Perturbed eccentricity value, #, beyond ' &
             //           'allowed bounds [0,1]. This error may ' &
             //           'indicate a bad TLE set.'                )
            call errdp  ( '#', eccp                                )
            call sigerr ( 'SPICE(BADPECCENTRICITY)'                )
            call chkout ( 'XXSGP4E'                                )
            return

         end if

      end if

!
!     Update for long period periodics if a deep space trajectory.
!

      if (dodeep) then

          sinip =  dsin(xincp)
          cosip =  dcos(xincp)
          aycof = -0.5_wp*j3oj2*sinip

!
!         sgp4fix for divide by zero with xincp = 180 deg
!

          if (dabs(cosip+1.0_wp).gt. 1.5e-12_wp) then
             xlcof  = -0.25_wp*j3oj2*sinip* &
                       (3.0_wp+5.0_wp*cosip)/(1.0_wp+cosip)
          else
             xlcof  = -0.25_wp*j3oj2*sinip* &
                       (3.0_wp+5.0_wp*cosip)/temp4
          end if

      end if

      axnl = eccp*dcos(argpp)
      temp = 1.0_wp / (am*(1.0_wp-eccp*eccp))
      aynl = eccp*dsin(argpp) + temp*aycof
      xl   = mp + argpp + nodep + temp*xlcof*axnl

!
!     Solve Kepler's equation.
!
      u    = dmod(xl-nodep,twopi())
      eo1  = u
      iter = 0

!
!     sgp4fix for Kepler iteration the following iteration needs
!     better limits on corrections
!

      temp = 9999.9_wp

      do while ((temp.ge.1.0e-12_wp).and.(iter.lt.10))

         iter  = iter+1
         sineo1= dsin(eo1)
         coseo1= dcos(eo1)
         tem5  = 1.0_wp - coseo1*axnl - sineo1*aynl
         tem5  = (u - aynl*coseo1 + axnl*sineo1 - eo1) / tem5
         temp  = dabs(tem5)

!
!        Stop excessive correction.
!
         if(temp.gt.1.0_wp) tem5=tem5/temp

         eo1   = eo1+tem5

      end do

!
!     Short period preliminary quantities.
!
      ecose = axnl*coseo1+aynl*sineo1
      esine = axnl*sineo1-aynl*coseo1
      el2   = axnl*axnl+aynl*aynl
      pl    = am*(1.0_wp-el2)

!
!     Error check for semi-latus rectum < 0.0
!
      if ( pl .lt. 0.0_wp ) then

         call setmsg ( 'Semi-latus rectum less-than zero.'          )
         call sigerr ( 'SPICE(BADSEMILATUS)'                        )
         call chkout ( 'XXSGP4E'                                    )
         return

      end if

      rl    = am*(1.0_wp-ecose)
      rdotl = dsqrt(am)*esine/rl
      rvdotl= dsqrt(pl)/rl
      betal = dsqrt(1.0_wp-el2)
      temp  = esine/(1.0_wp+betal)
      sinu  = am/rl*(sineo1-aynl-axnl*temp)
      cosu  = am/rl*(coseo1-axnl+aynl*temp)
      su    = datan2(sinu,cosu)
      sin2u = (cosu+cosu)*sinu
      cos2u = 1.0_wp-2.0_wp*sinu*sinu
      temp  = 1.0_wp/pl
      temp1 = 0.5_wp*j2*temp
      temp2 = temp1*temp

!
!     Update for short period periodics if a deep space trajectory.
!
      if (dodeep) then
          cosisq = cosip*cosip
          con41  = 3.0_wp*cosisq - 1.0_wp
          x1mth2 = 1.0_wp - cosisq
          x7thm1 = 7.0_wp*cosisq - 1.0_wp
      end if

      mr    = rl*(1.0_wp - 1.5_wp*temp2*betal*con41) + &
                 0.5_wp*temp1*x1mth2*cos2u
      su    = su - 0.25_wp*temp2*x7thm1*sin2u
      xnode = nodep + 1.5_wp*temp2*cosip*sin2u
      xinc  = xincp + 1.5_wp*temp2*cosip*sinip*cos2u
      mv    = rdotl - xn*temp1*x1mth2*sin2u / xke
      rvdot = rvdotl + xn*temp1* (x1mth2*cos2u+1.5_wp*con41) / xke

!
!     Orientation vectors.
!

      sinsu=  dsin(su)
      cossu=  dcos(su)
      snod =  dsin(xnode)
      cnod =  dcos(xnode)
      sini =  dsin(xinc)
      cosi =  dcos(xinc)
      xmx  = -snod*cosi
      xmy  =  cnod*cosi
      ux   =  xmx*sinsu + cnod*cossu
      uy   =  xmy*sinsu + snod*cossu
      uz   =  sini*sinsu
      vx   =  xmx*cossu - cnod*sinsu
      vy   =  xmy*cossu - snod*sinsu
      vz   =  sini*cossu

!
!     Position and velocity.
!

      state(1) = mr*ux * er
      state(2) = mr*uy * er
      state(3) = mr*uz * er
      state(4) = (mv*ux + rvdot*vx) * kps
      state(5) = (mv*uy + rvdot*vy) * kps
      state(6) = (mv*uz + rvdot*vz) * kps

!
!     sgp4fix for decaying satellites
!
!     Place this test here to ensure evaluation of STATE.
!     The result may be physically invalid.
!
      if (mr .lt. 1.0_wp) then

         call setmsg ( 'Satellite has decayed.' )
         call sigerr ( 'SPICE(ORBITDECAY)'      )
         call chkout ( 'XXSGP4E'                 )
         return

      end if

      call chkout ( 'XXSGP4E' )

      return

      end

