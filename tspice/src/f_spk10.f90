!$Procedure F_SPK10 ( Family of tests for types 10 )

      SUBROUTINE F_SPK10 ( OK )
      use pepper_legacy, pepper_legacy_frame => frame, pepper_legacy_ident => ident
      use pepper_inc_zzsgp4

! $ Abstract
!
!     This routine performs tests on SPK type 10 to make sure that
!     the correct routines are called and that subsetting occurs
!     correctly.
!
! $ Required_Reading
!
!   None.
!
! $ Keywords
!
!   None.
!
! $ Declarations

      IMPLICIT NONE


      LOGICAL               OK

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     OK         O   Indicates success or failure of test family.
!
! $ Detailed_Input
!
!   None.
!
! $ Detailed_Output
!
!   OK           Boolean indicating if all test family cases passed.
!
! $ Parameters
!
!   None.
!
! $ Exceptions
!
!   None.
!
! $ Files
!
!   None.
!
! $ Particulars
!
!   None.
!
! $ Examples
!
!   None.
!
! $ Restrictions
!
!   None.
!
! $ Literature_References
!
!   None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.1.0, 12-OCT-2021 (EDW)
!
!        Use of modified ZZTEME eliminating a matrix inversion.
!
!        Added tests for EVSGP4 routine.
!
!-    SPICELIB Version 3.0.0, 24-AUG-2016 (EDW) (BVS)
!
!        Tests reimplemented for new TLE code.
!        Eliminated unneeded test for EV2LIN; the new code
!        logic unifies low and high orbit propagation
!        calculations.
!
!-    SPICELIB Version 2.0.0, 11-OCT-2013 (EDW)
!
!        Recoded to use new ZZTEME routine in SPKE10 and
!        corresponding logic.
!
!        Added a proper SPICE header.
!
!        Replace equality checks, "=", with tolerance check, "~".
!
!-    SPICELIB Version 1.1.0, 20-OCT-1999 (WLT)
!
!        Declared PI to be an EXTERNAL function.
!
!-&

! $ Index_Entries
!
!     SPK type 10 (two line elements) test cases
!
!-&

!
!     SPICELIB Functions
!


!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               IDSIZE
      PARAMETER           ( IDSIZE = 40 )

      INTEGER               NMIX
      PARAMETER           ( NMIX   = 29 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )

      DOUBLE PRECISION      MED
      PARAMETER           ( MED    = 1.D-10 )

      CHARACTER*(24)        MSG    ( NMIX )
      CHARACTER*(IDSIZE)    IDENT
      CHARACTER*(LNSIZE)    ELE    ( NMIX*2 )
      CHARACTER*(LNSIZE)    HI2LN  (  2 )
      CHARACTER*(LNSIZE)    LOW2LN ( 20 )

      DOUBLE PRECISION      ARG
      INTEGER               BEGIN
      DOUBLE PRECISION      GEOPHS ( 8 )
      DOUBLE PRECISION      DARGDT
      DOUBLE PRECISION      DENOM
      DOUBLE PRECISION      DESCR  ( 6 )
      DOUBLE PRECISION      DWDT
      DOUBLE PRECISION      ELEMS  ( 290 )
      INTEGER               END
      DOUBLE PRECISION      EPOCH  ( NMIX  )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      EXPST  ( 6 )
      DOUBLE PRECISION      EXPSTA ( 6, NMIX )
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      J2TM   ( 6, 6 )
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NUMER
      DOUBLE PRECISION      OFFSET ( NMIX )
      DOUBLE PRECISION      PART1  ( 6 )
      DOUBLE PRECISION      PART1X ( 6 )
      DOUBLE PRECISION      PART2  ( 6 )
      DOUBLE PRECISION      PART2X ( 6 )
      DOUBLE PRECISION      RECORD ( 50 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      TEMPV  ( 3 )
      DOUBLE PRECISION      TM2J   ( 6, 6 )
      DOUBLE PRECISION      TMPSTA ( 6 )
      DOUBLE PRECISION      TOL    ( NMIX  )
      DOUBLE PRECISION      VEL    ( 3 )
      DOUBLE PRECISION      W

      INTEGER               BODY
      INTEGER               CENTER
      INTEGER               FRAME
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               ID
      INTEGER               SPK1
      INTEGER               SPK2
      INTEGER               SPK3
      INTEGER               TYPE
      INTEGER               N

      LOGICAL               ERR    ( NMIX )
      LOGICAL               FOUND


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SPK10' )

      CALL TCASE ( 'Preliminaries' )

         CALL TSTLSK
         CALL CHCKXC ( .FALSE., ' ',  OK )

!
!     Check the XXSGP4 wrapper, EVSGP4, after each XXSGP4
!     pair call.
!

      CALL TCASE ( 'Make sure we can create an SPK type 10 ' &
                // 'segment from a single set of two-line ' &
                // 'elements. ' )

         CALL KILFIL ( 'type10.bsp' )
!
!        We'll use the two-line elements for Topex given in the
!        header to GETELM.
!

         LOW2LN(  1 ) = '1 22076U 92052A   97173.53461370 -.0' &
         //             '0000038  00000-0  10000-3 0   594'
         LOW2LN(  2 ) = '2 22076  66.0378 163.4372 0008359 27' &
         //             '8.7732  81.2337 12.80930736227550'

!
!        Set the geophysical constants used by SPACE COMMAND in
!        the distributed code.
!
         GEOPHS( 1 ) =    1.082616D-3
         GEOPHS( 2 ) =   -2.53881D-6
         GEOPHS( 3 ) =   -1.65597D-6
         GEOPHS( 4 ) =    7.43669161D-2
         GEOPHS( 5 ) =  120.0D0
         GEOPHS( 6 ) =   78.0D0
         GEOPHS( 7 ) = 6378.135D0
         GEOPHS( 8 ) =    1.0D0

         CALL GETELM ( 1950, LOW2LN, EPOCH(1), ELEMS )

         FIRST = EPOCH(1) - 100.0D0*SPD()
         LAST  = EPOCH(1) + 100.0D0*SPD()

         ID    = -122076

         CALL SPKOPN ( 'type10.bsp', 'TEST_FILE', 1000, HANDLE )

         CALL SPKW10 (  HANDLE, ID,   399,   'J2000', FIRST, LAST, &
                       'Test TOPEX',  GEOPHS, 1,      ELEMS, EPOCH )
         CALL SPKCLS (  HANDLE )

         CALL CHCKXC ( .FALSE., ' ',  OK )


      CALL TCASE ( 'Make sure we can read read out of the file ' &
                // 'the data we just inserted. ' )

         ET = EPOCH(1)

         CALL SPKLEF ( 'type10.bsp', SPK1 )
         CALL SPKSFS ( ID, ET, HANDLE, DESCR, IDENT, FOUND )

         CALL CHCKXC ( .FALSE., ' ',            OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

!
!        Unpack the descriptor and make sure it has the correct
!        data in it.
!

         CALL  SPKUDS ( DESCR, &
                        BODY,  CENTER, FRAME, TYPE, &
                        FIRST, LAST,   BEGIN, END   )

         CALL SPKR10 ( HANDLE, DESCR, ET, RECORD )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKAD ( 'GEOPHS', GEOPHS, '=', RECORD,      8, 0.0D0, OK)
         CALL CHCKAD ( 'ELEM1',  ELEMS,  '=', RECORD(9),  10, 0.0D0, OK)
         CALL CHCKAD ( 'ELEM2',  ELEMS,  '=', RECORD(23), 10, 0.0D0, OK)

         CALL SPKUEF ( SPK1 )
         CALL CHCKXC ( .FALSE., ' ',  OK )


      CALL TCASE ( 'Construct a longer file and make sure that ' &
                // 'we get the correct states from SPKEZ. ' )

         CALL KILFIL ( 'type10_2.bsp' )

         LOW2LN(  1 ) = '1 18123U 87 53  A 87324.61041692 -.0' &
         //             '0000023  00000-0 -75103-5 0 00675'
         LOW2LN(  2 ) = '2 18123  98.8296 152.0074 0014950 16' &
         //             '8.7820 191.3688 14.12912554 21686'
         LOW2LN(  3 ) = '1 18123U 87 53  A 87326.73487726  .0' &
         //             '0000045  00000-0  28709-4 0 00684'
         LOW2LN(  4 ) = '2 18123  98.8335 154.1103 0015643 16' &
         //             '3.5445 196.6235 14.12912902 21988'
         LOW2LN(  5 ) = '1 18123U 87 53  A 87331.40868801  .0' &
         //             '0000104  00000-0  60183-4 0 00690'
         LOW2LN(  6 ) = '2 18123  98.8311 158.7160 0015481 14' &
         //             '9.9848 210.2220 14.12914624 22644'
         LOW2LN(  7 ) = '1 18123U 87 53  A 87334.24129978  .0' &
         //             '0000086  00000-0  51111-4 0 00702'
         LOW2LN(  8 ) = '2 18123  98.8296 161.5054 0015372 14' &
         //             '2.4159 217.8089 14.12914879 23045'
         LOW2LN(  9 ) = '1 18123U 87 53  A 87336.93227900 -.0' &
         //             '0000107  00000-0 -52860-4 0 00713'
         LOW2LN( 10 ) = '2 18123  98.8317 164.1627 0014570 13' &
         //             '5.9191 224.2321 14.12910572 23425'
         LOW2LN( 11 ) = '1 18123U 87 53  A 87337.28635487  .0' &
         //             '0000173  00000-0  10226-3 0 00726'
         LOW2LN( 12 ) = '2 18123  98.8284 164.5113 0015289 13' &
         //             '3.5979 226.6438 14.12916140 23475'
         LOW2LN( 13 ) = '1 18123U 87 53  A 87339.05673569  .0' &
         //             '0000079  00000-0  47069-4 0 00738'
         LOW2LN( 14 ) = '2 18123  98.8288 166.2585 0015281 12' &
         //             '7.9985 232.2567 14.12916010 24908'
         LOW2LN( 15 ) = '1 18123U 87 53  A 87345.43010859  .0' &
         //             '0000022  00000-0  16481-4 0 00758'
         LOW2LN( 16 ) = '2 18123  98.8241 172.5226 0015362 10' &
         //             '9.1515 251.1323 14.12915487 24626'
         LOW2LN( 17 ) = '1 18123U 87 53  A 87349.04167543  .0' &
         //             '0000042  00000-0  27370-4 0 00764'
         LOW2LN( 18 ) = '2 18123  98.8301 176.1010 0015565 10' &
         //             '0.0881 260.2047 14.12916361 25138'

!
!        Parse the element lines to arrays or orbit parameters.
!
         DO I = 0, 8
            CALL GETELM(1950, LOW2LN(I*2+1), EPOCH(I+1), ELEMS(I*10+1) )
         END DO

         FIRST = EPOCH(1) - 0.5D0*SPD()
         LAST  = EPOCH(9) + 0.5D0*SPD()

!
!        Create the TLE based SPK.
!
         ID    = -118123

         CALL SPKOPN ( 'type10_2.bsp', 'TEST_FILE', 1000, HANDLE )
         CALL SPKW10 (  HANDLE, ID,   399,   'J2000', FIRST, LAST, &
                       'DMSP F8',     GEOPHS, 9,      ELEMS, EPOCH )
         CALL SPKCLS (  HANDLE )

         CALL CHCKXC ( .FALSE., ' ',  OK )

!
!        EPOCH(1) corresponds to ELEMS(1)
!        EPOCH(2) corresponds to ELEMS(11)
!        EPOCH(3) corresponds to ELEMS(21)
!        EPOCH(4) corresponds to ELEMS(31)
!        EPOCH(5) corresponds to ELEMS(41)
!        EPOCH(6) corresponds to ELEMS(51)
!        EPOCH(7) corresponds to ELEMS(61)
!        EPOCH(8) corresponds to ELEMS(71)
!        EPOCH(9) corresponds to ELEMS(81)
!

         ET = 0.6D0 * EPOCH(5) + 0.4D0 * EPOCH(6)

         CALL SPKLEF ( 'type10_2.bsp', SPK2 )

         CALL XXSGP4I ( GEOPHS, ELEMS(41), AFSPC )
         CALL XXSGP4E ( (ET - EPOCH(5))/60.D0, PART1 )

!
!        Confirm EVSGP4.
!
         CALL EVSGP4 ( ET, GEOPHS, ELEMS(41), PART1X )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'STATE', PART1X, '=', PART1, 6, 0.0D0, OK )

         CALL XXSGP4I ( GEOPHS, ELEMS(51), AFSPC )
         CALL XXSGP4E ( (ET - EPOCH(6))/60.D0, PART2 )

!
!        Confirm EVSGP4.
!
         CALL EVSGP4 ( ET, GEOPHS, ELEMS(51), PART2X )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'STATE', PART2X, '=', PART2, 6, 0.0D0, OK )

         NUMER  = ET       - EPOCH(5)
         DENOM  = EPOCH(6) - EPOCH(5)
         ARG    = NUMER*PI()/DENOM
         DARGDT =       PI()/DENOM

         W      =  0.5D0 + 0.5D0 * COS( ARG )
         DWDT   =        - 0.5D0 * SIN( ARG ) * DARGDT

         CALL VLCOMG ( 6, W,  PART1, 1.0D0-W, PART2, EXPST    )
         CALL VLCOM  ( DWDT,  PART1, -DWDT,   PART2, VEL      )
         CALL VADD   ( VEL,   EXPST(4),              TEMPV    )
         CALL VEQU   ( TEMPV,                        EXPST(4) )

!
!        Calculate TEME to J2000.
!
         CALL ZZTEME ( ET, J2TM, TM2J )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL MXVG  ( TM2J, EXPST, 6, 6, TMPSTA )
         CALL MOVED ( TMPSTA,  6,      EXPST     )

         CALL SPKEZ  ( ID, ET, 'J2000', 'NONE', 399, STATE, LT )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'STATE', STATE, '=', EXPST, 6, 0.0D0, OK )



      CALL TCASE ( 'Using the previous file, make sure we get ' &
                // 'the correct state at one second after the ' &
                // 'beginning of the segment.' )


         ET = FIRST + 1.0D0

         CALL XXSGP4I ( GEOPHS, ELEMS(1), AFSPC )
         CALL XXSGP4E ( (ET - EPOCH(1))/60.D0, EXPST )

!
!        Confirm EVSGP4.
!
         CALL EVSGP4 ( ET, GEOPHS, ELEMS(1), PART1X )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'STATE', PART1X, '=', EXPST, 6, 0.0D0, OK )

!
!        Calculate TEME to J2000.
!
         CALL ZZTEME ( ET, J2TM, TM2J )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL MXVG  ( TM2J, EXPST, 6, 6, TMPSTA )
         CALL MOVED ( TMPSTA,  6,      EXPST     )

         CALL SPKEZ  ( ID, ET, 'J2000', 'NONE', 399, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'STATE', STATE, '~', EXPST, 6, TIGHT, OK )



      CALL TCASE ( 'Using the same file make sure we get the ' &
                // 'correct state at one second before the end ' &
                // 'of the segment. ' )


         ET = LAST - 1.0D0

         CALL XXSGP4I ( GEOPHS, ELEMS(81), AFSPC )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL XXSGP4E ( (ET - EPOCH(9))/60.D0, EXPST )
         CALL CHCKXC ( .FALSE., ' ',  OK )

!
!        Confirm EVSGP4.
!
         CALL EVSGP4 ( ET, GEOPHS, ELEMS(81), PART1X )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'STATE', PART1X, '=', EXPST, 6, 0.0D0, OK )

!
!        Calculate TEME to J2000.
!
         CALL ZZTEME ( ET, J2TM, TM2J )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL MXVG  ( TM2J, EXPST, 6, 6, TMPSTA )
         CALL MOVED ( TMPSTA,  6,      EXPST     )

         CALL SPKEZ  ( ID, ET, 'J2000', 'NONE', 399, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'STATE', STATE, '~', EXPST, 6, TIGHT, OK )

         CALL SPKUEF ( SPK2 )
         CALL CHCKXC ( .FALSE., ' ',  OK )


      CALL TCASE ( 'Make sure we can perform the same ' &
                // 'experiments using a deep space satellite. ' )


         CALL KILFIL ( 'type10_3.bsp' )

         HI2LN( 1) = '1 24846U 97031A   97179.08162378 -.0' &
         //          '0000182  00000-0  00000+0 0   129'
         HI2LN( 2) = '2 24846   4.5222  86.7012 6052628 17' &
         //          '8.7924 183.5048  2.04105068    52'

         CALL GETELM ( 1950, HI2LN, EPOCH(1), ELEMS )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         FIRST = EPOCH(1) - 10.0D0*SPD()
         LAST  = EPOCH(1) + 10.0D0*SPD()

         ID    = -124846

         CALL SPKOPN ( 'type10_3.bsp', 'TEST_FILE', 1000, HANDLE )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL SPKW10 (  HANDLE, ID,     399,   'J2000', FIRST, LAST, &
                       'Test INTELSAT', GEOPHS, 1,      ELEMS, EPOCH )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL SPKCLS (  HANDLE )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         ET = EPOCH(1)

         CALL XXSGP4I ( GEOPHS, ELEMS(1), AFSPC )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL XXSGP4E ( (ET - EPOCH(1))/60.D0, EXPST )
         CALL CHCKXC ( .FALSE., ' ',  OK )

!
!        Confirm EVSGP4.
!
         CALL EVSGP4 ( ET, GEOPHS, ELEMS(1), PART1X )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'STATE', PART1X, '=', EXPST, 6, 0.0D0, OK )

!
!        Calculate TEME to J2000.
!
         CALL ZZTEME ( ET, J2TM, TM2J )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL MXVG  ( TM2J, EXPST, 6, 6, TMPSTA )
         CALL MOVED ( TMPSTA,  6,      EXPST     )

         CALL SPKLEF ( 'type10_3.bsp', SPK3 )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL SPKEZ  ( ID, ET, 'J2000', 'NONE', 399, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'STATE', STATE, '~', EXPST, 6, TIGHT, OK )

         CALL SPKUEF ( SPK3 )
         CALL CHCKXC ( .FALSE., ' ',  OK )


      CALL TCASE ( 'TLE set mixing low and high orbit elements.' )

!
!        Test TLE evaluations using SPICE code, compare results
!        to values calculated from non SPICE routines.
!
!        EXPSTA data values taken from output of tle_t_ref.f.
!        That program uses only Vallado 2006 code for TLE
!        propagation.
!
!        Evaluate each TLE at epoch, or at an epoch + offset
!        that causes an error signal.
!
!        ERR, boolean indicating if expected SPICE error.
!        MSG, expected SPICE error message, if any.
!        OFFSET, from TLE epoch, in TDB seconds
!

         N             = 1

         ELE( 2*N -1 ) = '1 00005U 58002B   00179.78495062  ' &
         //         '.00000023  00000-0  28098-4 0  4753'
         ELE( 2*N    ) = '2 00005  34.2682 348.7242 1859667 ' &
         //         '331.7664  19.3264 10.82419157413667'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    7022.4652905749172D0
         EXPSTA( 2, N) =   -1400.0829671389206D0
         EXPSTA( 3, N) =    3.9951552899067093D-002
         EXPSTA( 4, N) =    1.8938410139509001D0
         EXPSTA( 5, N) =    6.4058937573052042D0
         EXPSTA( 6, N) =    4.5348072490078408D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 04632U 70093B   04031.91070959 ' &
         //         '-.00000084  00000-0  10000-3 0  9955'
         ELE( 2*N    ) = '2 04632  11.4628 273.1101 1450506 ' &
         //         '207.6000 143.9350  1.20231981 44145'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    2334.1145001531290D0
         EXPSTA( 2, N) =   -41920.440341023626D0
         EXPSTA( 3, N) =   -3.8674373804744179D-002
         EXPSTA( 4, N) =    2.8263210311692482D0
         EXPSTA( 5, N) =   -6.5091663977403971D-002
         EXPSTA( 6, N) =   0.57093605288508975D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 06251U 62025E   06176.82412014  ' &
         //         '.00008885  00000-0  12808-3 0  3985'
         ELE( 2*N    ) = '2 06251  58.0579  54.0425 0030035 ' &
         //         '139.1568 221.1854 15.56387291  6774'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    3988.3102258099252D0
         EXPSTA( 2, N) =    5498.9665707163922D0
         EXPSTA( 3, N) =   0.90055878311511461D0
         EXPSTA( 4, N) =   -3.2900327369587359D0
         EXPSTA( 5, N) =    2.3576528189349908D0
         EXPSTA( 6, N) =    6.4966234730266708D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 08195U 75081A   06176.33215444  ' &
         //         '.00000099  00000-0  11873-3 0   813'
         ELE( 2*N    ) = '2 08195  64.1586 279.0717 6877146 ' &
         //         '264.7651  20.2257  2.00491383225656'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    2349.8948327983148D0
         EXPSTA( 2, N) =   -14785.938111219008D0
         EXPSTA( 3, N) =    2.1193776981972199D-002
         EXPSTA( 4, N) =    2.7214880947491822D0
         EXPSTA( 5, N) =   -3.2568116536917593D0
         EXPSTA( 6, N) =    4.4984166710329783D0

         TOL( N )      = MED*10.D0


         N             = N + 1

         ELE( 2*N -1 ) = '1 09880U 77021A   06176.56157475  ' &
         //         '.00000421  00000-0  10000-3 0  9814'
         ELE( 2*N    ) = '2 09880  64.5968 349.3786 7069051 ' &
         //         '270.0229  16.3320  2.00813614112380'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    13020.067503971142D0
         EXPSTA( 2, N) =   -2449.0719342705006D0
         EXPSTA( 3, N) =    1.1589602950956694D0
         EXPSTA( 4, N) =    4.2473639336004894D0
         EXPSTA( 5, N) =    1.5971785003727861D0
         EXPSTA( 6, N) =    4.9567086099164372D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 09998U 74033F   05148.79417928 ' &
         //         '-.00000112  00000-0  00000+0 0  4480'
         ELE( 2*N    ) = '2 09998   9.4958 313.1750 0270971 ' &
         //         '327.5225  30.8097  1.16186785 45878'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    25532.989465075800D0
         EXPSTA( 2, N) =   -27244.263271425338D0
         EXPSTA( 3, N) =   -1.1157242099297304D0
         EXPSTA( 4, N) =    2.4102838843371428D0
         EXPSTA( 5, N) =    2.1941756819375415D0
         EXPSTA( 6, N) =   0.54588852559427548D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 11801U          80230.29629788  ' &
         //         '.01431103  00000-0  14311-1      13'
         ELE( 2*N    ) = '2 11801  46.7916 230.4354 7318036  ' &
         //         '47.4722  10.4117  2.28537848    13'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    7473.3710226929679D0
         EXPSTA( 2, N) =    428.94748300162695D0
         EXPSTA( 3, N) =    5828.7484660902210D0
         EXPSTA( 4, N) =    5.1071553893435429D0
         EXPSTA( 5, N) =    6.4446803027110429D0
         EXPSTA( 6, N) =  -0.18613329728878689D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 14128U 83058A   06176.02844893 ' &
         //         '-.00000158  00000-0  10000-3 0  9627'
         ELE( 2*N    ) = '2 14128  11.4384  35.2134 0011562  ' &
         //         '26.4582 333.5652  0.98870114 46093'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    34747.579316629301D0
         EXPSTA( 2, N) =    24502.371133501172D0
         EXPSTA( 3, N) =   -1.3283298582193905D0
         EXPSTA( 4, N) =   -1.7316426613919500D0
         EXPSTA( 5, N) =    2.4527726147067073D0
         EXPSTA( 6, N) =   0.60851008051124345D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 16925U 86065D   06151.67415771  ' &
         //         '.02550794 -30915-6  18784-3 0  4486'
         ELE( 2*N    ) = '2 16925  62.0906 295.0239 5596327 ' &
         //         '245.1593  47.9690  4.88511875148616'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    5559.1168667023858D0
         EXPSTA( 2, N) =   -11941.040904261730D0
         EXPSTA( 3, N) =   -19.412352061843578D0
         EXPSTA( 4, N) =    3.3921167606245350D0
         EXPSTA( 5, N) =   -1.9469851236560267D0
         EXPSTA( 6, N) =    4.2507558511834462D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 20413U 83020D   05363.79166667  ' &
         //         '.00000000  00000-0  00000+0 0  7041'
         ELE( 2*N    ) = '2 20413  12.3514 187.4253 7864447 ' &
         //         '196.3027 356.5478  0.24690082  7978'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    25123.292899943484D0
         EXPSTA( 2, N) =   -13225.499658932509D0
         EXPSTA( 3, N) =    3249.4035177273227D0
         EXPSTA( 4, N) =   0.48868341891562678D0
         EXPSTA( 5, N) =    4.7978975920330251D0
         EXPSTA( 6, N) =  -0.96111969247696638D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 21897U 92011A   06176.02341244 ' &
         //         '-.00001273  00000-0 -13525-3 0  3044'
         ELE( 2*N    ) = '2 21897  62.1749 198.0096 7421690 ' &
         //         '253.0462  20.1561  2.01269994104880'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =   -14464.721347520543D0
         EXPSTA( 2, N) =   -4699.1951744705957D0
         EXPSTA( 3, N) =    6.6816848095162931D-002
         EXPSTA( 4, N) =   -3.2493120125354000D0
         EXPSTA( 5, N) =   -3.2810327059775490D0
         EXPSTA( 6, N) =    4.0070469384187533D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 22312U 93002D   06094.46235912  ' &
         //         '.99999999  81888-5  49949-3 0  3953'
         ELE( 2*N    ) = '2 22312  62.1486  77.4698 0308723 ' &
         //         '267.9229  88.7392 15.95744531 98783'
         ERR(     N )  = .TRUE.
         MSG(     N )  = 'SPICE(BADMECCENTRICITY)'
         OFFSET(  N )  = 490.D0*60.D0

         EXPSTA( 1, N) = 0.D0
         EXPSTA( 2, N) = 0.D0
         EXPSTA( 3, N) = 0.D0
         EXPSTA( 4, N) = 0.D0
         EXPSTA( 5, N) = 0.D0
         EXPSTA( 6, N) = 0.D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 22674U 93035D   06176.55909107  ' &
         //         '.00002121  00000-0  29868-3 0  6569'
         ELE( 2*N    ) = '2 22674  63.5035 354.4452 7541712 ' &
         //         '253.3264  18.7754  1.96679808 93877'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    14712.220228426777D0
         EXPSTA( 2, N) =   -1443.8106180803009D0
         EXPSTA( 3, N) =   0.83497887031521567D0
         EXPSTA( 4, N) =    4.4189654690537532D0
         EXPSTA( 5, N) =    1.6295920970275477D0
         EXPSTA( 6, N) =    4.1155318005100501D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 23177U 94040C   06175.45752052  ' &
         //         '.00000386  00000-0  76590-3 0    95'
         ELE( 2*N    ) = '2 23177   7.0496 179.8238 7258491 ' &
         //         '296.0482   8.3061  2.25906668 97438'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =   -8801.6004644471086D0
         EXPSTA( 2, N) =   -3.3575572814206908D-002
         EXPSTA( 3, N) =  -0.44522742636108958D0
         EXPSTA( 4, N) =   -3.8352790996615878D0
         EXPSTA( 5, N) =   -7.6625521731745740D0
         EXPSTA( 6, N) =   0.94456132286685801D0

         TOL( N )      = MED*10.D0


         N             = N + 1

         ELE( 2*N -1 ) = '1 23333U 94071A   94305.49999999 ' &
         //         '-.00172956  26967-3  10000-3 0    15'
         ELE( 2*N    ) = '2 23333  28.7490   2.3720 9728298  ' &
         //         '30.4360   1.3500  0.07309491    70'
         ERR( N )      = .TRUE.
         MSG( N )      = 'SPICE(ORBITDECAY)'
         OFFSET( N )   = -20.D0*60.D0

         EXPSTA( 1, N) = 0.D0
         EXPSTA( 2, N) = 0.D0
         EXPSTA( 3, N) = 0.D0
         EXPSTA( 4, N) = 0.D0
         EXPSTA( 5, N) = 0.D0
         EXPSTA( 6, N) = 0.D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 23599U 95029B   06171.76535463  ' &
         //         '.00085586  12891-6  12956-2 0  2905'
         ELE( 2*N    ) = '2 23599   6.9327   0.2849 5782022 ' &
         //         '274.4436  25.2425  4.47796565123555'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    9892.6379404641721D0
         EXPSTA( 2, N) =    35.761449679428672D0
         EXPSTA( 3, N) =   -1.0822883762430016D0
         EXPSTA( 4, N) =    3.5566432356568418D0
         EXPSTA( 5, N) =    6.4560093731824457D0
         EXPSTA( 6, N) =   0.78361088961711700D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 24208U 96044A   06177.04061740 ' &
         //         '-.00000094  00000-0  10000-3 0  1600'
         ELE( 2*N    ) = '2 24208   3.8536  80.0121 0026640 ' &
         //         '311.0977  48.3000  1.00778054 36119'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    7534.1098696537065D0
         EXPSTA( 2, N) =    41266.392656156131D0
         EXPSTA( 3, N) =  -0.10801028478177664D0
         EXPSTA( 4, N) =   -3.0271680074579521D0
         EXPSTA( 5, N) =   0.55884899599332005D0
         EXPSTA( 6, N) =   0.20798275541008424D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 25954U 99060A   04039.68057285 ' &
         //         '-.00000108  00000-0  00000-0 0  6847'
         ELE( 2*N    ) = '2 25954   0.0004 243.8136 0001765  ' &
         //         '15.5294  22.7134  1.00271289 15615'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    8827.1566020954251D0
         EXPSTA( 2, N) =   -41223.009700114068D0
         EXPSTA( 3, N) =    3.6348296275007628D0
         EXPSTA( 4, N) =    3.0070873176244128D0
         EXPSTA( 5, N) =   0.64370132294004578D0
         EXPSTA( 6, N) =    9.4166299972929057D-004

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 26900U 01039A   06106.74503247  ' &
         //         '.00000045  00000-0  10000-3 0  8290'
         ELE( 2*N    ) = '2 26900   0.0164 266.5378 0003319  ' &
         //         '86.1794 182.2590  1.00273847 16981'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =   -42014.837945374689D0
         EXPSTA( 2, N) =    3702.3435766153675D0
         EXPSTA( 3, N) =   -26.675002565785363D0
         EXPSTA( 4, N) =  -0.26977524684110693D0
         EXPSTA( 5, N) =   -3.0618543924537232D0
         EXPSTA( 6, N) =    3.3672573774080308D-004

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 26975U 78066F   06174.85818871  ' &
         //         '.00000620  00000-0  10000-3 0  6809'
         ELE( 2*N    ) = '2 26975  68.4714 236.1303 5602877 ' &
         //         '123.7484 302.5767  2.05657553 67521'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =   -14506.923133367129D0
         EXPSTA( 2, N) =   -21613.560426385960D0
         EXPSTA( 3, N) =    10.050188926590833D0
         EXPSTA( 4, N) =    2.2129433074605691D0
         EXPSTA( 5, N) =    1.1599708913588078D0
         EXPSTA( 6, N) =    3.0206002010544735D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 28057U 03049A   06177.78615833  ' &
         //         '.00000060  00000-0  35940-4 0  1836'
         ELE( 2*N    ) = '2 28057  98.4283 247.6961 0000884  ' &
         //         '88.1964 271.9322 14.35478080140550'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =   -2715.2823740490348D0
         EXPSTA( 2, N) =   -6619.2643669242079D0
         EXPSTA( 3, N) =   -1.3414434536107979D-002
         EXPSTA( 4, N) =   -1.0085872729754288D0
         EXPSTA( 5, N) =   0.42278200265497323D0
         EXPSTA( 6, N) =    7.3852729394058771D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 28129U 03058A   06175.57071136 ' &
         //         '-.00000104  00000-0  10000-3 0   459'
         ELE( 2*N    ) = '2 28129  54.7298 324.8098 0048506 ' &
         //         '266.2640  93.1663  2.00562768 18443'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    21707.464117055755D0
         EXPSTA( 2, N) =   -15318.617519348343D0
         EXPSTA( 3, N) =   0.13551151963463845D0
         EXPSTA( 4, N) =    1.3040292138648395D0
         EXPSTA( 5, N) =    1.8169049737045730D0
         EXPSTA( 6, N) =    3.1619199752770473D0

         TOL( N )      = MED*10.D0


         N             = N + 1

         ELE( 2*N -1 ) = '1 28350U 04020A   06167.21788666  ' &
         //         '.16154492  76267-5  18678-3 0  8894'
         ELE( 2*N    ) = '2 28350  64.9977 345.6130 0024870 ' &
         //         '260.7578  99.9590 16.47856722116490'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    6333.0812293998079D0
         EXPSTA( 2, N) =   -1580.8285227911904D0
         EXPSTA( 3, N) =    90.693557173272907D0
         EXPSTA( 4, N) =   0.71463442323171589D0
         EXPSTA( 5, N) =    3.2242465486030065D0
         EXPSTA( 6, N) =    7.0831281301836881D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 28623U 05006B   06177.81079184  ' &
         //         '.00637644  69054-6  96390-3 0  6000'
         ELE( 2*N    ) = '2 28623  28.5200 114.9834 6249053 ' &
         //         '170.2550 212.8965  3.79477162 12753'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =   -11665.709019766888D0
         EXPSTA( 2, N) =    24943.614326156436D0
         EXPSTA( 3, N) =    25.805436322352136D0
         EXPSTA( 4, N) =   -1.5962286209749481D0
         EXPSTA( 5, N) =   -1.4761279607723508D0
         EXPSTA( 6, N) =    1.1260597533138914D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 28626U 05008A   06176.46683397 ' &
         //         '-.00000205  00000-0  10000-3 0  2190'
         ELE( 2*N    ) = '2 28626   0.0019 286.9433 0000335  ' &
         //         '13.7918  55.6504  1.00270176  4891'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    42080.718509611550D0
         EXPSTA( 2, N) =   -2646.8638735693594D0
         EXPSTA( 3, N) =   0.81851293889159726D0
         EXPSTA( 4, N) =   0.19310517730917121D0
         EXPSTA( 5, N) =    3.0686882496601733D0
         EXPSTA( 6, N) =    4.3844943135621002D-004

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 28872U 05037B   05333.02012661  ' &
         //         '.25992681  00000-0  24476-3 0  1534'
         ELE( 2*N    ) = '2 28872  96.4736 157.9986 0303955 ' &
         //         '244.0492 110.6523 16.46015938 10708'
         ERR( N )     = .TRUE.
         MSG( N )     = 'SPICE(SUBORBITAL)'
         OFFSET( N )  = 0.D0

         EXPSTA( 1, N) = 0.D0
         EXPSTA( 2, N) = 0.D0
         EXPSTA( 3, N) = 0.D0
         EXPSTA( 4, N) = 0.D0
         EXPSTA( 5, N) = 0.D0
         EXPSTA( 6, N) = 0.D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 29141U 85108AA  06170.26783845  ' &
         //         '.99999999  00000-0  13519-0 0   718'
         ELE( 2*N    ) = '2 29141  82.4288 273.4882 0015848 ' &
         //         '277.2124  83.9133 15.93343074  6828'
         ERR( N )      = .TRUE.
         MSG( N )      = 'SPICE(BADMSEMIMAJOR)'
         OFFSET( N )   = -1400.D0*60.D0

         EXPSTA( 1, N) = 0.D0
         EXPSTA( 2, N) = 0.D0
         EXPSTA( 3, N) = 0.D0
         EXPSTA( 4, N) = 0.D0
         EXPSTA( 5, N) = 0.D0
         EXPSTA( 6, N) = 0.D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 29238U 06022G   06177.28732010  ' &
         //         '.00766286  10823-4  13334-2 0   101'
         ELE( 2*N    ) = '2 29238  51.5595 213.7903 0202579  ' &
         //         '95.2503 267.9010 15.73823839  1061'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =   -5566.5951265376598D0
         EXPSTA( 2, N) =   -3789.7599104569354D0
         EXPSTA( 3, N) =    67.603822429836370D0
         EXPSTA( 4, N) =    2.8737593660919392D0
         EXPSTA( 5, N) =   -3.8253405215249763D0
         EXPSTA( 6, N) =    6.0232539237473670D0

         TOL( N )      = MED


         N             = N + 1

         ELE( 2*N -1 ) = '1 88888U          80275.98708465  ' &
         //         '.00073094  13844-3  66816-4 0    87'
         ELE( 2*N    ) = '2 88888  72.8435 115.9689 0086731  ' &
         //         '52.6988 110.5714 16.05824518  1058'
         ERR( N )      = .FALSE.
         MSG( N )      = ' '
         OFFSET( N )   = 0.D0

         EXPSTA( 1, N) =    2328.9697519299193D0
         EXPSTA( 2, N) =   -5995.2205115974202D0
         EXPSTA( 3, N) =    1719.9729714014379D0
         EXPSTA( 4, N) =    2.9120732803862603D0
         EXPSTA( 5, N) =  -0.98341795550286237D0
         EXPSTA( 6, N) =   -7.0908162079545889D0

         TOL( N )      = MED

!
!        Check the number of mixed element sets equals the expected
!        number of mix element sets.
!
         CALL CHCKSI ( 'NMIX', NMIX, '=', N, 0, OK )

!
!        Open a new SPK to contain the ELE array.
!
         CALL SPKOPN ( 'spk10mix.bsp', 'TEST_FILE', 1000, HANDLE )
         CALL CHCKXC ( .FALSE., ' ',  OK )

!
!        Create an SPK using the TLE set included in the 2006 Vallado
!        paper. This TLE set includes low and high orbit vehicles.
!
         DO I = 0, 28

            CALL GETELM (1950, ELE(I*2+1), EPOCH(I+1), ELEMS(I*10+1) )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            FIRST = EPOCH(I+1) -SPD()
            LAST  = EPOCH(I+1) +SPD()

            CALL SPKW10 (  HANDLE,        -I -10000, &
                           399,           'J2000', &
                           FIRST,         LAST, &
                           'MIX',         GEOPHS,    1, &
                           ELEMS(I*10+1), EPOCH(I+1) )
            CALL CHCKXC ( .FALSE., ' ',  OK )

         END DO

         CALL SPKCLS (  HANDLE )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL FURNSH ( 'spk10mix.bsp' )
         CALL CHCKXC ( .FALSE., ' ',  OK )

!
!        Continue tests if successful creation of mix kernel.
!
         IF ( OK ) THEN

!
!           Confirm evaluation at epoch of each TLE set. If
!           the evaluation fails, confirm expected error.
!
            DO I = 0, 28

               CALL TCASE ( ELE(2*I+1) )

               CALL SPKEZ ( -I -10000, EPOCH(I+1) + OFFSET(I+1), &
                           'J2000', 'NONE', 399, STATE, LT )
               CALL CHCKXC ( ERR(I+1), MSG(I+1),  OK )

!
!              If no error and expected result, compare the STATE
!              vector against an expected state.
!
               IF ( OK .AND. .NOT. ERR(I+1) ) THEN

!
!                 Calculate TEME to J2000.
!
                  CALL ZZTEME ( EPOCH(I+1) + OFFSET(I+1), J2TM, TM2J )
                  CALL CHCKXC ( .FALSE., ' ',  OK )

                  CALL MXVG ( TM2J, EXPSTA(1,I+1), 6, 6, EXPST )

!
!                 Confirm SPK evaluation equals to the case-specific
!                 tolerance the output from the Vallado based routine.
!
                  CALL CHCKAD ( 'EXPST', EXPST, '~/', STATE, &
                                 6, TOL(I+1), OK )

               END IF

            END DO

         END IF

!
!     Unload all files, then delete.
!

      CALL KCLEAR ()
      CALL KILFIL ( 'type10.bsp'   )
      CALL KILFIL ( 'type10_2.bsp' )
      CALL KILFIL ( 'type10_3.bsp' )
      CALL KILFIL ( 'spk10mix.bsp' )

      CALL T_SUCCESS ( OK )
      RETURN
      END
