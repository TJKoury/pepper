!$Procedure      F_TTRANS ( Family of tests for TTRANS )

      SUBROUTINE F_TTRANS ( OK )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     This routine test the functionality of the main SPICE
!     time translation routine TTRANS.
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
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the low-level SPICELIB
!     time system routine TTRANS.
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
!     W.L. Taber       (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 05-MAR-2009 (NJB) (WLT)
!
!        Added header.
!
!        Added test case for watcher-related error
!        handling. Routines that encounter failures
!        when looking up kernel data must not assume
!        they have the data, even if the watcher
!        system says no update is needed; formerly
!        TTRANS had this error.
!
!-&


!
!     Test Utility Functions
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(WDSIZE)    BEGDAT

!
!     SPICELIB Functions
!
!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      CHARACTER*(WDSIZE)    INSYS   ( 21 )
      CHARACTER*(WDSIZE)    OUTSYS  ( 21 )
      CHARACTER*(WDSIZE)    NTVEC   (  7 )
      CHARACTER*(WDSIZE)    STRING
      CHARACTER*(WDSIZE)    ESTRNG

      CHARACTER*(LNSIZE)    BADKER  ( 23 )
      CHARACTER*(LNSIZE)    GOODKR  ( 23 )

      DOUBLE PRECISION      ETVEC   ( 7 )
      DOUBLE PRECISION      TVEC    ( 10 )
      DOUBLE PRECISION      TVECS   ( 7, 21 )

      DOUBLE PRECISION      TDT1
      DOUBLE PRECISION      TDT2
      DOUBLE PRECISION      DIFF
      DOUBLE PRECISION      JDUTC1
      DOUBLE PRECISION      JDUTC2
      DOUBLE PRECISION      SECNDS
      DOUBLE PRECISION      JD

      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               NLINES
      INTEGER               SIZE    ( 21 )


      NLINES = 23

      BADKER(  1 ) = BEGDAT()
      BADKER(  2 ) = ' '
      BADKER(  3 ) = 'DELTET/DELTA_T_A       =   32.184'
      BADKER(  4 ) = 'DELTET/K               =    1.657D-3'
      BADKER(  5 ) = 'DELTET/EB              =    1.671D-2'
      BADKER(  6 ) = 'DELTET/M               = (  6.239996D0 ' &
      //             '  1.99096871D-7 )'
      BADKER(  7 ) = ' '
      BADKER(  8 ) = 'DELTET/DELTA_AT        = ( 10,   @1972-' &
      //             'JAN-1'
      BADKER(  9 ) = '                           11,   @1972-' &
      //             'JUL-1'
      BADKER( 10 ) = '                           12,   @1973-' &
      //             'JAN-1'
      BADKER( 11 ) = '                           13,   @1974-' &
      //             'JAN-1'
      BADKER( 12 ) = '                           14,   @1975-' &
      //             'JAN-1'
      BADKER( 13 ) = '                           15,   @1976-' &
      //             'JAN-1'
      BADKER( 14 ) = '                           16,   @1977-' &
      //             'JAN-1'
      BADKER( 15 ) = '                           17,   @1978-' &
      //             'JAN-1'
      BADKER( 16 ) = '                           18,   @1979-' &
      //             'JAN-1'
      BADKER( 17 ) = '                           20,   @1981-' &
      //             'JUL-1'
      BADKER( 18 ) = '                           19,   @1980-' &
      //             'JAN-1'
      BADKER( 19 ) = '                           21,   @1982-' &
      //             'JUL-1'
      BADKER( 20 ) = '                           22,   @1983-' &
      //             'JUL-1'
      BADKER( 21 ) = '                           23,   @1985-' &
      //             'JUL-1'
      BADKER( 22 ) = '                           24,   @1988-' &
      //             'JAN-1'
      BADKER( 23 ) = '                           25,   @1990-' &
      //             'JAN-1 )'


      GOODKR(  1 ) = BEGDAT()
      GOODKR(  2 ) = ' '
      GOODKR(  3 ) = 'DELTET/DELTA_T_A       =   32.184'
      GOODKR(  4 ) = 'DELTET/K               =    1.657D-3'
      GOODKR(  5 ) = 'DELTET/EB              =    1.671D-2'
      GOODKR(  6 ) = 'DELTET/M               = (  6.239996D0 ' &
      //             '  1.99096871D-7 )'
      GOODKR(  7 ) = ' '
      GOODKR(  8 ) = 'DELTET/DELTA_AT        = ( 10,   @1972-' &
      //             'JAN-1'
      GOODKR(  9 ) = '                           11,   @1972-' &
      //             'JUL-1'
      GOODKR( 10 ) = '                           12,   @1973-' &
      //             'JAN-1'
      GOODKR( 11 ) = '                           13,   @1974-' &
      //             'JAN-1'
      GOODKR( 12 ) = '                           14,   @1975-' &
      //             'JAN-1'
      GOODKR( 13 ) = '                           15,   @1976-' &
      //             'JAN-1'
      GOODKR( 14 ) = '                           16,   @1977-' &
      //             'JAN-1'
      GOODKR( 15 ) = '                           17,   @1978-' &
      //             'JAN-1'
      GOODKR( 16 ) = '                           18,   @1979-' &
      //             'JAN-1'
      GOODKR( 17 ) = '                           19,   @1980-' &
      //             'JAN-1'
      GOODKR( 18 ) = '                           20,   @1981-' &
      //             'JUL-1'
      GOODKR( 19 ) = '                           21,   @1982-' &
      //             'JUL-1'
      GOODKR( 20 ) = '                           22,   @1983-' &
      //             'JUL-1'
      GOODKR( 21 ) = '                           23,   @1985-' &
      //             'JUL-1'
      GOODKR( 22 ) = '                           24,   @1988-' &
      //             'JAN-1'
      GOODKR( 23 ) = '                           25,   @1990-' &
      //             'JAN-1 )'



!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_TTRANS' )

      TVEC(1) = 1995.0D0
      TVEC(2) =    2.0D0
      TVEC(3) =    1.0D0
      TVEC(4) =    0.0D0
      TVEC(5) =    0.0D0
      TVEC(6) =    0.0D0
      TVEC(7) =    0.0D0

!
!     Try the "no LSK" test case twice to make sure we get
!     the same error diagnosis both times.
!
      CALL TCASE ( 'Check the ''NOLEAPSECONDS'' exception. ' )

         CALL CLPOOL
         CALL TTRANS ( 'ET', 'YMD', TVEC  )
         CALL CHCKXC ( .TRUE., 'SPICE(NOLEAPSECONDS)', OK )

         CALL TTRANS ( 'ET', 'YMD', TVEC  )
         CALL CHCKXC ( .TRUE., 'SPICE(NOLEAPSECONDS)', OK )


!
!     Try the "bad LSK" test case twice to make sure we get
!     the same error diagnosis both times.
!
      CALL TCASE ( 'Check to make sure a leapsecond out of order ' &
      //           'error is properly diagnosed. ' )

         CALL TSTTXT ( 'testleap.ker', BADKER, NLINES, .TRUE., .FALSE. )
         CALL TTRANS ( 'ET', 'YMD', TVEC  )
         CALL CHCKXC ( .TRUE., 'SPICE(BADLEAPSECONDS)', OK )

         CALL TTRANS ( 'ET', 'YMD', TVEC  )
         CALL CHCKXC ( .TRUE., 'SPICE(BADLEAPSECONDS)', OK )


      CALL TCASE ( 'Make sure that an unknown time system is ' &
      //           'properly diagnosed when the first system is ' &
      //           'unknown. ' )


         CALL CLPOOL
         CALL TSTTXT ( 'testleap.ker', GOODKR, NLINES, .TRUE., .FALSE. )

         CALL TTRANS ( 'ETF', 'YMD', TVEC  )
         CALL CHCKXC ( .TRUE., 'SPICE(UNKNONWNTIMESYSTEM)', OK )

      CALL TCASE ( 'Make sure that an unknown time system is ' &
      //           'properly diagnosed when the second system is ' &
      //           'unknown. ' )

         CALL TTRANS ( 'YMD', 'DMY', TVEC  )
         CALL CHCKXC ( .TRUE., 'SPICE(UNKNONWNTIMESYSTEM)', OK )

      CALL TCASE ( 'Without checking the validity of TTRANS, ' &
      //           'make sure that all of the advertised time ' &
      //           'systems are recognized and that TVEC changes ' &
      //           'from it''s input value to a different output ' &
      //           'value. ' )


         INSYS(  1 ) = 'YMD'
         INSYS(  2 ) = 'YMDF'
         INSYS(  3 ) = 'YD'
         INSYS(  4 ) = 'YDF'
         INSYS(  5 ) = 'YD.D'
         INSYS(  6 ) = 'YD.DF'
         INSYS(  7 ) = 'DAYSEC'
         INSYS(  8 ) = 'DP2000'
         INSYS(  9 ) = 'JDUTC'
         INSYS( 10 ) = 'FORMAL'
         INSYS( 11 ) = 'YWD'
         INSYS( 12 ) = 'YWDF'
         INSYS( 13 ) = 'YMWD'
         INSYS( 14 ) = 'YMWDF'
         INSYS( 15 ) = 'TAI'
         INSYS( 16 ) = 'TDT'
         INSYS( 17 ) = 'TDB'
         INSYS( 18 ) = 'JED'
         INSYS( 19 ) = 'ET'
         INSYS( 20 ) = 'JDTDB'
         INSYS( 21 ) = 'JDTDT'

         DO I = 1, 21
            OUTSYS(I) = INSYS(I)
         END DO

         DO J = 1, 21
            DO I = 1, 7
               TVECS(I,J) = 0.0D0
            END DO
         END DO
!
!        Set up the test values for YMD
!
         TVECS( 1,  1 ) = 1995.0D0
         TVECS( 2,  1 ) = 1.0D0
         TVECS( 3,  1 ) = 2.0D0
         SIZE (     1 ) = 6
!
!        ... for YMDF
!
         TVECS( 1,  2 ) = 1995.0D0
         TVECS( 2,  2 ) = 1.0D0
         TVECS( 3,  2 ) = 2.0D0
         SIZE (     2 ) = 6
!
!        ... for YD
!
         TVECS( 1,  3 ) = 1995.0D0
         TVECS( 2,  3 ) = 2.0D0
         SIZE (     3 ) = 5
!
!        ... for YDF
!
         TVECS( 1,  4 ) = 1995.0D0
         TVECS( 2,  4 ) = 2.0D0
         SIZE (     4 ) = 5
!
!        ... for YD.D
!
         TVECS( 1,  5 ) = 1995.0D0
         TVECS( 2,  5 ) = 2.0D0
         SIZE (     5 ) = 2
!
!        ... for YD.DF
!
         TVECS( 1,  6 ) = 1995.0D0
         TVECS( 2,  6 ) = 2.0D0
         SIZE (     6 ) = 2
!
!        ... for DAYSEC
!
         TVECS( 1,  7 ) = 729039.0D0
         TVECS( 2,  7 ) = 20.0D0
         SIZE (     7 ) = 2
!
!        ... for DP2000
!
         TVECS( 1,  8 ) = -1850.0D0
         TVECS( 2,  8 ) =  20.0D0
         SIZE (     8 ) =  2
!
!        ... for JDUTC and FORMAL
!
         TVECS( 1,  9 ) =   2451525.0D0
         TVECS( 1, 10 ) = -31000000.0D0
         SIZE (     9 ) = 1
         SIZE (    10 ) = 1
!
!        ... for YWD
!
         TVECS( 1, 11 ) = 1995.0D0
         TVECS( 2, 11 ) = 2.0D0
         TVECS( 3, 11 ) = 2.0D0
         SIZE (    11 ) = 6
!
!        ... for YWDF
!
         TVECS( 1, 12 ) = 1995.0D0
         TVECS( 2, 12 ) = 2.0D0
         TVECS( 3, 12 ) = 2.0D0
         SIZE (    12 ) = 6
!
!        ... for YMWD
!
         TVECS( 1, 13 ) = 1995.0D0
         TVECS( 2, 13 ) = 2.0D0
         TVECS( 3, 13 ) = 2.0D0
         TVECS( 4, 13 ) = 1.0D0
         SIZE (    13 ) = 7
!
!        ... for YMWDF
!
         TVECS( 1, 14 ) = 1995.0D0
         TVECS( 2, 14 ) = 2.0D0
         TVECS( 3, 14 ) = 2.0D0
         TVECS( 4, 14 ) = 2.0D0
         SIZE (    14 ) = 7
!
!        ... for TAI, TDT, TDB, JED, ET, JDTDB, JDTDT
!
         TVECS( 1, 15 ) = -31000000.0D0
         TVECS( 1, 16 ) = -31000000.0D0
         TVECS( 1, 17 ) = -31000000.0D0
         TVECS( 1, 18 ) =   2451545.0D0
         TVECS( 1, 19 ) = -31000000.0D0
         TVECS( 1, 20 ) =   2451545.0D0
         TVECS( 1, 21 ) =   2451545.0D0

         SIZE (    15 ) = 1
         SIZE (    16 ) = 1
         SIZE (    17 ) = 1
         SIZE (    18 ) = 1
         SIZE (    19 ) = 1
         SIZE (    20 ) = 1
         SIZE (    21 ) = 1

         DO I = 1, 21
            DO J = 1,21

               DO K = 1,7
                  TVEC(K) = TVECS(K,I)
               END DO

               CALL TTRANS ( INSYS(I), OUTSYS(J), TVEC )
               CALL CHCKXC ( .FALSE., ' ', OK )
            END DO
         END DO


      CALL TCASE ( 'This test checks for internal consistency. ' &
      //           'Each pair is of possible input and output ' &
      //           'systems are converted "there and back ' &
      //           'again". ' )



         NTVEC(1) = 'TVEC(1)'
         NTVEC(2) = 'TVEC(2)'
         NTVEC(3) = 'TVEC(3)'
         NTVEC(4) = 'TVEC(4)'
         NTVEC(5) = 'TVEC(5)'
         NTVEC(6) = 'TVEC(6)'
         NTVEC(7) = 'TVEC(7)'

         DO I = 1, 21
            DO J = 1,21

               IF ( OUTSYS(J)(1:1) .NE. 'J' ) THEN

                  CALL TSTMSG ( '#', 'Input system #, Output System #' )
                  CALL TSTMSC ( INSYS(I) )
                  CALL TSTMSC ( OUTSYS(J) )

                  DO K = 1,7
                     TVEC(K) = TVECS(K,I)
                  END DO

                  CALL TTRANS ( INSYS(I), OUTSYS(J), TVEC )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL TTRANS ( OUTSYS(J), INSYS(I), TVEC )
                  CALL CHCKXC ( .FALSE., ' ', OK )


                  DO K = 1, SIZE(I)-1

                     CALL CHCKSD (NTVEC(K), TVEC(K), '=', TVECS(K,I), &
                                  0.0D0,    OK )

                  END DO

                  K = SIZE(I)
                  CALL CHCKSD ( NTVEC(K), TVEC(K), '~', TVECS(K,I), &
                                1.0D-7,    OK )

               END IF
            END DO
         END DO


      CALL TCASE ( 'Check to make sure that when we cross a ' &
      //           'leapsecond boundary in UTC components that ' &
      //           'the corresponding TDT change by the right amount.' )

         TVEC(1) = 1989.0D0
         TVEC(2) =   12.0D0
         TVEC(3) =   31.0D0
         TVEC(4) =   23.0D0
         TVEC(5) =   59.0D0
         TVEC(6) =   59.0D0

         CALL TTRANS ( 'YMD', 'TDT', TVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TDT1 = TVEC(1)

         TVEC(1) = 1990.0D0
         TVEC(2) =    1.0D0
         TVEC(3) =    1.0D0
         TVEC(4) =    0.0D0
         TVEC(5) =    0.0D0
         TVEC(6) =    0.0D0

         CALL TTRANS ( 'YMD', 'TDT', TVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TDT2 = TVEC(1)
         DIFF = TDT2 - TDT1

         CALL CHCKSD ( 'DIFF', DIFF, '=', 2.0D0, 0.0D0, OK )

      CALL TCASE ( 'Continuing the last case, make sure that TDT ' &
      //           'epochs that occur during leapseconds are ' &
      //           'properly transformed when returning to YMD ' &
      //           'UTC format. ' )


         TVEC(1) = TDT2 - 0.5D0

         CALL TTRANS ( 'TDT', 'YMD', TVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         ETVEC(1) = 1989.0D0
         ETVEC(2) =   12.0D0
         ETVEC(3) =   31.0D0
         ETVEC(4) =   23.0D0
         ETVEC(5) =   59.0D0
         ETVEC(6) =   60.5D0

         CALL CHCKAD ( 'TVEC', TVEC, '=', ETVEC, 6, 0.0D0, OK )

      CALL TCASE ( 'Make sure that we can get the day of the ' &
      //           'week and week of the month correct. ' )

         TVEC(1)  = 1996.0D0
         TVEC(2)  =    5.0D0
         TVEC(3)  =   23.0D0
         TVEC(4)  =   12.0D0
         TVEC(5)  =    0.0D0
         TVEC(6)  =    0.0D0

         ETVEC(1) = 1996.0D0
         ETVEC(2) =    5.0D0
         ETVEC(3) =    4.0D0
         ETVEC(4) =    5.0D0
         ETVEC(5) =   12.0D0
         ETVEC(6) =    0.0D0
         ETVEC(7) =    0.0D0

         CALL TTRANS ( 'YMD', 'YMWD', TVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'TVEC', TVEC, '=', ETVEC, 7, 0.0D0, OK )

      CALL TCASE ( 'The value of JDUTC should not change during ' &
      //           'a leapsecond, make sure that it doesn''t. ' )


         TVEC(1) = 1989.0D0
         TVEC(2) =   12.0D0
         TVEC(3) =   31.0D0
         TVEC(4) =   23.0D0
         TVEC(5) =   59.0D0
         TVEC(6) =   60.0D0

         CALL TTRANS ( 'YMD', 'JDUTC', TVEC )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         JDUTC1 = TVEC(1)

         TVEC(1) = 1989.0D0
         TVEC(2) =   12.0D0
         TVEC(3) =   31.0D0
         TVEC(4) =   23.0D0
         TVEC(5) =   59.0D0
         TVEC(6) =   60.5D0

         CALL TTRANS ( 'YMD', 'JDUTC', TVEC )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         JDUTC2 = TVEC(1)

         CALL CHCKSD ( 'JDUTC', JDUTC2, '=', JDUTC1, 0.0D0, OK )

         TVEC(1) = 1990.0D0
         TVEC(2) =    1.0D0
         TVEC(3) =    1.0D0
         TVEC(4) =    0.0D0
         TVEC(5) =    0.0D0
         TVEC(6) =    0.0D0

         CALL TTRANS ( 'YMD', 'JDUTC', TVEC )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         JDUTC2 = TVEC(1)

         CALL CHCKSD ( 'JDUTC', JDUTC2, '=', JDUTC1, 0.0D0, OK )


      CALL TCASE ( 'Using a format time system we should be able ' &
      //           'to convert to FORMAL and then predict the ' &
      //           'string that will be returned by ETCAL. ' )


         TVEC(1) = 1996.0D0
         TVEC(2) =    5.0D0
         TVEC(3) =   24.0D0
         TVEC(4) =    3.0D0
         TVEC(5) =   11.0D0
         TVEC(6) =   12.0D0

         ESTRNG  = '1996 MAY 24 03:11:12.000'

         CALL TTRANS ( 'YMDF', 'FORMAL', TVEC )
         CALL CHCKXC ( .FALSE., ' ',   OK  )

         CALL ETCAL  (  TVEC(1),  STRING )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( 'We can also predict the TDT time string ' &
      //           'associated with an epoch and use ETCAL to ' &
      //           'check this prediction. ' )




         TVEC(1) = 1996.0D0
         TVEC(2) =    5.0D0
         TVEC(3) =   24.0D0
         TVEC(4) =    3.0D0
         TVEC(5) =   11.0D0
         TVEC(6) =   12.0D0

         ESTRNG  = '1996 MAY 24 03:12:09.184'

         CALL TTRANS ( 'YMD',  'TDT', TVEC )
         CALL CHCKXC ( .FALSE., ' ',   OK  )

         CALL ETCAL  (  TVEC(1),  STRING )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( 'There were two leapseconds in 1972, make ' &
      //           'sure that we can compute the actual number ' &
      //           'of seconds in 1972. ' )


         TVEC(1) = 1972.0D0
         TVEC(2) =    1.0D0
         TVEC(3) =    1.0D0
         TVEC(4) =    0.0D0
         TVEC(5) =    0.0D0
         TVEC(6) =    0.0D0

         CALL TTRANS ( 'YMD',  'TDT', TVEC )
         CALL CHCKXC ( .FALSE., ' ',   OK  )
         TDT1 = TVEC(1)

         TVEC(1) = 1973.0D0
         TVEC(2) =    1.0D0
         TVEC(3) =    1.0D0
         TVEC(4) =    0.0D0
         TVEC(5) =    0.0D0
         TVEC(6) =    0.0D0

         CALL TTRANS ( 'YMD',  'TDT', TVEC )
         CALL CHCKXC ( .FALSE., ' ',   OK  )
         TDT2 = TVEC(1)
         DIFF = TDT2 - TDT1

         SECNDS = 366.0D0 * 86400.0D0 + 2.0D0

         CALL CHCKSD ( 'DIFF', DIFF, '=', SECNDS, 0.0D0, OK )


      CALL TCASE ( 'Make sure that the FORMAL time associated ' &
      //           'with 2000 Jan 1 12:00:00 is zero. ' )


         TVEC(1) = 2000.0D0
         TVEC(2) =    1.0D0
         TVEC(3) =    1.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =    0.0D0
         TVEC(6) =    0.0D0

         CALL TTRANS ( 'YMDF',  'FORMAL', TVEC )
         CALL CHCKXC ( .FALSE., ' ',      OK   )

         CALL CHCKSD ( 'TVEC(1)', TVEC(1), '=', 0.0D0, 0.0D0, OK )

      CALL TCASE ( 'Make sure that the TDT time associated ' &
      //           'with 2000 Jan 1 12:00:00 is zero. 57.184' )


         TVEC(1) = 2000.0D0
         TVEC(2) =    1.0D0
         TVEC(3) =    1.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =    0.0D0
         TVEC(6) =    0.0D0

         CALL TTRANS ( 'YMD',  'TDT', TVEC )
         CALL CHCKXC ( .FALSE., ' ',  OK   )

         CALL CHCKSD ( 'TVEC(1)', TVEC(1), '=', 57.184D0, 0.0D0, OK )

      CALL TCASE ( 'Make sure that we get the correct julian ' &
      //           'date for 0.0D0 ET. ' )

         TVEC(1) = 0.0D0

         CALL TTRANS ( 'TDT', 'JDTDT', TVEC(1) )
         CALL CHCKXC ( .FALSE., ' ',   OK     )

         JD = J2000()

         CALL CHCKSD ( 'JD', JD, '=', TVEC(1), 0.0D0, OK )


      CALL TCASE ( 'Make sure that ancient epochs are handled ' &
      //           'according to specification.  We use ETCAL to ' &
      //           'assist with this. ' )




         TVEC(1) = -333.0D0
         TVEC(2) =    5.0D0
         TVEC(3) =   24.0D0
         TVEC(4) =    3.0D0
         TVEC(5) =   11.0D0
         TVEC(6) =   12.0D0

         ESTRNG  = '334 B.C. MAY 24 03:11:12.000'

         CALL TTRANS ( 'YMD', 'FORMAL', TVEC )
         CALL CHCKXC ( .FALSE., ' ',    OK  )

         CALL ETCAL  (  TVEC(1),  STRING )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )




      CALL T_SUCCESS ( OK )
      RETURN
      END
