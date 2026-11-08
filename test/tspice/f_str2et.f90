!$Procedure F_STR2ET ( Family of tests for STR2ET )

      SUBROUTINE F_STR2ET ( OK )

! $ Abstract
!
!     This routine tests the routine STR2ET to make sure it
!     can handle the claimed variety of input strings.
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
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the higher-level SPICELIB
!     time system routines.
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
!     B.V. Semenov     (JPL)
!     W.L. Taber       (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 22-DEC-2021 (EDW) (BVS)
!
!        Add tests for TT (alias to TDT), TAI, and GPS.
!
!        Add disjoint "JD" test strings to TSTRNG.
!
!        Add test strings for decimal hour and decimal minutes.
!
!        Add tests for ISO formats with and without 'Z' marker.
!
!        Add tests for ISO formats 0 AD (1 BC) to 9999 AD.
!
!-    TSPICE Version 1.0.0, 22-FEB-2008 (NJB) (WLT)
!
!-&

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(WDSIZE)    BEGDAT

!
!     SPICELIB Functions
!
      DOUBLE PRECISION      UNITIM

!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MSGSIZ
      PARAMETER           ( MSGSIZ = 300 )

      INTEGER               I
      INTEGER               J
      INTEGER               NLINES
      INTEGER               N

      CHARACTER*(LNSIZE)    TSTRNG ( 29  )
      CHARACTER*(LNSIZE)    ISO    ( 13  )
      CHARACTER*(LNSIZE)    KERNEL ( 23  )
      CHARACTER*(MSGSIZ)    ERROR

      DOUBLE PRECISION      EXPDET ( 29  )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ETISO
      DOUBLE PRECISION      TT
      DOUBLE PRECISION      TDT
      DOUBLE PRECISION      TDB
      DOUBLE PRECISION      TAI
      DOUBLE PRECISION      GPS
      DOUBLE PRECISION      EB
      DOUBLE PRECISION      K
      DOUBLE PRECISION      M     ( 0:1 )

      LOGICAL               FOUND

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_STR2ET' )

      KERNEL(  1 ) = BEGDAT()
      KERNEL(  2 ) = ' '
      KERNEL(  3 ) = 'DELTET/DELTA_T_A       =   32.184'
      KERNEL(  4 ) = 'DELTET/K               =    1.657D-3'
      KERNEL(  5 ) = 'DELTET/EB              =    1.671D-2'
      KERNEL(  6 ) = 'DELTET/M               = (  6.239996D0 ' &
      //             '  1.99096871D-7 )'
      KERNEL(  7 ) = ' '
      KERNEL(  8 ) = 'DELTET/DELTA_AT        = ( 10,   @1972-' &
      //             'JAN-1'
      KERNEL(  9 ) = '                           11,   @1972-' &
      //             'JUL-1'
      KERNEL( 10 ) = '                           12,   @1973-' &
      //             'JAN-1'
      KERNEL( 11 ) = '                           13,   @1974-' &
      //             'JAN-1'
      KERNEL( 12 ) = '                           14,   @1975-' &
      //             'JAN-1'
      KERNEL( 13 ) = '                           15,   @1976-' &
      //             'JAN-1'
      KERNEL( 14 ) = '                           16,   @1977-' &
      //             'JAN-1'
      KERNEL( 15 ) = '                           17,   @1978-' &
      //             'JAN-1'
      KERNEL( 16 ) = '                           18,   @1979-' &
      //             'JAN-1'
      KERNEL( 17 ) = '                           19,   @1980-' &
      //             'JAN-1'
      KERNEL( 18 ) = '                           20,   @1981-' &
      //             'JUL-1'
      KERNEL( 19 ) = '                           21,   @1982-' &
      //             'JUL-1'
      KERNEL( 20 ) = '                           22,   @1983-' &
      //             'JUL-1'
      KERNEL( 21 ) = '                           23,   @1985-' &
      //             'JUL-1'
      KERNEL( 22 ) = '                           24,   @1988-' &
      //             'JAN-1'
      KERNEL( 23 ) = '                           25,   @1990-' &
      //             'JAN-1 )'

      NLINES = 23

      CALL CLPOOL
      CALL TSTTXT ( 'testleap.ker', KERNEL, NLINES, .TRUE., .FALSE. )


!
!     STR2ET critically depends on UNITIM. Confirm expected
!     basic behavior.
!
      CALL TCASE ( 'Check UNITIM.' )

          TT  = UNITIM( 0.D0, 'TDB', 'TT')
          TDT = UNITIM( 0.D0, 'TDB', 'TDT')

          CALL CHCKXC ( .FALSE., ' ', OK )

          CALL CHCKSD ( 'TDB -> TT TDT', TT, '=', TDT, 0.D0, OK )

!
!         Confirm the expected offset from TT of GPS and TAI.
!
          GPS = UNITIM( 0.D0, 'TDT', 'GPS')
          TAI = UNITIM( 0.D0, 'TDT', 'TAI')

          CALL CHCKXC ( .FALSE., ' ', OK )

          CALL CHCKSD ( 'TDT -> GPS', GPS, '~/', -51.184D0, &
                         1.0D-15, OK )

          CALL CHCKSD ( 'TDT -> TAI', TAI, '~/', -32.184D0, &
                         1.0D-15, OK )

          CALL CHCKSD ( 'TAI-GPS', (TAI-GPS), '~/', 19.D0, 1.0D-15, OK )

!
!         Reverse order.
!

          CALL GDPOOL ( 'DELTET/EB', 1, 1, N, EB, FOUND )
          CALL GDPOOL ( 'DELTET/K',  1, 1, N, K,  FOUND )
          CALL GDPOOL ( 'DELTET/M',  1, 2, N, M,  FOUND )

          CALL CHCKXC ( .FALSE., ' ', OK )


          TT  = UNITIM( 0.D0, 'TT', 'TDB')
          TDT = UNITIM( 0.D0, 'TDT', 'TDB')

          CALL CHCKXC ( .FALSE., ' ', OK )

!
!         Explicitly calculate the TDB value for TDT 0.
!
          TDB = K*SIN( M(0)+ EB*SIN( M(0) ) )

!
!         Idiot check.
!
          CALL CHCKSD ( 'TDB !=0', TDB, '!=', 0.D0, 0.D0, OK )

!
!         Confirm the subsystem recognizes 'TT' and 'TDT' as the same
!         time system.
!
          CALL CHCKSD ( 'TT TDT -> TDB', TT, '=', TDT, 0.D0, OK )
          CALL CHCKSD ( 'TDB = f(0)', TT, '=', TDB, 0.D0, OK )

          GPS = UNITIM( 0.D0, 'GPS', 'TDT')
          TAI = UNITIM( 0.D0, 'TAI', 'TDT')

          CALL CHCKXC ( .FALSE., ' ', OK )

!
!         Confirm the expected offset from TT of GPS and TAI.
!
          CALL CHCKSD ( 'GPS -> TDT', GPS, '~/', 51.184D0, &
                         1.0D-15, OK )

          CALL CHCKSD ( 'TAI -> TDT', TAI, '~/', 32.184D0, &
                         1.0D-15, OK )

          CALL CHCKSD ( 'GPS-TAI', (GPS-TAI), '~/', 19.D0, 1.0D-15, OK )

!
!         Confirm error response.
!
          TT = UNITIM( 0.D0, 'NULL', 'TDT')

          CALL CHCKXC ( .TRUE., 'SPICE(BADTIMETYPE)', OK )

          TT = UNITIM( 0.D0, 'TDT', 'NULL')

          CALL CHCKXC ( .TRUE., 'SPICE(BADTIMETYPE)', OK )



      CALL TCASE ( 'Make sure that ' &
      //           'an exception is generated when a component ' &
      //           'of the time string is out of range. ' )


         TSTRNG(1) = '1994 JAN 32 08:43:12'

         CALL STR2ET ( TSTRNG(1), ET )
         CALL CHCKXC ( .TRUE., 'SPICE(BADTIMESTRING)', OK )


      CALL TCASE ( 'Check STR2ET against a collection of times ' &
      //           'for which the outcome has been computed and ' &
      //           'hard coded in advance. ' )

         TSTRNG (  1  ) = ' 1/9/1986 3:12:59.2'
         EXPDET (  1  ) =  -441103565.6158334016800D+00

         TSTRNG (  2  ) = '9 JAN 1986 03:12:59.2'
         EXPDET (  2  ) =  -441103565.6158334016800D+00

         TSTRNG (  3  ) = '1 9 1986 3:12:59.2'
         EXPDET (  3  ) =  -441103565.6158334016800D+00

         TSTRNG (  4  ) = '9 JAN 1986 03:12:59.2'
         EXPDET (  4  ) =  -441103565.6158334016800D+00

         TSTRNG (  5  ) = '2 jan 1991 3:00:12.2'
         EXPDET (  5  ) =  -283942730.6160448789597D+00

         TSTRNG (  6  ) = '2 JAN 1991 03:00:12.2'
         EXPDET (  6  ) =  -283942730.6160448789597D+00

         TSTRNG (  7  ) = '1991 MAR 10 12:00:00'
         EXPDET (  7  ) =  -278121542.8144892454147D+00

         TSTRNG (  8  ) = '10 MAR 1991 12:00:00'
         EXPDET (  8  ) =  -278121542.8144892454147D+00

         TSTRNG (  9  ) = '1 March 1975 3:00'
         EXPDET (  9  ) =  -783853153.8146169185638D+00

         TSTRNG ( 10  ) = '1 MAR 1975 03:00:00'
         EXPDET ( 10  ) =  -783853153.8146169185638D+00

         TSTRNG ( 11  ) = '2010 October 29 3:58'
         EXPDET ( 11  ) =   341596737.1824791431427D+00

         TSTRNG ( 12  ) = '29 OCT 2010 03:58:00'
         EXPDET ( 12  ) =   341596737.1824791431427D+00

         TSTRNG ( 13  ) = 'dec 31 86 12:00'
         EXPDET ( 13  ) =  -410313544.8160908222198D+00

         TSTRNG ( 14  ) = '31 DEC 1986 12:00:00'
         EXPDET ( 14  ) =  -410313544.8160908222198D+00

         TSTRNG ( 15  ) = '86-365 // 12:00'
         EXPDET ( 15  ) =  -410313544.8160908222198D+00

         TSTRNG ( 16  ) = '31 DEC 1986 12:00:00'
         EXPDET ( 16  ) =  -410313544.8160908222198D+00

         TSTRNG ( 17  ) = '1991 MAR 10 12:00.25'
         EXPDET ( 17  ) =  -278121527.8144892454147D+00

         TSTRNG ( 18  ) = '1991 MAR 10 12.5'
         EXPDET ( 18  ) =  -278119742.8144892454147D+00

         TSTRNG ( 19  ) = '069-1991 // 12:00.25'
         EXPDET ( 19  ) =  -278121527.8144892454147D+00

         TSTRNG ( 20  ) = 'JD 2451545.'
         EXPDET ( 20  ) =  57.1839272823238D+00

         TSTRNG ( 21  ) = '1 JAN 2000 12:00:00'
         EXPDET ( 21  ) =  57.1839272823238D+00

         TSTRNG ( 22  ) = 'jd 2451545.'
         EXPDET ( 22  ) =  57.1839272823238D+00

         TSTRNG ( 23  ) = '1 JAN 2000 12:00:00'
         EXPDET ( 23  ) =  57.1839272823238D+00

         TSTRNG ( 24  ) = 'JD2451545.'
         EXPDET ( 24  ) =  57.1839272823238D+00

         TSTRNG ( 25  ) = '1 JAN 2000 12:00:00'
         EXPDET ( 25  ) =  57.1839272823238D+00

         TSTRNG ( 26  ) = 'JDTDB 2451546.0'
         EXPDET ( 26  ) =  86400.0D+00

         TSTRNG ( 27  ) = 'JDTDB 2451545.0'
         EXPDET ( 27  ) =  0.0D+00

         TSTRNG ( 28  ) = 'TDBJD 2451546.0'
         EXPDET ( 28  ) =  86400.0D+00

         TSTRNG ( 29  ) = '2451545.0 TDBJD'
         EXPDET ( 29  ) =  0.0D+00

         DO I = 1, 29

            CALL TSTMSG ( '#', 'Test subcase #. #' )
            CALL TSTMSI (  I )

            CALL STR2ET ( TSTRNG(I), ET )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( I .LT. 20 ) THEN
               CALL CHCKSD ( 'ET', ET, '~/', EXPDET(I), 1.0D-13, OK )
            ELSE
               CALL CHCKSD ( 'ET', ET, '~/', EXPDET(I), 1.0D-11, OK )
            END IF

         END DO


      CALL TCASE ( 'Check ISO formats with and without Z marker.' )

         ISO(1) = '1986-01-18T12:19:52.18'
         ISO(2) = '1996-12-18T12:28:28'
         ISO(3) = '1986-01-18T12:19'
         ISO(4) = '1986-01-18T12'
         ISO(5) = '1986-01-18T'
         ISO(6) = '1986-01-18T12.5'
         ISO(7) = '1986-01-18T12:19.5'
         ISO(8) = '1995-08T18:28:12.53'
         ISO(9) = '1995-08T18:28:12'
         ISO(10)= '1995-08T18:28'
         ISO(11)= '1995-18T'
         ISO(12)= '1986-209T12:19.18'
         ISO(13)= '1986-209T12.18'

         DO I = 1, 13

            CALL TCASE ( ISO(I) )

            CALL STR2ET ( ISO(I),   ET    )

            CALL STR2ET ( ISO(I)//'Z',   ETISO )

            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL CHCKSD ( 'ETISO', ET, '=', ETISO, 0.D0, OK )
         END DO



      CALL TCASE ( 'Check ISO formats two digit year' )

         ISO(1) = '00-01-01T'

         TSTRNG (1) = 'Jan 1, 2000'

         CALL TCASE ( ISO(1) )

         CALL STR2ET ( ISO(1), ETISO )

         CALL STR2ET ( TSTRNG(1), ET )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ETISO', ET, '=', ETISO, 0.D0, OK )



         ISO(1) = '11-001T00:00:01.000'

         TSTRNG (1) = '2011-01-01 00:00:01.000'

         CALL TCASE ( ISO(1) )

         CALL STR2ET ( ISO(1), ETISO )

         CALL STR2ET ( TSTRNG(1), ET )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ETISO', ET, '=', ETISO, 0.D0, OK )




      CALL TCASE ( 'Check ISO formats 100 AD. to 9999 AD' )

!
!        Loop over AD years 100 to 9999.
!
         DO J = 100, 9999

            ISO(1)    = '#-01-01T'
            TSTRNG(1) = 'Jan 1, # A.D.'

            CALL REPMI( ISO(1), '#', J, ISO(1) )
            CALL REPMI( TSTRNG(1), '#', J, TSTRNG(1) )

            CALL TCASE ( ISO(1) )

            CALL STR2ET ( ISO(1),   ETISO    )

            CALL STR2ET ( TSTRNG(1),   ET )

            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL CHCKSD ( 'ETISO', ET, '=', ETISO, 0.D0, OK )

         END DO


      CALL TCASE ( 'Step across a leapsecond making sure that ' &
      //           'ET has the expected behavior. ' )


         TSTRNG (  1  ) = '31 DEC 1989 23:59:55.01'
         EXPDET (  1  ) =  -315575948.8060699105263D+00

         TSTRNG (  2  ) = '31 DEC 1989 23:59:56.02'
         EXPDET (  2  ) =  -315575947.7960699200630D+00

         TSTRNG (  3  ) = '31 DEC 1989 23:59:57.03'
         EXPDET (  3  ) =  -315575946.7860699295998D+00

         TSTRNG (  4  ) = '31 DEC 1989 23:59:58.04'
         EXPDET (  4  ) =  -315575945.7760698795319D+00

         TSTRNG (  5  ) = '31 DEC 1989 23:59:59.05'
         EXPDET (  5  ) =  -315575944.7660698890686D+00

         TSTRNG (  6  ) = '31 DEC 1989 23:59:60.06'
         EXPDET (  6  ) =  -315575943.7560698986053D+00

         TSTRNG (  7  ) = '1  JAN 1990 00:00:00.07'
         EXPDET (  7  ) =  -315575942.7460699081421D+00

         TSTRNG (  8  ) = '1  JAN 1990 00:00:01.08'
         EXPDET (  8  ) =  -315575941.7360699176788D+00


         DO I = 1, 8

            CALL TSTMSG ( '#', 'Test subcase #.' )
            CALL TSTMSI (  I )

            CALL STR2ET ( TSTRNG(I), ET )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL CHCKSD ( 'ET', ET, '~/', EXPDET(I), 1.0D-13, OK )

         END DO


      CALL TCASE ( 'Check to see that we can successfully ' &
      //           'compute TDB based times. SubCase 1.' )

         TSTRNG(1) = 'Fri Sep 27 11:02:05 1996 TDB'

         CALL TPARSE ( 'Sep 27 11:02:05 1996',  EXPDET(1), ERROR )
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ET', ET, '=', EXPDET(1), 1.0D-13, OK )


      CALL TCASE ( 'Check to see that we can successfully ' &
      //           'compute TDB based times. SubCase 2.' )

         TSTRNG(1) = 'Fri, Sep 27 11:02:05 1996 TDB'

         CALL TPARSE ( 'Sep 27 11:02:05 1996',  EXPDET(1), ERROR )
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ET', ET, '=', EXPDET(1), 1.0D-13, OK )


      CALL TCASE ( 'Check to see that we can successfully ' &
      //           'compute times for various time zones. ' )

         CALL UTC2ET ( 'Sep 27 18:08:31 1996', EXPDET(1) )

         TSTRNG(1) = 'Fri Sep 27 11:08:31 PDT 1996'
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ET', ET, '=', EXPDET(1), 1.0D-13, OK )


         TSTRNG(1) = 'Fri Sep 27 10:08:31 PST 1996'
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ET', ET, '=', EXPDET(1), 1.0D-13, OK )

         TSTRNG(1) = 'Fri, Sep 27 10:08:31 PST 1996'
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ET', ET, '=', EXPDET(1), 1.0D-13, OK )


         TSTRNG(1) = 'Fri Sep 27 11:08:31 MST 1996'
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ET', ET, '=', EXPDET(1), 1.0D-13, OK )

         TSTRNG(1) = 'Fri, Sep 27 11:08:31 MST 1996'
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ET', ET, '=', EXPDET(1), 1.0D-13, OK )


         TSTRNG(1) = 'Fri Sep 27 12:08:31 MDT 1996'
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ET', ET, '=', EXPDET(1), 1.0D-13, OK )

         TSTRNG(1) = 'Fri Sep 27 12:08:31 CST 1996'
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ET', ET, '=', EXPDET(1), 1.0D-13, OK )

         TSTRNG(1) = 'Fri Sep 27 13:08:31 CDT 1996'
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ET', ET, '=', EXPDET(1), 1.0D-13, OK )

         TSTRNG(1) = 'Fri Sep 27 13:08:31 EST 1996'
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ET', ET, '=', EXPDET(1), 1.0D-13, OK )


         TSTRNG(1) = 'Fri Sep 27 14:08:31 EDT 1996'
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ET', ET, '=', EXPDET(1), 1.0D-13, OK )


      CALL TCASE ( 'Check to see that we can successfully ' &
      //           'compute times for time zones that are offset ' &
      //           'from UTC by a fractional number of hours. ' )

         CALL UTC2ET ( 'Sep 27 18:08:31 1996', EXPDET(1) )

         TSTRNG(1) = 'Fri Sep 27 13:00:31 1996  UTC-5:08'
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ET', ET, '=', EXPDET(1), 1.0D-13, OK )


      CALL TCASE ( 'Make sure that TDT labeled strings are ' &
      //           'recognized and properly processed. '  )

         TSTRNG(1) = 'Fri Sep 27 11:02:05 1996 TDT'

         CALL TPARSE ( 'Sep 27 11:02:05 1996',  EXPDET(1), ERROR )

         EXPDET(1) = UNITIM ( EXPDET(1), 'TDT', 'TDB' )

         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ET', ET, '~', EXPDET(1), 1.0D-7, OK )


      CALL TCASE ( 'Make sure that an exception is triggered if ' &
      //           'we try to put a leapsecond in the wrong ' &
      //           'place on the UTC scale. ' )


         TSTRNG(1) = 'Fri Sep 27 23:59:60.1 1996'
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .TRUE., 'SPICE(BADTIMESTRING)',  OK )


      CALL TCASE ( 'Make sure that an exception is triggered if ' &
      //           'we try to put a leapsecond in the wrong ' &
      //           'place on a non-UTC scale. ' )


         TSTRNG(1) = 'DEC 31 23:59:60.1 1996 PDT'
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .TRUE., 'SPICE(BADTIMESTRING)',  OK )


      CALL TCASE ( 'Make sure that we get a good value for a ' &
      //           'real leapsecond on a non-UTC Scale. ' )
!
!        Note we just copied the expected value of ET from a
!        previous case.
!
         TSTRNG (  1  ) = '31 DEC 1989 15:59:60.06 PST'
         EXPDET (  1  ) =  -315575943.7560698986053D+00

         CALL STR2ET ( TSTRNG(1), ET )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ET', ET, '~/', EXPDET(1), 1.0D-13, OK )

         TSTRNG (  1  ) = '31 DEC 1989 13:39:60.06 UTC-10:20'
         EXPDET (  1  ) =  -315575943.7560698986053D+00

         CALL STR2ET ( TSTRNG(1), ET )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ET', ET, '~/', EXPDET(1), 1.0D-13, OK )


         TSTRNG (  1  ) = '1 JAN 1990 10:19:60.06 UTC+10:20'
         EXPDET (  1  ) =  -315575943.7560698986053D+00

         CALL STR2ET ( TSTRNG(1), ET )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ET', ET, '~/', EXPDET(1), 1.0D-13, OK )


      CALL TCASE ( 'Make sure that leap seconds are not allowed ' &
      //           'in the TDB system. ' )


         TSTRNG (  1  ) = '31 DEC 1989 23:59:60.06 TDB'
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .TRUE., 'SPICE(BADTIMESTRING)',  OK )


      CALL TCASE ( 'Make sure that leap seconds are not allowed ' &
      //           'in the TDT system. ' )


         TSTRNG (  1  ) = '31 DEC 1989 23:59:60.06 TDT'
         CALL STR2ET ( TSTRNG(1),  ET )

         CALL CHCKXC ( .TRUE., 'SPICE(BADTIMESTRING)',  OK )


      CALL TCASE ( 'Change the default time system and see that ' &
      //           'the change is properly reflected in the ' &
      //           'behavior of STR2ET. DEFSYS = TDB ' )

         CALL TIMDEF ( 'SET', 'SYSTEM', 'TDB' )

         TSTRNG (  1  ) = '31 DEC 1989 15:59:00 '
         CALL TPARSE ( '31 DEC 1989 15:59:00 ', EXPDET(1), ERROR )
         CALL STR2ET ( TSTRNG(1), ET )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ET', ET, '~/', EXPDET(1), 1.0D-13, OK )

         CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )


      CALL TCASE ( 'Change the default time system and see that ' &
      //           'the change is properly reflected in the ' &
      //           'behavior of STR2ET. DEFSYS = TDT ' )

         CALL TIMDEF ( 'SET', 'SYSTEM', 'TDT' )

         TSTRNG (  1  ) = '31 DEC 1989 15:59:00 '
         CALL TPARSE ( '31 DEC 1989 15:59:00 ', ET, ERROR )
         EXPDET(1) = UNITIM ( ET, 'TDT', 'TDB' )

         CALL STR2ET ( TSTRNG(1), ET )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ET', ET, '~/', EXPDET(1), 1.0D-14, OK )

         CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )


      CALL TCASE ( 'Change the default time zone and make sure ' &
      //           'the change is properly reflected in the ' &
      //           'action of STR2ET. ZONE = ''PST'' ' )

         CALL TIMDEF ( 'SET', 'ZONE', 'PST' )

         TSTRNG(1) = 'Fri Sep 27 12:42:55 1996'
         CALL UTC2ET ( 'Fri Sep 27 20:42:55 1996', EXPDET(1) )

         CALL STR2ET ( TSTRNG(1), ET )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ET', ET, '~/', EXPDET(1), 1.0D-14, OK )

         CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )


      CALL TCASE ( 'Change the default calendar to JULIAN and ' &
      //           'make sure that we get the appropriate ' &
      //           'behavior from STR2ET. ' )


         CALL TIMDEF ( 'SET', 'CALENDAR', 'JULIAN' )

         TSTRNG(1) = 'Sep 3 12:42:55 1752'

         CALL STR2ET ( TSTRNG(1), ET )
         CALL UTC2ET ( 'Sep 14 12:42:55 1752', EXPDET (1) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ET', ET, '~', EXPDET(1), 1.0D-14, OK )


         TSTRNG(1) = '1752-200//12:42:55'
         CALL STR2ET ( TSTRNG(1), ET )
         CALL UTC2ET ( '1752-211//12:42:55', EXPDET (1) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ET', ET, '~', EXPDET(1), 1.0D-14, OK )

         CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )
         CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )


      CALL TCASE ( 'Change the default calendar to MIXED and ' &
      //           'make sure that we get the appropriate ' &
      //           'behavior from STR2ET. ' )

         CALL TIMDEF ( 'SET', 'CALENDAR', 'MIXED' )

         TSTRNG(1) = 'Fri Sep 27 13:08:46 1996'

         CALL STR2ET ( TSTRNG(1), ET )
         CALL UTC2ET ( 'Fri Sep 27 13:08:46 1996', EXPDET (1) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ET', ET, '~', EXPDET(1), 1.0D-14, OK )


         TSTRNG(1) = '1582 OCT 3, 12:42:55'
         CALL STR2ET ( TSTRNG(1), ET )
         CALL UTC2ET ( '1582 OCT 13, 12:42:55', EXPDET (1) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ET', ET, '~', EXPDET(1), 1.0D-14, OK )

         TSTRNG(1) = '1582 OCT 13, 12:42:55'
         CALL STR2ET ( TSTRNG(1), ET )
         CALL UTC2ET ( '1582 OCT 13, 12:42:55', EXPDET (1) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ET', ET, '~', EXPDET(1), 1.0D-14, OK )

         CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )
         CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )


      CALL TCASE ( 'Make sure that we can handle leap seconds on ' &
      //           'the Julian calendar. ' )

         CALL TIMDEF ( 'SET', 'CALENDAR', 'JULIAN' )

         TSTRNG(1) = 'December 18, 1996 23:59:60.6'

         CALL STR2ET ( TSTRNG(1), ET )
         CALL UTC2ET ( 'December 31, 1996 23:59:60.6', EXPDET (1) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ET', ET, '~', EXPDET(1), 1.0D-14, OK )
         CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )
         CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )


      CALL TCASE ( 'Make sure that we can handle leap seconds on ' &
      //           'the Julian calendar in non-UTC time zones. ' )

         CALL TIMDEF ( 'SET', 'CALENDAR', 'JULIAN' )
         CALL TIMDEF ( 'SET', 'ZONE',     'PST' )

         TSTRNG(1) = 'December 18, 1996 15:59:60.6'

         CALL STR2ET ( TSTRNG(1), ET )
         CALL UTC2ET ( 'December 31, 1996 23:59:60.6', EXPDET (1) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ET', ET, '~', EXPDET(1), 1.0D-14, OK )
         CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )
         CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )


      CALL TCASE ( 'Make sure that seconds out of range are ' &
      //           'properly diagnosed on the Julian Calendar. ' )

         CALL TIMDEF ( 'SET', 'CALENDAR', 'JULIAN' )

         TSTRNG(1) = 'December 17, 1996 23:59:60.6'

         CALL STR2ET ( TSTRNG(1), ET )

         CALL CHCKXC ( .TRUE., 'SPICE(BADTIMESTRING)', OK )


      CALL TCASE ( 'Make sure that out of range seconds are ' &
      //           'properly diagnosed on the Julian Calendar ' &
      //           'when using a non-UTC time zone. ' )


         CALL TIMDEF ( 'SET', 'CALENDAR', 'JULIAN' )
         CALL TIMDEF ( 'SET', 'ZONE',     'PST' )

         TSTRNG(1) = 'December 17, 1996 15:59:60.6'

         CALL STR2ET ( TSTRNG(1), ET )
         CALL CHCKXC ( .TRUE., 'SPICE(BADTIMESTRING)', OK )

      CALL TPARCH    ( 'NO' )

!
!     Leave time system in default state.
!
      CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TIMDEF ( 'SET', 'SYSTEM',   'UTC' )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The documentation of TIMDEF states that the above
!     call sets the time zone to blank ( ' ' ), so
!     no call to set the time zone is required.
!
      CALL T_SUCCESS (  OK  )
      RETURN
      END
