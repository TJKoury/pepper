!$Procedure      F_TIMCVR ( TIMOUT tests to exercise branch coverage)

      SUBROUTINE F_TIMCVR ( OK )

! $ Abstract
!
!     This routine generates a sufficiently large set of test cases
!     to exercise every reachable branch in TIMOUT.
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
!     B.V. Semenov     (JPL)
!     W.L. Taber       (JPL)
!     E.D. Wright      (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 05-NOV-2020 (EDW)
!
!        Added/modified test cases to confirm HR.###... function
!        with ::UTC+N:M and ::UTC-N:M meta tags.
!
!        Extended "::TDT" test blocks to perform the same
!        tests, using "::TT". timout now recognizes "::TT" as equivalent
!        to "::TDT".
!
!        Added missing header sections.
!
!-    TSPICE Version 1.0.0, 26-JUL-1996 (WLT)
!
!        A long time ago, probably before 26-JUL-1996 (WLT)
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!


!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(LNSIZE)    PICTUR
      CHARACTER*(LNSIZE)    EXPCTD( 48 )
      CHARACTER*(LNSIZE)    FTIME
      CHARACTER*(LNSIZE)    TIMES ( 48 )

      CHARACTER*(WDSIZE)    NAME

      DOUBLE PRECISION      ET

      INTEGER               I
      INTEGER               J


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_TIMCVR' )


      TIMES(01) = '12 B.C. JAN 01 10:00:00 '
      TIMES(02) = '12 B.C. FEB 05 11:00:00 '
      TIMES(03) = '12 B.C. MAR 10 12:00:00 '
      TIMES(04) = '12 B.C. APR 15 13:00:00 '
      TIMES(05) = '12 B.C. MAY 20 14:00:00 '
      TIMES(06) = '12 B.C. JUN 25 15:00:00 '
      TIMES(07) = '12 B.C. JUL 01 16:00:00 '
      TIMES(08) = '12 B.C. AUG 10 17:00:00 '
      TIMES(09) = '12 B.C. SEP 15 18:00:00 '
      TIMES(10) = '12 B.C. OCT 20 19:00:00 '
      TIMES(11) = '12 B.C. NOV 25 20:00:00 '
      TIMES(12) = '12 B.C. DEC 31 21:00:00 '


      TIMES(13) = '141 A.D. JAN 01 10:00:00 '
      TIMES(14) = '141 A.D. FEB 05 11:00:00 '
      TIMES(15) = '141 A.D. MAR 10 12:00:00 '
      TIMES(16) = '141 A.D. APR 15 13:00:00 '
      TIMES(17) = '141 A.D. MAY 20 14:00:00 '
      TIMES(18) = '141 A.D. JUN 25 15:00:00 '
      TIMES(19) = '141 A.D. JUL 01 16:00:00 '
      TIMES(20) = '141 A.D. AUG 10 17:00:00 '
      TIMES(21) = '141 A.D. SEP 15 18:00:00 '
      TIMES(22) = '141 A.D. OCT 20 19:00:00 '
      TIMES(23) = '141 A.D. NOV 25 20:00:00 '
      TIMES(24) = '141 A.D. DEC 31 21:00:00 '


      TIMES(25) = '1582 JAN 01  10:00:00 '
      TIMES(26) = '1582 FEB 05  11:00:00 '
      TIMES(27) = '1582 MAR 10  12:00:00 '
      TIMES(28) = '1582 APR 15  13:00:00 '
      TIMES(29) = '1582 MAY 20  14:00:00 '
      TIMES(30) = '1582 JUN 25  15:00:00 '
      TIMES(31) = '1582 JUL 01  16:00:00 '
      TIMES(32) = '1582 AUG 10  17:00:00 '
      TIMES(33) = '1582 SEP 15  18:00:00 '
      TIMES(34) = '1582 OCT 12  19:00:00 '
      TIMES(35) = '1582 OCT 25  20:00:00 '
      TIMES(36) = '1582 DEC 31  21:00:00 '


      TIMES(37) = '1986 JAN 01 10:00:00 '
      TIMES(38) = '1986 FEB 05 11:00:00 '
      TIMES(39) = '1986 MAR 10 12:00:00 '
      TIMES(40) = '1986 APR 15 13:00:00 '
      TIMES(41) = '1986 MAY 20 14:00:00 '
      TIMES(42) = '1986 JUN 25 15:00:00 '
      TIMES(43) = '1986 JUL 01 16:00:00 '
      TIMES(44) = '1986 AUG 10 17:00:00 '
      TIMES(45) = '1986 SEP 15 18:00:00 '
      TIMES(46) = '1986 OCT 20 19:00:00 '
      TIMES(47) = '1986 NOV 25 20:00:00 '
      TIMES(48) = '1986 DEC 31 21:00:00 '
!
!     Clear the kernel pool and then load a leapseconds kernel.
!
      CALL CLPOOL
      CALL TSTLSK


      PICTUR = 'YYYY ERA MON DD AP:MN:SC.### ampm PDT ::RND'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = '  12 B.C. JAN 01 10:00:00.000 a.m. PDT'
            EXPCTD(2)  = '  12 B.C. FEB 05 11:00:00.000 a.m. PDT'
            EXPCTD(3)  = '  12 B.C. MAR 10 12:00:00.000 p.m. PDT'
            EXPCTD(4)  = '  12 B.C. APR 15 01:00:00.000 p.m. PDT'
            EXPCTD(5)  = '  12 B.C. MAY 20 02:00:00.000 p.m. PDT'
            EXPCTD(6)  = '  12 B.C. JUN 25 03:00:00.000 p.m. PDT'
            EXPCTD(7)  = '  12 B.C. JUL 01 04:00:00.000 p.m. PDT'
            EXPCTD(8)  = '  12 B.C. AUG 10 05:00:00.000 p.m. PDT'
            EXPCTD(9)  = '  12 B.C. SEP 15 06:00:00.000 p.m. PDT'
            EXPCTD(10) = '  12 B.C. OCT 20 07:00:00.000 p.m. PDT'
            EXPCTD(11) = '  12 B.C. NOV 25 08:00:00.000 p.m. PDT'
            EXPCTD(12) = '  12 B.C. DEC 31 09:00:00.000 p.m. PDT'
            EXPCTD(13) = ' 141 A.D. JAN 01 10:00:00.000 a.m. PDT'
            EXPCTD(14) = ' 141 A.D. FEB 05 11:00:00.000 a.m. PDT'
            EXPCTD(15) = ' 141 A.D. MAR 10 12:00:00.000 p.m. PDT'
            EXPCTD(16) = ' 141 A.D. APR 15 01:00:00.000 p.m. PDT'
            EXPCTD(17) = ' 141 A.D. MAY 20 02:00:00.000 p.m. PDT'
            EXPCTD(18) = ' 141 A.D. JUN 25 03:00:00.000 p.m. PDT'
            EXPCTD(19) = ' 141 A.D. JUL 01 04:00:00.000 p.m. PDT'
            EXPCTD(20) = ' 141 A.D. AUG 10 05:00:00.000 p.m. PDT'
            EXPCTD(21) = ' 141 A.D. SEP 15 06:00:00.000 p.m. PDT'
            EXPCTD(22) = ' 141 A.D. OCT 20 07:00:00.000 p.m. PDT'
            EXPCTD(23) = ' 141 A.D. NOV 25 08:00:00.000 p.m. PDT'
            EXPCTD(24) = ' 141 A.D. DEC 31 09:00:00.000 p.m. PDT'
            EXPCTD(25) = '1582 A.D. JAN 01 10:00:00.000 a.m. PDT'
            EXPCTD(26) = '1582 A.D. FEB 05 11:00:00.000 a.m. PDT'
            EXPCTD(27) = '1582 A.D. MAR 10 12:00:00.000 p.m. PDT'
            EXPCTD(28) = '1582 A.D. APR 15 01:00:00.000 p.m. PDT'
            EXPCTD(29) = '1582 A.D. MAY 20 02:00:00.000 p.m. PDT'
            EXPCTD(30) = '1582 A.D. JUN 25 03:00:00.000 p.m. PDT'
            EXPCTD(31) = '1582 A.D. JUL 01 04:00:00.000 p.m. PDT'
            EXPCTD(32) = '1582 A.D. AUG 10 05:00:00.000 p.m. PDT'
            EXPCTD(33) = '1582 A.D. SEP 15 06:00:00.000 p.m. PDT'
            EXPCTD(34) = '1582 A.D. OCT 12 07:00:00.000 p.m. PDT'
            EXPCTD(35) = '1582 A.D. OCT 25 08:00:00.000 p.m. PDT'
            EXPCTD(36) = '1582 A.D. DEC 31 09:00:00.000 p.m. PDT'
            EXPCTD(37) = '1986 A.D. JAN 01 10:00:00.000 a.m. PDT'
            EXPCTD(38) = '1986 A.D. FEB 05 11:00:00.000 a.m. PDT'
            EXPCTD(39) = '1986 A.D. MAR 10 12:00:00.000 p.m. PDT'
            EXPCTD(40) = '1986 A.D. APR 15 01:00:00.000 p.m. PDT'
            EXPCTD(41) = '1986 A.D. MAY 20 02:00:00.000 p.m. PDT'
            EXPCTD(42) = '1986 A.D. JUN 25 03:00:00.000 p.m. PDT'
            EXPCTD(43) = '1986 A.D. JUL 01 04:00:00.000 p.m. PDT'
            EXPCTD(44) = '1986 A.D. AUG 10 05:00:00.000 p.m. PDT'
            EXPCTD(45) = '1986 A.D. SEP 15 06:00:00.000 p.m. PDT'
            EXPCTD(46) = '1986 A.D. OCT 20 07:00:00.000 p.m. PDT'
            EXPCTD(47) = '1986 A.D. NOV 25 08:00:00.000 p.m. PDT'
            EXPCTD(48) = '1986 A.D. DEC 31 09:00:00.000 p.m. PDT'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )
               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'YYYY MM DD Weekday HR:MN:SC.## ::TRNC '

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = ' -11 01 01 Sunday 10:00:41.18'
            EXPCTD(2)  = ' -11 02 05 Sunday 11:00:41.18'
            EXPCTD(3)  = ' -11 03 10 Friday 12:00:41.18'
            EXPCTD(4)  = ' -11 04 15 Saturday 13:00:41.18'
            EXPCTD(5)  = ' -11 05 20 Saturday 14:00:41.18'
            EXPCTD(6)  = ' -11 06 25 Sunday 15:00:41.18'
            EXPCTD(7)  = ' -11 07 01 Saturday 16:00:41.18'
            EXPCTD(8)  = ' -11 08 10 Thursday 17:00:41.18'
            EXPCTD(9)  = ' -11 09 15 Friday 18:00:41.18'
            EXPCTD(10) = ' -11 10 20 Friday 19:00:41.18'
            EXPCTD(11) = ' -11 11 25 Saturday 20:00:41.18'
            EXPCTD(12) = ' -11 12 31 Sunday 21:00:41.18'
            EXPCTD(13) = ' 141 01 01 Sunday 10:00:41.18'
            EXPCTD(14) = ' 141 02 05 Sunday 11:00:41.18'
            EXPCTD(15) = ' 141 03 10 Friday 12:00:41.18'
            EXPCTD(16) = ' 141 04 15 Saturday 13:00:41.18'
            EXPCTD(17) = ' 141 05 20 Saturday 14:00:41.18'
            EXPCTD(18) = ' 141 06 25 Sunday 15:00:41.18'
            EXPCTD(19) = ' 141 07 01 Saturday 16:00:41.18'
            EXPCTD(20) = ' 141 08 10 Thursday 17:00:41.18'
            EXPCTD(21) = ' 141 09 15 Friday 18:00:41.18'
            EXPCTD(22) = ' 141 10 20 Friday 19:00:41.18'
            EXPCTD(23) = ' 141 11 25 Saturday 20:00:41.18'
            EXPCTD(24) = ' 141 12 31 Sunday 21:00:41.18'
            EXPCTD(25) = '1582 01 01 Friday 10:00:41.18'
            EXPCTD(26) = '1582 02 05 Friday 11:00:41.18'
            EXPCTD(27) = '1582 03 10 Wednesday 12:00:41.18'
            EXPCTD(28) = '1582 04 15 Thursday 13:00:41.18'
            EXPCTD(29) = '1582 05 20 Thursday 14:00:41.18'
            EXPCTD(30) = '1582 06 25 Friday 15:00:41.18'
            EXPCTD(31) = '1582 07 01 Thursday 16:00:41.18'
            EXPCTD(32) = '1582 08 10 Tuesday 17:00:41.18'
            EXPCTD(33) = '1582 09 15 Wednesday 18:00:41.18'
            EXPCTD(34) = '1582 10 12 Tuesday 19:00:41.18'
            EXPCTD(35) = '1582 10 25 Monday 20:00:41.18'
            EXPCTD(36) = '1582 12 31 Friday 21:00:41.18'
            EXPCTD(37) = '1986 01 01 Wednesday 10:00:55.18'
            EXPCTD(38) = '1986 02 05 Wednesday 11:00:55.18'
            EXPCTD(39) = '1986 03 10 Monday 12:00:55.18'
            EXPCTD(40) = '1986 04 15 Tuesday 13:00:55.18'
            EXPCTD(41) = '1986 05 20 Tuesday 14:00:55.18'
            EXPCTD(42) = '1986 06 25 Wednesday 15:00:55.18'
            EXPCTD(43) = '1986 07 01 Tuesday 16:00:55.18'
            EXPCTD(44) = '1986 08 10 Sunday 17:00:55.18'
            EXPCTD(45) = '1986 09 15 Monday 18:00:55.18'
            EXPCTD(46) = '1986 10 20 Monday 19:00:55.18'
            EXPCTD(47) = '1986 11 25 Tuesday 20:00:55.18'
            EXPCTD(48) = '1986 12 31 Wednesday 21:00:55.18'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET )
               CALL TIMOUT ( ET, PICTUR//'::TDT', FTIME )

               NAME = 'FTIME(#) TDT'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )

               CALL TIMOUT ( ET, PICTUR//'::TT', FTIME )

               NAME = 'FTIME(#) TT'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )

            END DO


      PICTUR = 'YYYY Mon DD HR:MN:SC.## Wkd'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'JULIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'TDB' )

            EXPCTD(1)  = ' -11 Jan 01 10:00:00.00 Fri'
            EXPCTD(2)  = ' -11 Feb 05 11:00:00.00 Fri'
            EXPCTD(3)  = ' -11 Mar 10 12:00:00.00 Wed'
            EXPCTD(4)  = ' -11 Apr 15 13:00:00.00 Thu'
            EXPCTD(5)  = ' -11 May 20 14:00:00.00 Thu'
            EXPCTD(6)  = ' -11 Jun 25 15:00:00.00 Fri'
            EXPCTD(7)  = ' -11 Jul 01 16:00:00.00 Thu'
            EXPCTD(8)  = ' -11 Aug 10 17:00:00.00 Tue'
            EXPCTD(9)  = ' -11 Sep 15 18:00:00.00 Wed'
            EXPCTD(10) = ' -11 Oct 20 19:00:00.00 Wed'
            EXPCTD(11) = ' -11 Nov 25 20:00:00.00 Thu'
            EXPCTD(12) = ' -11 Dec 31 21:00:00.00 Fri'
            EXPCTD(13) = ' 141 Jan 01 10:00:00.00 Sat'
            EXPCTD(14) = ' 141 Feb 05 11:00:00.00 Sat'
            EXPCTD(15) = ' 141 Mar 10 12:00:00.00 Thu'
            EXPCTD(16) = ' 141 Apr 15 13:00:00.00 Fri'
            EXPCTD(17) = ' 141 May 20 14:00:00.00 Fri'
            EXPCTD(18) = ' 141 Jun 25 15:00:00.00 Sat'
            EXPCTD(19) = ' 141 Jul 01 16:00:00.00 Fri'
            EXPCTD(20) = ' 141 Aug 10 17:00:00.00 Wed'
            EXPCTD(21) = ' 141 Sep 15 18:00:00.00 Thu'
            EXPCTD(22) = ' 141 Oct 20 19:00:00.00 Thu'
            EXPCTD(23) = ' 141 Nov 25 20:00:00.00 Fri'
            EXPCTD(24) = ' 141 Dec 31 21:00:00.00 Sat'
            EXPCTD(25) = '1582 Jan 01 10:00:00.00 Mon'
            EXPCTD(26) = '1582 Feb 05 11:00:00.00 Mon'
            EXPCTD(27) = '1582 Mar 10 12:00:00.00 Sat'
            EXPCTD(28) = '1582 Apr 15 13:00:00.00 Sun'
            EXPCTD(29) = '1582 May 20 14:00:00.00 Sun'
            EXPCTD(30) = '1582 Jun 25 15:00:00.00 Mon'
            EXPCTD(31) = '1582 Jul 01 16:00:00.00 Sun'
            EXPCTD(32) = '1582 Aug 10 17:00:00.00 Fri'
            EXPCTD(33) = '1582 Sep 15 18:00:00.00 Sat'
            EXPCTD(34) = '1582 Oct 12 19:00:00.00 Fri'
            EXPCTD(35) = '1582 Oct 25 20:00:00.00 Thu'
            EXPCTD(36) = '1582 Dec 31 21:00:00.00 Mon'
            EXPCTD(37) = '1986 Jan 01 10:00:00.00 Tue'
            EXPCTD(38) = '1986 Feb 05 11:00:00.00 Tue'
            EXPCTD(39) = '1986 Mar 10 12:00:00.00 Sun'
            EXPCTD(40) = '1986 Apr 15 13:00:00.00 Mon'
            EXPCTD(41) = '1986 May 20 14:00:00.00 Mon'
            EXPCTD(42) = '1986 Jun 25 15:00:00.00 Tue'
            EXPCTD(43) = '1986 Jul 01 16:00:00.00 Mon'
            EXPCTD(44) = '1986 Aug 10 17:00:00.00 Sat'
            EXPCTD(45) = '1986 Sep 15 18:00:00.00 Sun'
            EXPCTD(46) = '1986 Oct 20 19:00:00.00 Sun'
            EXPCTD(47) = '1986 Nov 25 20:00:00.00 Mon'
            EXPCTD(48) = '1986 Dec 31 21:00:00.00 Tue'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )
               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'YYYY Mon DD HR:MN:SC.### Wkd ::TDB'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'MIXED' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'TDB' )

            EXPCTD(1)  = ' -11 Jan 01 10:00:00.000 Fri'
            EXPCTD(2)  = ' -11 Feb 05 11:00:00.000 Fri'
            EXPCTD(3)  = ' -11 Mar 10 12:00:00.000 Wed'
            EXPCTD(4)  = ' -11 Apr 15 13:00:00.000 Thu'
            EXPCTD(5)  = ' -11 May 20 14:00:00.000 Thu'
            EXPCTD(6)  = ' -11 Jun 25 15:00:00.000 Fri'
            EXPCTD(7)  = ' -11 Jul 01 16:00:00.000 Thu'
            EXPCTD(8)  = ' -11 Aug 10 17:00:00.000 Tue'
            EXPCTD(9)  = ' -11 Sep 15 18:00:00.000 Wed'
            EXPCTD(10) = ' -11 Oct 20 19:00:00.000 Wed'
            EXPCTD(11) = ' -11 Nov 25 20:00:00.000 Thu'
            EXPCTD(12) = ' -11 Dec 31 21:00:00.000 Fri'
            EXPCTD(13) = ' 141 Jan 01 10:00:00.000 Sat'
            EXPCTD(14) = ' 141 Feb 05 11:00:00.000 Sat'
            EXPCTD(15) = ' 141 Mar 10 12:00:00.000 Thu'
            EXPCTD(16) = ' 141 Apr 15 13:00:00.000 Fri'
            EXPCTD(17) = ' 141 May 20 14:00:00.000 Fri'
            EXPCTD(18) = ' 141 Jun 25 15:00:00.000 Sat'
            EXPCTD(19) = ' 141 Jul 01 16:00:00.000 Fri'
            EXPCTD(20) = ' 141 Aug 10 17:00:00.000 Wed'
            EXPCTD(21) = ' 141 Sep 15 18:00:00.000 Thu'
            EXPCTD(22) = ' 141 Oct 20 19:00:00.000 Thu'
            EXPCTD(23) = ' 141 Nov 25 20:00:00.000 Fri'
            EXPCTD(24) = ' 141 Dec 31 21:00:00.000 Sat'
            EXPCTD(25) = '1582 Jan 01 10:00:00.000 Mon'
            EXPCTD(26) = '1582 Feb 05 11:00:00.000 Mon'
            EXPCTD(27) = '1582 Mar 10 12:00:00.000 Sat'
            EXPCTD(28) = '1582 Apr 15 13:00:00.000 Sun'
            EXPCTD(29) = '1582 May 20 14:00:00.000 Sun'
            EXPCTD(30) = '1582 Jun 25 15:00:00.000 Mon'
            EXPCTD(31) = '1582 Jul 01 16:00:00.000 Sun'
            EXPCTD(32) = '1582 Aug 10 17:00:00.000 Fri'
            EXPCTD(33) = '1582 Sep 15 18:00:00.000 Sat'
            EXPCTD(34) = '1582 Oct 02 19:00:00.000 Tue'
            EXPCTD(35) = '1582 Oct 25 20:00:00.000 Mon'
            EXPCTD(36) = '1582 Dec 31 21:00:00.000 Fri'
            EXPCTD(37) = '1986 Jan 01 10:00:00.000 Wed'
            EXPCTD(38) = '1986 Feb 05 11:00:00.000 Wed'
            EXPCTD(39) = '1986 Mar 10 12:00:00.000 Mon'
            EXPCTD(40) = '1986 Apr 15 13:00:00.000 Tue'
            EXPCTD(41) = '1986 May 20 14:00:00.000 Tue'
            EXPCTD(42) = '1986 Jun 25 15:00:00.000 Wed'
            EXPCTD(43) = '1986 Jul 01 16:00:00.000 Tue'
            EXPCTD(44) = '1986 Aug 10 17:00:00.000 Sun'
            EXPCTD(45) = '1986 Sep 15 18:00:00.000 Mon'
            EXPCTD(46) = '1986 Oct 20 19:00:00.000 Mon'
            EXPCTD(47) = '1986 Nov 25 20:00:00.000 Tue'
            EXPCTD(48) = '1986 Dec 31 21:00:00.000 Wed'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )
               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'YYYY mon DD wkd HR:MN:SC.### AMPM'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'TDB' )

            EXPCTD(1)  = ' -11 jan 01 sun 10:00:00.000 A.M.'
            EXPCTD(2)  = ' -11 feb 05 sun 11:00:00.000 A.M.'
            EXPCTD(3)  = ' -11 mar 10 fri 12:00:00.000 P.M.'
            EXPCTD(4)  = ' -11 apr 15 sat 13:00:00.000 P.M.'
            EXPCTD(5)  = ' -11 may 20 sat 14:00:00.000 P.M.'
            EXPCTD(6)  = ' -11 jun 25 sun 15:00:00.000 P.M.'
            EXPCTD(7)  = ' -11 jul 01 sat 16:00:00.000 P.M.'
            EXPCTD(8)  = ' -11 aug 10 thu 17:00:00.000 P.M.'
            EXPCTD(9)  = ' -11 sep 15 fri 18:00:00.000 P.M.'
            EXPCTD(10) = ' -11 oct 20 fri 19:00:00.000 P.M.'
            EXPCTD(11) = ' -11 nov 25 sat 20:00:00.000 P.M.'
            EXPCTD(12) = ' -11 dec 31 sun 21:00:00.000 P.M.'
            EXPCTD(13) = ' 141 jan 01 sun 10:00:00.000 A.M.'
            EXPCTD(14) = ' 141 feb 05 sun 11:00:00.000 A.M.'
            EXPCTD(15) = ' 141 mar 10 fri 12:00:00.000 P.M.'
            EXPCTD(16) = ' 141 apr 15 sat 13:00:00.000 P.M.'
            EXPCTD(17) = ' 141 may 20 sat 14:00:00.000 P.M.'
            EXPCTD(18) = ' 141 jun 25 sun 15:00:00.000 P.M.'
            EXPCTD(19) = ' 141 jul 01 sat 16:00:00.000 P.M.'
            EXPCTD(20) = ' 141 aug 10 thu 17:00:00.000 P.M.'
            EXPCTD(21) = ' 141 sep 15 fri 18:00:00.000 P.M.'
            EXPCTD(22) = ' 141 oct 20 fri 19:00:00.000 P.M.'
            EXPCTD(23) = ' 141 nov 25 sat 20:00:00.000 P.M.'
            EXPCTD(24) = ' 141 dec 31 sun 21:00:00.000 P.M.'
            EXPCTD(25) = '1582 jan 01 fri 10:00:00.000 A.M.'
            EXPCTD(26) = '1582 feb 05 fri 11:00:00.000 A.M.'
            EXPCTD(27) = '1582 mar 10 wed 12:00:00.000 P.M.'
            EXPCTD(28) = '1582 apr 15 thu 13:00:00.000 P.M.'
            EXPCTD(29) = '1582 may 20 thu 14:00:00.000 P.M.'
            EXPCTD(30) = '1582 jun 25 fri 15:00:00.000 P.M.'
            EXPCTD(31) = '1582 jul 01 thu 16:00:00.000 P.M.'
            EXPCTD(32) = '1582 aug 10 tue 17:00:00.000 P.M.'
            EXPCTD(33) = '1582 sep 15 wed 18:00:00.000 P.M.'
            EXPCTD(34) = '1582 oct 12 tue 19:00:00.000 P.M.'
            EXPCTD(35) = '1582 oct 25 mon 20:00:00.000 P.M.'
            EXPCTD(36) = '1582 dec 31 fri 21:00:00.000 P.M.'
            EXPCTD(37) = '1986 jan 01 wed 10:00:00.000 A.M.'
            EXPCTD(38) = '1986 feb 05 wed 11:00:00.000 A.M.'
            EXPCTD(39) = '1986 mar 10 mon 12:00:00.000 P.M.'
            EXPCTD(40) = '1986 apr 15 tue 13:00:00.000 P.M.'
            EXPCTD(41) = '1986 may 20 tue 14:00:00.000 P.M.'
            EXPCTD(42) = '1986 jun 25 wed 15:00:00.000 P.M.'
            EXPCTD(43) = '1986 jul 01 tue 16:00:00.000 P.M.'
            EXPCTD(44) = '1986 aug 10 sun 17:00:00.000 P.M.'
            EXPCTD(45) = '1986 sep 15 mon 18:00:00.000 P.M.'
            EXPCTD(46) = '1986 oct 20 mon 19:00:00.000 P.M.'
            EXPCTD(47) = '1986 nov 25 tue 20:00:00.000 P.M.'
            EXPCTD(48) = '1986 dec 31 wed 21:00:00.000 P.M.'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )
               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'YYYY Mon DD WKD'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )

            EXPCTD(1)  = ' -11 Jan 01 SUN'
            EXPCTD(2)  = ' -11 Feb 05 SUN'
            EXPCTD(3)  = ' -11 Mar 10 FRI'
            EXPCTD(4)  = ' -11 Apr 15 SAT'
            EXPCTD(5)  = ' -11 May 20 SAT'
            EXPCTD(6)  = ' -11 Jun 25 SUN'
            EXPCTD(7)  = ' -11 Jul 01 SAT'
            EXPCTD(8)  = ' -11 Aug 10 THU'
            EXPCTD(9)  = ' -11 Sep 15 FRI'
            EXPCTD(10) = ' -11 Oct 20 FRI'
            EXPCTD(11) = ' -11 Nov 25 SAT'
            EXPCTD(12) = ' -11 Dec 31 SUN'
            EXPCTD(13) = ' 141 Jan 01 SUN'
            EXPCTD(14) = ' 141 Feb 05 SUN'
            EXPCTD(15) = ' 141 Mar 10 FRI'
            EXPCTD(16) = ' 141 Apr 15 SAT'
            EXPCTD(17) = ' 141 May 20 SAT'
            EXPCTD(18) = ' 141 Jun 25 SUN'
            EXPCTD(19) = ' 141 Jul 01 SAT'
            EXPCTD(20) = ' 141 Aug 10 THU'
            EXPCTD(21) = ' 141 Sep 15 FRI'
            EXPCTD(22) = ' 141 Oct 20 FRI'
            EXPCTD(23) = ' 141 Nov 25 SAT'
            EXPCTD(24) = ' 141 Dec 31 SUN'
            EXPCTD(25) = '1582 Jan 01 FRI'
            EXPCTD(26) = '1582 Feb 05 FRI'
            EXPCTD(27) = '1582 Mar 10 WED'
            EXPCTD(28) = '1582 Apr 15 THU'
            EXPCTD(29) = '1582 May 20 THU'
            EXPCTD(30) = '1582 Jun 25 FRI'
            EXPCTD(31) = '1582 Jul 01 THU'
            EXPCTD(32) = '1582 Aug 10 TUE'
            EXPCTD(33) = '1582 Sep 15 WED'
            EXPCTD(34) = '1582 Oct 12 TUE'
            EXPCTD(35) = '1582 Oct 25 MON'
            EXPCTD(36) = '1582 Dec 31 FRI'
            EXPCTD(37) = '1986 Jan 01 WED'
            EXPCTD(38) = '1986 Feb 05 WED'
            EXPCTD(39) = '1986 Mar 10 MON'
            EXPCTD(40) = '1986 Apr 15 TUE'
            EXPCTD(41) = '1986 May 20 TUE'
            EXPCTD(42) = '1986 Jun 25 WED'
            EXPCTD(43) = '1986 Jul 01 TUE'
            EXPCTD(44) = '1986 Aug 10 SUN'
            EXPCTD(45) = '1986 Sep 15 MON'
            EXPCTD(46) = '1986 Oct 20 MON'
            EXPCTD(47) = '1986 Nov 25 TUE'
            EXPCTD(48) = '1986 Dec 31 WED'


            DO J = 1, 2

               IF ( J .EQ. 1 ) THEN
                  CALL TIMDEF ( 'SET', 'SYSTEM', 'TDT' )
               ELSE
                  CALL TIMDEF ( 'SET', 'SYSTEM', 'TT' )
               END IF

               DO I = 1, 48
                  CALL STR2ET ( TIMES(I),   ET    )
                  CALL TIMOUT ( ET, PICTUR, FTIME )

                  NAME = 'FTIME(#)'
                  CALL REPMI ( NAME, '#', I, NAME )

                  CALL CHCKXC ( .FALSE., ' ',  OK )
                  CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
               END DO

            END DO


      PICTUR = 'YYYY month weekday DD HR:MN.###'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'ZONE', 'UTC+8' )

            EXPCTD(1)  = ' -11 january sunday 01 10:00.000'
            EXPCTD(2)  = ' -11 february sunday 05 11:00.000'
            EXPCTD(3)  = ' -11 march friday 10 12:00.000'
            EXPCTD(4)  = ' -11 april saturday 15 13:00.000'
            EXPCTD(5)  = ' -11 may saturday 20 14:00.000'
            EXPCTD(6)  = ' -11 june sunday 25 15:00.000'
            EXPCTD(7)  = ' -11 july saturday 01 16:00.000'
            EXPCTD(8)  = ' -11 august thursday 10 17:00.000'
            EXPCTD(9)  = ' -11 september friday 15 18:00.000'
            EXPCTD(10) = ' -11 october friday 20 19:00.000'
            EXPCTD(11) = ' -11 november saturday 25 20:00.000'
            EXPCTD(12) = ' -11 december sunday 31 21:00.000'
            EXPCTD(13) = ' 141 january sunday 01 10:00.000'
            EXPCTD(14) = ' 141 february sunday 05 11:00.000'
            EXPCTD(15) = ' 141 march friday 10 12:00.000'
            EXPCTD(16) = ' 141 april saturday 15 13:00.000'
            EXPCTD(17) = ' 141 may saturday 20 14:00.000'
            EXPCTD(18) = ' 141 june sunday 25 15:00.000'
            EXPCTD(19) = ' 141 july saturday 01 16:00.000'
            EXPCTD(20) = ' 141 august thursday 10 17:00.000'
            EXPCTD(21) = ' 141 september friday 15 18:00.000'
            EXPCTD(22) = ' 141 october friday 20 19:00.000'
            EXPCTD(23) = ' 141 november saturday 25 20:00.000'
            EXPCTD(24) = ' 141 december sunday 31 21:00.000'
            EXPCTD(25) = '1582 january friday 01 10:00.000'
            EXPCTD(26) = '1582 february friday 05 11:00.000'
            EXPCTD(27) = '1582 march wednesday 10 12:00.000'
            EXPCTD(28) = '1582 april thursday 15 13:00.000'
            EXPCTD(29) = '1582 may thursday 20 14:00.000'
            EXPCTD(30) = '1582 june friday 25 15:00.000'
            EXPCTD(31) = '1582 july thursday 01 16:00.000'
            EXPCTD(32) = '1582 august tuesday 10 17:00.000'
            EXPCTD(33) = '1582 september wednesday 15 18:00.000'
            EXPCTD(34) = '1582 october tuesday 12 19:00.000'
            EXPCTD(35) = '1582 october monday 25 20:00.000'
            EXPCTD(36) = '1582 december friday 31 21:00.000'
            EXPCTD(37) = '1986 january wednesday 01 10:00.000'
            EXPCTD(38) = '1986 february wednesday 05 11:00.000'
            EXPCTD(39) = '1986 march monday 10 12:00.000'
            EXPCTD(40) = '1986 april tuesday 15 13:00.000'
            EXPCTD(41) = '1986 may tuesday 20 14:00.000'
            EXPCTD(42) = '1986 june wednesday 25 15:00.000'
            EXPCTD(43) = '1986 july tuesday 01 16:00.000'
            EXPCTD(44) = '1986 august sunday 10 17:00.000'
            EXPCTD(45) = '1986 september monday 15 18:00.000'
            EXPCTD(46) = '1986 october monday 20 19:00.000'
            EXPCTD(47) = '1986 november tuesday 25 20:00.000'
            EXPCTD(48) = '1986 december wednesday 31 21:00.000'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )
               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'YYYY MONTH DD::UTC+07:30-HR:MN ::MCAL'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = ' -11 JANUARY 03-17:30'
            EXPCTD(2)  = ' -11 FEBRUARY 07-18:30'
            EXPCTD(3)  = ' -11 MARCH 12-19:30'
            EXPCTD(4)  = ' -11 APRIL 17-20:30'
            EXPCTD(5)  = ' -11 MAY 22-21:30'
            EXPCTD(6)  = ' -11 JUNE 27-22:30'
            EXPCTD(7)  = ' -11 JULY 03-23:30'
            EXPCTD(8)  = ' -11 AUGUST 13-00:30'
            EXPCTD(9)  = ' -11 SEPTEMBER 18-01:30'
            EXPCTD(10) = ' -11 OCTOBER 23-02:30'
            EXPCTD(11) = ' -11 NOVEMBER 28-03:30'
            EXPCTD(12) = ' -10 JANUARY 03-04:30'
            EXPCTD(13) = ' 141 JANUARY 02-17:30'
            EXPCTD(14) = ' 141 FEBRUARY 06-18:30'
            EXPCTD(15) = ' 141 MARCH 11-19:30'
            EXPCTD(16) = ' 141 APRIL 16-20:30'
            EXPCTD(17) = ' 141 MAY 21-21:30'
            EXPCTD(18) = ' 141 JUNE 26-22:30'
            EXPCTD(19) = ' 141 JULY 02-23:30'
            EXPCTD(20) = ' 141 AUGUST 12-00:30'
            EXPCTD(21) = ' 141 SEPTEMBER 17-01:30'
            EXPCTD(22) = ' 141 OCTOBER 22-02:30'
            EXPCTD(23) = ' 141 NOVEMBER 27-03:30'
            EXPCTD(24) = ' 142 JANUARY 02-04:30'
            EXPCTD(25) = '1581 DECEMBER 22-17:30'
            EXPCTD(26) = '1582 JANUARY 26-18:30'
            EXPCTD(27) = '1582 FEBRUARY 28-19:30'
            EXPCTD(28) = '1582 APRIL 05-20:30'
            EXPCTD(29) = '1582 MAY 10-21:30'
            EXPCTD(30) = '1582 JUNE 15-22:30'
            EXPCTD(31) = '1582 JUNE 21-23:30'
            EXPCTD(32) = '1582 AUGUST 01-00:30'
            EXPCTD(33) = '1582 SEPTEMBER 06-01:30'
            EXPCTD(34) = '1582 OCTOBER 03-02:30'
            EXPCTD(35) = '1582 OCTOBER 26-03:30'
            EXPCTD(36) = '1583 JANUARY 01-04:30'
            EXPCTD(37) = '1986 JANUARY 01-17:30'
            EXPCTD(38) = '1986 FEBRUARY 05-18:30'
            EXPCTD(39) = '1986 MARCH 10-19:30'
            EXPCTD(40) = '1986 APRIL 15-20:30'
            EXPCTD(41) = '1986 MAY 20-21:30'
            EXPCTD(42) = '1986 JUNE 25-22:30'
            EXPCTD(43) = '1986 JULY 01-23:30'
            EXPCTD(44) = '1986 AUGUST 11-00:30'
            EXPCTD(45) = '1986 SEPTEMBER 16-01:30'
            EXPCTD(46) = '1986 OCTOBER 21-02:30'
            EXPCTD(47) = '1986 NOVEMBER 26-03:30'
            EXPCTD(48) = '1987 JANUARY 01-04:30'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )

               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'YYYY MONTH DD HR.### ::UTC+07:40::MCAL::RND'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = ' -11 JANUARY 03 17.667'
            EXPCTD(2)  = ' -11 FEBRUARY 07 18.667'
            EXPCTD(3)  = ' -11 MARCH 12 19.667'
            EXPCTD(4)  = ' -11 APRIL 17 20.667'
            EXPCTD(5)  = ' -11 MAY 22 21.667'
            EXPCTD(6)  = ' -11 JUNE 27 22.667'
            EXPCTD(7)  = ' -11 JULY 03 23.667'
            EXPCTD(8)  = ' -11 AUGUST 13 00.667'
            EXPCTD(9)  = ' -11 SEPTEMBER 18 01.667'
            EXPCTD(10) = ' -11 OCTOBER 23 02.667'
            EXPCTD(11) = ' -11 NOVEMBER 28 03.667'
            EXPCTD(12) = ' -10 JANUARY 03 04.667'
            EXPCTD(13) = ' 141 JANUARY 02 17.667'
            EXPCTD(14) = ' 141 FEBRUARY 06 18.667'
            EXPCTD(15) = ' 141 MARCH 11 19.667'
            EXPCTD(16) = ' 141 APRIL 16 20.667'
            EXPCTD(17) = ' 141 MAY 21 21.667'
            EXPCTD(18) = ' 141 JUNE 26 22.667'
            EXPCTD(19) = ' 141 JULY 02 23.667'
            EXPCTD(20) = ' 141 AUGUST 12 00.667'
            EXPCTD(21) = ' 141 SEPTEMBER 17 01.667'
            EXPCTD(22) = ' 141 OCTOBER 22 02.667'
            EXPCTD(23) = ' 141 NOVEMBER 27 03.667'
            EXPCTD(24) = ' 142 JANUARY 02 04.667'
            EXPCTD(25) = '1581 DECEMBER 22 17.667'
            EXPCTD(26) = '1582 JANUARY 26 18.667'
            EXPCTD(27) = '1582 FEBRUARY 28 19.667'
            EXPCTD(28) = '1582 APRIL 05 20.667'
            EXPCTD(29) = '1582 MAY 10 21.667'
            EXPCTD(30) = '1582 JUNE 15 22.667'
            EXPCTD(31) = '1582 JUNE 21 23.667'
            EXPCTD(32) = '1582 AUGUST 01 00.667'
            EXPCTD(33) = '1582 SEPTEMBER 06 01.667'
            EXPCTD(34) = '1582 OCTOBER 03 02.667'
            EXPCTD(35) = '1582 OCTOBER 26 03.667'
            EXPCTD(36) = '1583 JANUARY 01 04.667'
            EXPCTD(37) = '1986 JANUARY 01 17.667'
            EXPCTD(38) = '1986 FEBRUARY 05 18.667'
            EXPCTD(39) = '1986 MARCH 10 19.667'
            EXPCTD(40) = '1986 APRIL 15 20.667'
            EXPCTD(41) = '1986 MAY 20 21.667'
            EXPCTD(42) = '1986 JUNE 25 22.667'
            EXPCTD(43) = '1986 JULY 01 23.667'
            EXPCTD(44) = '1986 AUGUST 11 00.667'
            EXPCTD(45) = '1986 SEPTEMBER 16 01.667'
            EXPCTD(46) = '1986 OCTOBER 21 02.667'
            EXPCTD(47) = '1986 NOVEMBER 26 03.667'
            EXPCTD(48) = '1987 JANUARY 01 04.667'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )

               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO



      PICTUR = 'YYYY MONTH DD ::UTC-07:20HR.### .# SC. ::JULIAN'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = ' -11 JANUARY 01 02.666 .# 00. ::JULIAN'
            EXPCTD(2)  = ' -11 FEBRUARY 05 03.666 .# 00. ::JULIAN'
            EXPCTD(3)  = ' -11 MARCH 10 04.666 .# 00. ::JULIAN'
            EXPCTD(4)  = ' -11 APRIL 15 05.666 .# 00. ::JULIAN'
            EXPCTD(5)  = ' -11 MAY 20 06.666 .# 00. ::JULIAN'
            EXPCTD(6)  = ' -11 JUNE 25 07.666 .# 00. ::JULIAN'
            EXPCTD(7)  = ' -11 JULY 01 08.666 .# 00. ::JULIAN'
            EXPCTD(8)  = ' -11 AUGUST 10 09.666 .# 00. ::JULIAN'
            EXPCTD(9)  = ' -11 SEPTEMBER 15 10.666 .# 00. ::JULIAN'
            EXPCTD(10) = ' -11 OCTOBER 20 11.666 .# 00. ::JULIAN'
            EXPCTD(11) = ' -11 NOVEMBER 25 12.666 .# 00. ::JULIAN'
            EXPCTD(12) = ' -11 DECEMBER 31 13.666 .# 00. ::JULIAN'
            EXPCTD(13) = ' 141 JANUARY 01 02.666 .# 00. ::JULIAN'
            EXPCTD(14) = ' 141 FEBRUARY 05 03.666 .# 00. ::JULIAN'
            EXPCTD(15) = ' 141 MARCH 10 04.666 .# 00. ::JULIAN'
            EXPCTD(16) = ' 141 APRIL 15 05.666 .# 00. ::JULIAN'
            EXPCTD(17) = ' 141 MAY 20 06.666 .# 00. ::JULIAN'
            EXPCTD(18) = ' 141 JUNE 25 07.666 .# 00. ::JULIAN'
            EXPCTD(19) = ' 141 JULY 01 08.666 .# 00. ::JULIAN'
            EXPCTD(20) = ' 141 AUGUST 10 09.666 .# 00. ::JULIAN'
            EXPCTD(21) = ' 141 SEPTEMBER 15 10.666 .# 00. ::JULIAN'
            EXPCTD(22) = ' 141 OCTOBER 20 11.666 .# 00. ::JULIAN'
            EXPCTD(23) = ' 141 NOVEMBER 25 12.666 .# 00. ::JULIAN'
            EXPCTD(24) = ' 141 DECEMBER 31 13.666 .# 00. ::JULIAN'
            EXPCTD(25) = '1582 JANUARY 01 02.666 .# 00. ::JULIAN'
            EXPCTD(26) = '1582 FEBRUARY 05 03.666 .# 00. ::JULIAN'
            EXPCTD(27) = '1582 MARCH 10 04.666 .# 00. ::JULIAN'
            EXPCTD(28) = '1582 APRIL 15 05.666 .# 00. ::JULIAN'
            EXPCTD(29) = '1582 MAY 20 06.666 .# 00. ::JULIAN'
            EXPCTD(30) = '1582 JUNE 25 07.666 .# 00. ::JULIAN'
            EXPCTD(31) = '1582 JULY 01 08.666 .# 00. ::JULIAN'
            EXPCTD(32) = '1582 AUGUST 10 09.666 .# 00. ::JULIAN'
            EXPCTD(33) = '1582 SEPTEMBER 15 10.666 .# 00. ::JULIAN'
            EXPCTD(34) = '1582 OCTOBER 12 11.666 .# 00. ::JULIAN'
            EXPCTD(35) = '1582 OCTOBER 25 12.666 .# 00. ::JULIAN'
            EXPCTD(36) = '1582 DECEMBER 31 13.666 .# 00. ::JULIAN'
            EXPCTD(37) = '1986 JANUARY 01 02.666 .# 00. ::JULIAN'
            EXPCTD(38) = '1986 FEBRUARY 05 03.666 .# 00. ::JULIAN'
            EXPCTD(39) = '1986 MARCH 10 04.666 .# 00. ::JULIAN'
            EXPCTD(40) = '1986 APRIL 15 05.666 .# 00. ::JULIAN'
            EXPCTD(41) = '1986 MAY 20 06.666 .# 00. ::JULIAN'
            EXPCTD(42) = '1986 JUNE 25 07.666 .# 00. ::JULIAN'
            EXPCTD(43) = '1986 JULY 01 08.666 .# 00. ::JULIAN'
            EXPCTD(44) = '1986 AUGUST 10 09.666 .# 00. ::JULIAN'
            EXPCTD(45) = '1986 SEPTEMBER 15 10.666 .# 00. ::JULIAN'
            EXPCTD(46) = '1986 OCTOBER 20 11.666 .# 00. ::JULIAN'
            EXPCTD(47) = '1986 NOVEMBER 25 12.666 .# 00. ::JULIAN'
            EXPCTD(48) = '1986 DECEMBER 31 13.666 .# 00. ::JULIAN'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )

               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'YYYY MONTH DD HR:MN.SC.### #'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = ' -11 JANUARY 01 10:00.00.000 #'
            EXPCTD(2)  = ' -11 FEBRUARY 05 11:00.00.000 #'
            EXPCTD(3)  = ' -11 MARCH 10 12:00.00.000 #'
            EXPCTD(4)  = ' -11 APRIL 15 13:00.00.000 #'
            EXPCTD(5)  = ' -11 MAY 20 14:00.00.000 #'
            EXPCTD(6)  = ' -11 JUNE 25 15:00.00.000 #'
            EXPCTD(7)  = ' -11 JULY 01 16:00.00.000 #'
            EXPCTD(8)  = ' -11 AUGUST 10 17:00.00.000 #'
            EXPCTD(9)  = ' -11 SEPTEMBER 15 18:00.00.000 #'
            EXPCTD(10) = ' -11 OCTOBER 20 19:00.00.000 #'
            EXPCTD(11) = ' -11 NOVEMBER 25 20:00.00.000 #'
            EXPCTD(12) = ' -11 DECEMBER 31 21:00.00.000 #'
            EXPCTD(13) = ' 141 JANUARY 01 10:00.00.000 #'
            EXPCTD(14) = ' 141 FEBRUARY 05 11:00.00.000 #'
            EXPCTD(15) = ' 141 MARCH 10 12:00.00.000 #'
            EXPCTD(16) = ' 141 APRIL 15 13:00.00.000 #'
            EXPCTD(17) = ' 141 MAY 20 14:00.00.000 #'
            EXPCTD(18) = ' 141 JUNE 25 15:00.00.000 #'
            EXPCTD(19) = ' 141 JULY 01 16:00.00.000 #'
            EXPCTD(20) = ' 141 AUGUST 10 17:00.00.000 #'
            EXPCTD(21) = ' 141 SEPTEMBER 15 18:00.00.000 #'
            EXPCTD(22) = ' 141 OCTOBER 20 19:00.00.000 #'
            EXPCTD(23) = ' 141 NOVEMBER 25 20:00.00.000 #'
            EXPCTD(24) = ' 141 DECEMBER 31 21:00.00.000 #'
            EXPCTD(25) = '1582 JANUARY 01 10:00.00.000 #'
            EXPCTD(26) = '1582 FEBRUARY 05 11:00.00.000 #'
            EXPCTD(27) = '1582 MARCH 10 12:00.00.000 #'
            EXPCTD(28) = '1582 APRIL 15 13:00.00.000 #'
            EXPCTD(29) = '1582 MAY 20 14:00.00.000 #'
            EXPCTD(30) = '1582 JUNE 25 15:00.00.000 #'
            EXPCTD(31) = '1582 JULY 01 16:00.00.000 #'
            EXPCTD(32) = '1582 AUGUST 10 17:00.00.000 #'
            EXPCTD(33) = '1582 SEPTEMBER 15 18:00.00.000 #'
            EXPCTD(34) = '1582 OCTOBER 12 19:00.00.000 #'
            EXPCTD(35) = '1582 OCTOBER 25 20:00.00.000 #'
            EXPCTD(36) = '1582 DECEMBER 31 21:00.00.000 #'
            EXPCTD(37) = '1986 JANUARY 01 10:00.00.000 #'
            EXPCTD(38) = '1986 FEBRUARY 05 11:00.00.000 #'
            EXPCTD(39) = '1986 MARCH 10 12:00.00.000 #'
            EXPCTD(40) = '1986 APRIL 15 13:00.00.000 #'
            EXPCTD(41) = '1986 MAY 20 14:00.00.000 #'
            EXPCTD(42) = '1986 JUNE 25 15:00.00.000 #'
            EXPCTD(43) = '1986 JULY 01 16:00.00.000 #'
            EXPCTD(44) = '1986 AUGUST 10 17:00.00.000 #'
            EXPCTD(45) = '1986 SEPTEMBER 15 18:00.00.000 #'
            EXPCTD(46) = '1986 OCTOBER 20 19:00.00.000 #'
            EXPCTD(47) = '1986 NOVEMBER 25 20:00.00.000 #'
            EXPCTD(48) = '1986 DECEMBER 31 21:00.00.000 #'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )
               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'SP2000.####  ::TDB'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = '-6.346097996E+10'
            EXPCTD(2)  = '-6.345795236E+10'
            EXPCTD(3)  = '-6.345509756E+10'
            EXPCTD(4)  = '-6.345198356E+10'
            EXPCTD(5)  = '-6.344895596E+10'
            EXPCTD(6)  = '-6.344584196E+10'
            EXPCTD(7)  = '-6.344531996E+10'
            EXPCTD(8)  = '-6.344186036E+10'
            EXPCTD(9)  = '-6.343874636E+10'
            EXPCTD(10) = '-6.343571876E+10'
            EXPCTD(11) = '-6.343260476E+10'
            EXPCTD(12) = '-6.342949076E+10'
            EXPCTD(13) = '-5.866431116E+10'
            EXPCTD(14) = '-5.866128356E+10'
            EXPCTD(15) = '-5.865842876E+10'
            EXPCTD(16) = '-5.865531476E+10'
            EXPCTD(17) = '-5.865228716E+10'
            EXPCTD(18) = '-5.864917316E+10'
            EXPCTD(19) = '-5.864865116E+10'
            EXPCTD(20) = '-5.864519156E+10'
            EXPCTD(21) = '-5.864207756E+10'
            EXPCTD(22) = '-5.863904996E+10'
            EXPCTD(23) = '-5.863593596E+10'
            EXPCTD(24) = '-5.863282196E+10'
            EXPCTD(25) = '-1.319078156E+10'
            EXPCTD(26) = '-1.318775396E+10'
            EXPCTD(27) = '-1.318489916E+10'
            EXPCTD(28) = '-1.318178516E+10'
            EXPCTD(29) = '-1.317875756E+10'
            EXPCTD(30) = '-1.317564356E+10'
            EXPCTD(31) = '-1.317512156E+10'
            EXPCTD(32) = '-1.317166196E+10'
            EXPCTD(33) = '-1.316854796E+10'
            EXPCTD(34) = '-1.316621156E+10'
            EXPCTD(35) = '-1.316508476E+10'
            EXPCTD(36) = '-1.315929236E+10'
            EXPCTD(37) = ' -441770344.8161'
            EXPCTD(38) = ' -438742744.8151'
            EXPCTD(39) = ' -435887944.8145'
            EXPCTD(40) = ' -432773944.8144'
            EXPCTD(41) = ' -429746344.8149'
            EXPCTD(42) = ' -426632344.8158'
            EXPCTD(43) = ' -426110344.8160'
            EXPCTD(44) = ' -422650744.8170'
            EXPCTD(45) = ' -419536744.8176'
            EXPCTD(46) = ' -416509144.8176'
            EXPCTD(47) = ' -413395144.8171'
            EXPCTD(48) = ' -410281144.8161'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )
               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'SP1950.####'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = '-6.188310000E+10'
            EXPCTD(2)  = '-6.188007240E+10'
            EXPCTD(3)  = '-6.187721760E+10'
            EXPCTD(4)  = '-6.187410360E+10'
            EXPCTD(5)  = '-6.187107600E+10'
            EXPCTD(6)  = '-6.186796200E+10'
            EXPCTD(7)  = '-6.186744000E+10'
            EXPCTD(8)  = '-6.186398040E+10'
            EXPCTD(9)  = '-6.186086640E+10'
            EXPCTD(10) = '-6.185783880E+10'
            EXPCTD(11) = '-6.185472480E+10'
            EXPCTD(12) = '-6.185161080E+10'
            EXPCTD(13) = '-5.708643120E+10'
            EXPCTD(14) = '-5.708340360E+10'
            EXPCTD(15) = '-5.708054880E+10'
            EXPCTD(16) = '-5.707743480E+10'
            EXPCTD(17) = '-5.707440720E+10'
            EXPCTD(18) = '-5.707129320E+10'
            EXPCTD(19) = '-5.707077120E+10'
            EXPCTD(20) = '-5.706731160E+10'
            EXPCTD(21) = '-5.706419760E+10'
            EXPCTD(22) = '-5.706117000E+10'
            EXPCTD(23) = '-5.705805600E+10'
            EXPCTD(24) = '-5.705494200E+10'
            EXPCTD(25) = '-1.161290160E+10'
            EXPCTD(26) = '-1.160987400E+10'
            EXPCTD(27) = '-1.160701920E+10'
            EXPCTD(28) = '-1.160390520E+10'
            EXPCTD(29) = '-1.160087760E+10'
            EXPCTD(30) = '-1.159776360E+10'
            EXPCTD(31) = '-1.159724160E+10'
            EXPCTD(32) = '-1.159378200E+10'
            EXPCTD(33) = '-1.159066800E+10'
            EXPCTD(34) = '-1.158833160E+10'
            EXPCTD(35) = '-1.158720480E+10'
            EXPCTD(36) = '-1.158141240E+10'
            EXPCTD(37) = ' 1136109600.0000'
            EXPCTD(38) = ' 1139137200.0000'
            EXPCTD(39) = ' 1141992000.0000'
            EXPCTD(40) = ' 1145106000.0000'
            EXPCTD(41) = ' 1148133600.0000'
            EXPCTD(42) = ' 1151247600.0000'
            EXPCTD(43) = ' 1151769600.0000'
            EXPCTD(44) = ' 1155229200.0000'
            EXPCTD(45) = ' 1158343200.0000'
            EXPCTD(46) = ' 1161370800.0000'
            EXPCTD(47) = ' 1164484800.0000'
            EXPCTD(48) = ' 1167598800.0000'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )
               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'YYYY MONTH DD HR:MN:SC ::JCAL'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = ' -11 JANUARY 03 10:00:00'
            EXPCTD(2)  = ' -11 FEBRUARY 07 11:00:00'
            EXPCTD(3)  = ' -11 MARCH 12 12:00:00'
            EXPCTD(4)  = ' -11 APRIL 17 13:00:00'
            EXPCTD(5)  = ' -11 MAY 22 14:00:00'
            EXPCTD(6)  = ' -11 JUNE 27 15:00:00'
            EXPCTD(7)  = ' -11 JULY 03 16:00:00'
            EXPCTD(8)  = ' -11 AUGUST 12 17:00:00'
            EXPCTD(9)  = ' -11 SEPTEMBER 17 18:00:00'
            EXPCTD(10) = ' -11 OCTOBER 22 19:00:00'
            EXPCTD(11) = ' -11 NOVEMBER 27 20:00:00'
            EXPCTD(12) = ' -10 JANUARY 02 21:00:00'
            EXPCTD(13) = ' 141 JANUARY 02 10:00:00'
            EXPCTD(14) = ' 141 FEBRUARY 06 11:00:00'
            EXPCTD(15) = ' 141 MARCH 11 12:00:00'
            EXPCTD(16) = ' 141 APRIL 16 13:00:00'
            EXPCTD(17) = ' 141 MAY 21 14:00:00'
            EXPCTD(18) = ' 141 JUNE 26 15:00:00'
            EXPCTD(19) = ' 141 JULY 02 16:00:00'
            EXPCTD(20) = ' 141 AUGUST 11 17:00:00'
            EXPCTD(21) = ' 141 SEPTEMBER 16 18:00:00'
            EXPCTD(22) = ' 141 OCTOBER 21 19:00:00'
            EXPCTD(23) = ' 141 NOVEMBER 26 20:00:00'
            EXPCTD(24) = ' 142 JANUARY 01 21:00:00'
            EXPCTD(25) = '1581 DECEMBER 22 10:00:00'
            EXPCTD(26) = '1582 JANUARY 26 11:00:00'
            EXPCTD(27) = '1582 FEBRUARY 28 12:00:00'
            EXPCTD(28) = '1582 APRIL 05 13:00:00'
            EXPCTD(29) = '1582 MAY 10 14:00:00'
            EXPCTD(30) = '1582 JUNE 15 15:00:00'
            EXPCTD(31) = '1582 JUNE 21 16:00:00'
            EXPCTD(32) = '1582 JULY 31 17:00:00'
            EXPCTD(33) = '1582 SEPTEMBER 05 18:00:00'
            EXPCTD(34) = '1582 OCTOBER 02 19:00:00'
            EXPCTD(35) = '1582 OCTOBER 15 20:00:00'
            EXPCTD(36) = '1582 DECEMBER 21 21:00:00'
            EXPCTD(37) = '1985 DECEMBER 19 10:00:00'
            EXPCTD(38) = '1986 JANUARY 23 11:00:00'
            EXPCTD(39) = '1986 FEBRUARY 25 12:00:00'
            EXPCTD(40) = '1986 APRIL 02 13:00:00'
            EXPCTD(41) = '1986 MAY 07 14:00:00'
            EXPCTD(42) = '1986 JUNE 12 15:00:00'
            EXPCTD(43) = '1986 JUNE 18 16:00:00'
            EXPCTD(44) = '1986 JULY 28 17:00:00'
            EXPCTD(45) = '1986 SEPTEMBER 02 18:00:00'
            EXPCTD(46) = '1986 OCTOBER 07 19:00:00'
            EXPCTD(47) = '1986 NOVEMBER 12 20:00:00'
            EXPCTD(48) = '1986 DECEMBER 18 21:00:00'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )
               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'JULIAND.#####'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = '1717042.91666'
            EXPCTD(2)  = '1717077.95833'
            EXPCTD(3)  = '1717111.00000'
            EXPCTD(4)  = '1717147.04166'
            EXPCTD(5)  = '1717182.08333'
            EXPCTD(6)  = '1717218.12500'
            EXPCTD(7)  = '1717224.16666'
            EXPCTD(8)  = '1717264.20833'
            EXPCTD(9)  = '1717300.25000'
            EXPCTD(10) = '1717335.29166'
            EXPCTD(11) = '1717371.33333'
            EXPCTD(12) = '1717407.37500'
            EXPCTD(13) = '1772559.91666'
            EXPCTD(14) = '1772594.95833'
            EXPCTD(15) = '1772628.00000'
            EXPCTD(16) = '1772664.04166'
            EXPCTD(17) = '1772699.08333'
            EXPCTD(18) = '1772735.12500'
            EXPCTD(19) = '1772741.16666'
            EXPCTD(20) = '1772781.20833'
            EXPCTD(21) = '1772817.25000'
            EXPCTD(22) = '1772852.29166'
            EXPCTD(23) = '1772888.33333'
            EXPCTD(24) = '1772924.37500'
            EXPCTD(25) = '2298873.91666'
            EXPCTD(26) = '2298908.95833'
            EXPCTD(27) = '2298942.00000'
            EXPCTD(28) = '2298978.04166'
            EXPCTD(29) = '2299013.08333'
            EXPCTD(30) = '2299049.12500'
            EXPCTD(31) = '2299055.16666'
            EXPCTD(32) = '2299095.20833'
            EXPCTD(33) = '2299131.25000'
            EXPCTD(34) = '2299158.29166'
            EXPCTD(35) = '2299171.33333'
            EXPCTD(36) = '2299238.37500'
            EXPCTD(37) = '2446431.91666'
            EXPCTD(38) = '2446466.95833'
            EXPCTD(39) = '2446500.00000'
            EXPCTD(40) = '2446536.04166'
            EXPCTD(41) = '2446571.08333'
            EXPCTD(42) = '2446607.12500'
            EXPCTD(43) = '2446613.16666'
            EXPCTD(44) = '2446653.20833'
            EXPCTD(45) = '2446689.25000'
            EXPCTD(46) = '2446724.29166'
            EXPCTD(47) = '2446760.33333'
            EXPCTD(48) = '2446796.37500'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )
               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'YYYY Month DD HR:MN:SC Weekday'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = ' -11 January 01 10:00:00 Sunday'
            EXPCTD(2)  = ' -11 February 05 11:00:00 Sunday'
            EXPCTD(3)  = ' -11 March 10 12:00:00 Friday'
            EXPCTD(4)  = ' -11 April 15 13:00:00 Saturday'
            EXPCTD(5)  = ' -11 May 20 14:00:00 Saturday'
            EXPCTD(6)  = ' -11 June 25 15:00:00 Sunday'
            EXPCTD(7)  = ' -11 July 01 16:00:00 Saturday'
            EXPCTD(8)  = ' -11 August 10 17:00:00 Thursday'
            EXPCTD(9)  = ' -11 September 15 18:00:00 Friday'
            EXPCTD(10) = ' -11 October 20 19:00:00 Friday'
            EXPCTD(11) = ' -11 November 25 20:00:00 Saturday'
            EXPCTD(12) = ' -11 December 31 21:00:00 Sunday'
            EXPCTD(13) = ' 141 January 01 10:00:00 Sunday'
            EXPCTD(14) = ' 141 February 05 11:00:00 Sunday'
            EXPCTD(15) = ' 141 March 10 12:00:00 Friday'
            EXPCTD(16) = ' 141 April 15 13:00:00 Saturday'
            EXPCTD(17) = ' 141 May 20 14:00:00 Saturday'
            EXPCTD(18) = ' 141 June 25 15:00:00 Sunday'
            EXPCTD(19) = ' 141 July 01 16:00:00 Saturday'
            EXPCTD(20) = ' 141 August 10 17:00:00 Thursday'
            EXPCTD(21) = ' 141 September 15 18:00:00 Friday'
            EXPCTD(22) = ' 141 October 20 19:00:00 Friday'
            EXPCTD(23) = ' 141 November 25 20:00:00 Saturday'
            EXPCTD(24) = ' 141 December 31 21:00:00 Sunday'
            EXPCTD(25) = '1582 January 01 10:00:00 Friday'
            EXPCTD(26) = '1582 February 05 11:00:00 Friday'
            EXPCTD(27) = '1582 March 10 12:00:00 Wednesday'
            EXPCTD(28) = '1582 April 15 13:00:00 Thursday'
            EXPCTD(29) = '1582 May 20 14:00:00 Thursday'
            EXPCTD(30) = '1582 June 25 15:00:00 Friday'
            EXPCTD(31) = '1582 July 01 16:00:00 Thursday'
            EXPCTD(32) = '1582 August 10 17:00:00 Tuesday'
            EXPCTD(33) = '1582 September 15 18:00:00 Wednesday'
            EXPCTD(34) = '1582 October 12 19:00:00 Tuesday'
            EXPCTD(35) = '1582 October 25 20:00:00 Monday'
            EXPCTD(36) = '1582 December 31 21:00:00 Friday'
            EXPCTD(37) = '1986 January 01 10:00:00 Wednesday'
            EXPCTD(38) = '1986 February 05 11:00:00 Wednesday'
            EXPCTD(39) = '1986 March 10 12:00:00 Monday'
            EXPCTD(40) = '1986 April 15 13:00:00 Tuesday'
            EXPCTD(41) = '1986 May 20 14:00:00 Tuesday'
            EXPCTD(42) = '1986 June 25 15:00:00 Wednesday'
            EXPCTD(43) = '1986 July 01 16:00:00 Tuesday'
            EXPCTD(44) = '1986 August 10 17:00:00 Sunday'
            EXPCTD(45) = '1986 September 15 18:00:00 Monday'
            EXPCTD(46) = '1986 October 20 19:00:00 Monday'
            EXPCTD(47) = '1986 November 25 20:00:00 Tuesday'
            EXPCTD(48) = '1986 December 31 21:00:00 Wednesday'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )
               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'YYYY ERA Month DD HR:MN:SC.###'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = '  12 B.C. January 01 10:00:00.000'
            EXPCTD(2)  = '  12 B.C. February 05 11:00:00.000'
            EXPCTD(3)  = '  12 B.C. March 10 12:00:00.000'
            EXPCTD(4)  = '  12 B.C. April 15 13:00:00.000'
            EXPCTD(5)  = '  12 B.C. May 20 14:00:00.000'
            EXPCTD(6)  = '  12 B.C. June 25 15:00:00.000'
            EXPCTD(7)  = '  12 B.C. July 01 16:00:00.000'
            EXPCTD(8)  = '  12 B.C. August 10 17:00:00.000'
            EXPCTD(9)  = '  12 B.C. September 15 18:00:00.000'
            EXPCTD(10) = '  12 B.C. October 20 19:00:00.000'
            EXPCTD(11) = '  12 B.C. November 25 20:00:00.000'
            EXPCTD(12) = '  12 B.C. December 31 21:00:00.000'
            EXPCTD(13) = ' 141 A.D. January 01 10:00:00.000'
            EXPCTD(14) = ' 141 A.D. February 05 11:00:00.000'
            EXPCTD(15) = ' 141 A.D. March 10 12:00:00.000'
            EXPCTD(16) = ' 141 A.D. April 15 13:00:00.000'
            EXPCTD(17) = ' 141 A.D. May 20 14:00:00.000'
            EXPCTD(18) = ' 141 A.D. June 25 15:00:00.000'
            EXPCTD(19) = ' 141 A.D. July 01 16:00:00.000'
            EXPCTD(20) = ' 141 A.D. August 10 17:00:00.000'
            EXPCTD(21) = ' 141 A.D. September 15 18:00:00.000'
            EXPCTD(22) = ' 141 A.D. October 20 19:00:00.000'
            EXPCTD(23) = ' 141 A.D. November 25 20:00:00.000'
            EXPCTD(24) = ' 141 A.D. December 31 21:00:00.000'
            EXPCTD(25) = '1582 A.D. January 01 10:00:00.000'
            EXPCTD(26) = '1582 A.D. February 05 11:00:00.000'
            EXPCTD(27) = '1582 A.D. March 10 12:00:00.000'
            EXPCTD(28) = '1582 A.D. April 15 13:00:00.000'
            EXPCTD(29) = '1582 A.D. May 20 14:00:00.000'
            EXPCTD(30) = '1582 A.D. June 25 15:00:00.000'
            EXPCTD(31) = '1582 A.D. July 01 16:00:00.000'
            EXPCTD(32) = '1582 A.D. August 10 17:00:00.000'
            EXPCTD(33) = '1582 A.D. September 15 18:00:00.000'
            EXPCTD(34) = '1582 A.D. October 12 19:00:00.000'
            EXPCTD(35) = '1582 A.D. October 25 20:00:00.000'
            EXPCTD(36) = '1582 A.D. December 31 21:00:00.000'
            EXPCTD(37) = '1986 A.D. January 01 10:00:00.000'
            EXPCTD(38) = '1986 A.D. February 05 11:00:00.000'
            EXPCTD(39) = '1986 A.D. March 10 12:00:00.000'
            EXPCTD(40) = '1986 A.D. April 15 13:00:00.000'
            EXPCTD(41) = '1986 A.D. May 20 14:00:00.000'
            EXPCTD(42) = '1986 A.D. June 25 15:00:00.000'
            EXPCTD(43) = '1986 A.D. July 01 16:00:00.000'
            EXPCTD(44) = '1986 A.D. August 10 17:00:00.000'
            EXPCTD(45) = '1986 A.D. September 15 18:00:00.000'
            EXPCTD(46) = '1986 A.D. October 20 19:00:00.000'
            EXPCTD(47) = '1986 A.D. November 25 20:00:00.000'
            EXPCTD(48) = '1986 A.D. December 31 21:00:00.000'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )
               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'YYYY ERA MM/DD HR:MN ::TRNC'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = '  12 B.C. 01/01 10:00'
            EXPCTD(2)  = '  12 B.C. 02/05 11:00'
            EXPCTD(3)  = '  12 B.C. 03/10 12:00'
            EXPCTD(4)  = '  12 B.C. 04/15 13:00'
            EXPCTD(5)  = '  12 B.C. 05/20 14:00'
            EXPCTD(6)  = '  12 B.C. 06/25 15:00'
            EXPCTD(7)  = '  12 B.C. 07/01 16:00'
            EXPCTD(8)  = '  12 B.C. 08/10 17:00'
            EXPCTD(9)  = '  12 B.C. 09/15 18:00'
            EXPCTD(10) = '  12 B.C. 10/20 19:00'
            EXPCTD(11) = '  12 B.C. 11/25 20:00'
            EXPCTD(12) = '  12 B.C. 12/31 21:00'
            EXPCTD(13) = ' 141 A.D. 01/01 10:00'
            EXPCTD(14) = ' 141 A.D. 02/05 11:00'
            EXPCTD(15) = ' 141 A.D. 03/10 12:00'
            EXPCTD(16) = ' 141 A.D. 04/15 13:00'
            EXPCTD(17) = ' 141 A.D. 05/20 14:00'
            EXPCTD(18) = ' 141 A.D. 06/25 15:00'
            EXPCTD(19) = ' 141 A.D. 07/01 16:00'
            EXPCTD(20) = ' 141 A.D. 08/10 17:00'
            EXPCTD(21) = ' 141 A.D. 09/15 18:00'
            EXPCTD(22) = ' 141 A.D. 10/20 19:00'
            EXPCTD(23) = ' 141 A.D. 11/25 20:00'
            EXPCTD(24) = ' 141 A.D. 12/31 21:00'
            EXPCTD(25) = '1582 A.D. 01/01 10:00'
            EXPCTD(26) = '1582 A.D. 02/05 11:00'
            EXPCTD(27) = '1582 A.D. 03/10 12:00'
            EXPCTD(28) = '1582 A.D. 04/15 13:00'
            EXPCTD(29) = '1582 A.D. 05/20 14:00'
            EXPCTD(30) = '1582 A.D. 06/25 15:00'
            EXPCTD(31) = '1582 A.D. 07/01 16:00'
            EXPCTD(32) = '1582 A.D. 08/10 17:00'
            EXPCTD(33) = '1582 A.D. 09/15 18:00'
            EXPCTD(34) = '1582 A.D. 10/12 19:00'
            EXPCTD(35) = '1582 A.D. 10/25 20:00'
            EXPCTD(36) = '1582 A.D. 12/31 21:00'
            EXPCTD(37) = '1986 A.D. 01/01 10:00'
            EXPCTD(38) = '1986 A.D. 02/05 11:00'
            EXPCTD(39) = '1986 A.D. 03/10 12:00'
            EXPCTD(40) = '1986 A.D. 04/15 13:00'
            EXPCTD(41) = '1986 A.D. 05/20 14:00'
            EXPCTD(42) = '1986 A.D. 06/25 15:00'
            EXPCTD(43) = '1986 A.D. 07/01 16:00'
            EXPCTD(44) = '1986 A.D. 08/10 17:00'
            EXPCTD(45) = '1986 A.D. 09/15 18:00'
            EXPCTD(46) = '1986 A.D. 10/20 19:00'
            EXPCTD(47) = '1986 A.D. 11/25 20:00'
            EXPCTD(48) = '1986 A.D. 12/31 21:00'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )
               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'YYYY ERA MONTH DD WEEKDAY'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = '  12 B.C. JANUARY 01 SUNDAY'
            EXPCTD(2)  = '  12 B.C. FEBRUARY 05 SUNDAY'
            EXPCTD(3)  = '  12 B.C. MARCH 10 FRIDAY'
            EXPCTD(4)  = '  12 B.C. APRIL 15 SATURDAY'
            EXPCTD(5)  = '  12 B.C. MAY 20 SATURDAY'
            EXPCTD(6)  = '  12 B.C. JUNE 25 SUNDAY'
            EXPCTD(7)  = '  12 B.C. JULY 01 SATURDAY'
            EXPCTD(8)  = '  12 B.C. AUGUST 10 THURSDAY'
            EXPCTD(9)  = '  12 B.C. SEPTEMBER 15 FRIDAY'
            EXPCTD(10) = '  12 B.C. OCTOBER 20 FRIDAY'
            EXPCTD(11) = '  12 B.C. NOVEMBER 25 SATURDAY'
            EXPCTD(12) = '  12 B.C. DECEMBER 31 SUNDAY'
            EXPCTD(13) = ' 141 A.D. JANUARY 01 SUNDAY'
            EXPCTD(14) = ' 141 A.D. FEBRUARY 05 SUNDAY'
            EXPCTD(15) = ' 141 A.D. MARCH 10 FRIDAY'
            EXPCTD(16) = ' 141 A.D. APRIL 15 SATURDAY'
            EXPCTD(17) = ' 141 A.D. MAY 20 SATURDAY'
            EXPCTD(18) = ' 141 A.D. JUNE 25 SUNDAY'
            EXPCTD(19) = ' 141 A.D. JULY 01 SATURDAY'
            EXPCTD(20) = ' 141 A.D. AUGUST 10 THURSDAY'
            EXPCTD(21) = ' 141 A.D. SEPTEMBER 15 FRIDAY'
            EXPCTD(22) = ' 141 A.D. OCTOBER 20 FRIDAY'
            EXPCTD(23) = ' 141 A.D. NOVEMBER 25 SATURDAY'
            EXPCTD(24) = ' 141 A.D. DECEMBER 31 SUNDAY'
            EXPCTD(25) = '1582 A.D. JANUARY 01 FRIDAY'
            EXPCTD(26) = '1582 A.D. FEBRUARY 05 FRIDAY'
            EXPCTD(27) = '1582 A.D. MARCH 10 WEDNESDAY'
            EXPCTD(28) = '1582 A.D. APRIL 15 THURSDAY'
            EXPCTD(29) = '1582 A.D. MAY 20 THURSDAY'
            EXPCTD(30) = '1582 A.D. JUNE 25 FRIDAY'
            EXPCTD(31) = '1582 A.D. JULY 01 THURSDAY'
            EXPCTD(32) = '1582 A.D. AUGUST 10 TUESDAY'
            EXPCTD(33) = '1582 A.D. SEPTEMBER 15 WEDNESDAY'
            EXPCTD(34) = '1582 A.D. OCTOBER 12 TUESDAY'
            EXPCTD(35) = '1582 A.D. OCTOBER 25 MONDAY'
            EXPCTD(36) = '1582 A.D. DECEMBER 31 FRIDAY'
            EXPCTD(37) = '1986 A.D. JANUARY 01 WEDNESDAY'
            EXPCTD(38) = '1986 A.D. FEBRUARY 05 WEDNESDAY'
            EXPCTD(39) = '1986 A.D. MARCH 10 MONDAY'
            EXPCTD(40) = '1986 A.D. APRIL 15 TUESDAY'
            EXPCTD(41) = '1986 A.D. MAY 20 TUESDAY'
            EXPCTD(42) = '1986 A.D. JUNE 25 WEDNESDAY'
            EXPCTD(43) = '1986 A.D. JULY 01 TUESDAY'
            EXPCTD(44) = '1986 A.D. AUGUST 10 SUNDAY'
            EXPCTD(45) = '1986 A.D. SEPTEMBER 15 MONDAY'
            EXPCTD(46) = '1986 A.D. OCTOBER 20 MONDAY'
            EXPCTD(47) = '1986 A.D. NOVEMBER 25 TUESDAY'
            EXPCTD(48) = '1986 A.D. DECEMBER 31 WEDNESDAY'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )
               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'YYYY era MON DD'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = '  12 b.c. JAN 01'
            EXPCTD(2)  = '  12 b.c. FEB 05'
            EXPCTD(3)  = '  12 b.c. MAR 10'
            EXPCTD(4)  = '  12 b.c. APR 15'
            EXPCTD(5)  = '  12 b.c. MAY 20'
            EXPCTD(6)  = '  12 b.c. JUN 25'
            EXPCTD(7)  = '  12 b.c. JUL 01'
            EXPCTD(8)  = '  12 b.c. AUG 10'
            EXPCTD(9)  = '  12 b.c. SEP 15'
            EXPCTD(10) = '  12 b.c. OCT 20'
            EXPCTD(11) = '  12 b.c. NOV 25'
            EXPCTD(12) = '  12 b.c. DEC 31'
            EXPCTD(13) = ' 141 a.d. JAN 01'
            EXPCTD(14) = ' 141 a.d. FEB 05'
            EXPCTD(15) = ' 141 a.d. MAR 10'
            EXPCTD(16) = ' 141 a.d. APR 15'
            EXPCTD(17) = ' 141 a.d. MAY 20'
            EXPCTD(18) = ' 141 a.d. JUN 25'
            EXPCTD(19) = ' 141 a.d. JUL 01'
            EXPCTD(20) = ' 141 a.d. AUG 10'
            EXPCTD(21) = ' 141 a.d. SEP 15'
            EXPCTD(22) = ' 141 a.d. OCT 20'
            EXPCTD(23) = ' 141 a.d. NOV 25'
            EXPCTD(24) = ' 141 a.d. DEC 31'
            EXPCTD(25) = '1582 a.d. JAN 01'
            EXPCTD(26) = '1582 a.d. FEB 05'
            EXPCTD(27) = '1582 a.d. MAR 10'
            EXPCTD(28) = '1582 a.d. APR 15'
            EXPCTD(29) = '1582 a.d. MAY 20'
            EXPCTD(30) = '1582 a.d. JUN 25'
            EXPCTD(31) = '1582 a.d. JUL 01'
            EXPCTD(32) = '1582 a.d. AUG 10'
            EXPCTD(33) = '1582 a.d. SEP 15'
            EXPCTD(34) = '1582 a.d. OCT 12'
            EXPCTD(35) = '1582 a.d. OCT 25'
            EXPCTD(36) = '1582 a.d. DEC 31'
            EXPCTD(37) = '1986 a.d. JAN 01'
            EXPCTD(38) = '1986 a.d. FEB 05'
            EXPCTD(39) = '1986 a.d. MAR 10'
            EXPCTD(40) = '1986 a.d. APR 15'
            EXPCTD(41) = '1986 a.d. MAY 20'
            EXPCTD(42) = '1986 a.d. JUN 25'
            EXPCTD(43) = '1986 a.d. JUL 01'
            EXPCTD(44) = '1986 a.d. AUG 10'
            EXPCTD(45) = '1986 a.d. SEP 15'
            EXPCTD(46) = '1986 a.d. OCT 20'
            EXPCTD(47) = '1986 a.d. NOV 25'
            EXPCTD(48) = '1986 a.d. DEC 31'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )
               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'JULIAND.####### ::TDB'

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = '1717042.9171433'
            EXPCTD(2)  = '1717077.9588100'
            EXPCTD(3)  = '1717111.0004766'
            EXPCTD(4)  = '1717147.0421433'
            EXPCTD(5)  = '1717182.0838100'
            EXPCTD(6)  = '1717218.1254766'
            EXPCTD(7)  = '1717224.1671433'
            EXPCTD(8)  = '1717264.2088099'
            EXPCTD(9)  = '1717300.2504766'
            EXPCTD(10) = '1717335.2921433'
            EXPCTD(11) = '1717371.3338099'
            EXPCTD(12) = '1717407.3754766'
            EXPCTD(13) = '1772559.9171433'
            EXPCTD(14) = '1772594.9588100'
            EXPCTD(15) = '1772628.0004766'
            EXPCTD(16) = '1772664.0421433'
            EXPCTD(17) = '1772699.0838100'
            EXPCTD(18) = '1772735.1254766'
            EXPCTD(19) = '1772741.1671433'
            EXPCTD(20) = '1772781.2088099'
            EXPCTD(21) = '1772817.2504766'
            EXPCTD(22) = '1772852.2921433'
            EXPCTD(23) = '1772888.3338099'
            EXPCTD(24) = '1772924.3754766'
            EXPCTD(25) = '2298873.9171433'
            EXPCTD(26) = '2298908.9588100'
            EXPCTD(27) = '2298942.0004766'
            EXPCTD(28) = '2298978.0421433'
            EXPCTD(29) = '2299013.0838100'
            EXPCTD(30) = '2299049.1254766'
            EXPCTD(31) = '2299055.1671433'
            EXPCTD(32) = '2299095.2088099'
            EXPCTD(33) = '2299131.2504766'
            EXPCTD(34) = '2299158.2921433'
            EXPCTD(35) = '2299171.3338099'
            EXPCTD(36) = '2299238.3754766'
            EXPCTD(37) = '2446431.9173053'
            EXPCTD(38) = '2446466.9589720'
            EXPCTD(39) = '2446500.0006387'
            EXPCTD(40) = '2446536.0423053'
            EXPCTD(41) = '2446571.0839720'
            EXPCTD(42) = '2446607.1256387'
            EXPCTD(43) = '2446613.1673053'
            EXPCTD(44) = '2446653.2089720'
            EXPCTD(45) = '2446689.2506386'
            EXPCTD(46) = '2446724.2923053'
            EXPCTD(47) = '2446760.3339720'
            EXPCTD(48) = '2446796.3756387'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR, FTIME )
               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )
            END DO


      PICTUR = 'JULIAND.####### '

      CALL TCASE ( PICTUR )

            CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
            CALL TIMDEF ( 'SET', 'SYSTEM', 'UTC' )

            EXPCTD(1)  = '1717042.9171433'
            EXPCTD(2)  = '1717077.9588099'
            EXPCTD(3)  = '1717111.0004766'
            EXPCTD(4)  = '1717147.0421433'
            EXPCTD(5)  = '1717182.0838099'
            EXPCTD(6)  = '1717218.1254766'
            EXPCTD(7)  = '1717224.1671433'
            EXPCTD(8)  = '1717264.2088099'
            EXPCTD(9)  = '1717300.2504766'
            EXPCTD(10) = '1717335.2921433'
            EXPCTD(11) = '1717371.3338099'
            EXPCTD(12) = '1717407.3754766'
            EXPCTD(13) = '1772559.9171433'
            EXPCTD(14) = '1772594.9588099'
            EXPCTD(15) = '1772628.0004766'
            EXPCTD(16) = '1772664.0421433'
            EXPCTD(17) = '1772699.0838099'
            EXPCTD(18) = '1772735.1254766'
            EXPCTD(19) = '1772741.1671433'
            EXPCTD(20) = '1772781.2088099'
            EXPCTD(21) = '1772817.2504766'
            EXPCTD(22) = '1772852.2921433'
            EXPCTD(23) = '1772888.3338099'
            EXPCTD(24) = '1772924.3754766'
            EXPCTD(25) = '2298873.9171433'
            EXPCTD(26) = '2298908.9588100'
            EXPCTD(27) = '2298942.0004766'
            EXPCTD(28) = '2298978.0421433'
            EXPCTD(29) = '2299013.0838100'
            EXPCTD(30) = '2299049.1254766'
            EXPCTD(31) = '2299055.1671433'
            EXPCTD(32) = '2299095.2088100'
            EXPCTD(33) = '2299131.2504766'
            EXPCTD(34) = '2299158.2921433'
            EXPCTD(35) = '2299171.3338100'
            EXPCTD(36) = '2299238.3754766'
            EXPCTD(37) = '2446431.9173053'
            EXPCTD(38) = '2446466.9589720'
            EXPCTD(39) = '2446500.0006387'
            EXPCTD(40) = '2446536.0423053'
            EXPCTD(41) = '2446571.0839720'
            EXPCTD(42) = '2446607.1256387'
            EXPCTD(43) = '2446613.1673053'
            EXPCTD(44) = '2446653.2089720'
            EXPCTD(45) = '2446689.2506387'
            EXPCTD(46) = '2446724.2923053'
            EXPCTD(47) = '2446760.3339720'
            EXPCTD(48) = '2446796.3756387'

            DO I = 1, 48
               CALL STR2ET ( TIMES(I),   ET    )
               CALL TIMOUT ( ET, PICTUR//'::TDT', FTIME )

               NAME = 'FTIME(#)'
               CALL REPMI ( NAME, '#', I, NAME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )

               CALL TIMOUT ( ET, PICTUR//'::TT', FTIME )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSC ( NAME, FTIME, '=', EXPCTD(I), OK )

            END DO


!
!     All 'DO Loops' have been executed.
!     All 'IF branches' have been executed.
!
!
!     Clean up after ourselves.  Clear the kernel pool.
!
      CALL CLPOOL
      CALL T_SUCCESS ( OK )
      RETURN
      END
