!$Procedure      F_TCHECK ( Test the routine TCHECK )

      SUBROUTINE F_TCHECK ( OK )

! $ Abstract
!
!     Test the routine TCHECK and its associated entry points.
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
!     This routine tests the SPICELIB routine TCHECK.
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
!-    TSPICE Version 1.2.0, 14-OCT-2017 (EDW)
!
!        Explicitly initialize MODIFY to ' ' to prevent possible
!        uninitialized evaluation event.
!
!-    TSPICE Version 1.1.0, 31-JAN-2017 (NJB)
!
!        Added test to make sure B.C. leap years are accepted.
!
!-    TSPICE Version 1.0.0, 22-FEB-2008 (NJB) (WLT)
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
      INTEGER               ERA
      PARAMETER           ( ERA    =  1 )

      INTEGER               WDAY
      PARAMETER           ( WDAY   = ERA    + 1 )

      INTEGER               ZONE
      PARAMETER           ( ZONE   = WDAY   + 1 )

      INTEGER               AMPM
      PARAMETER           ( AMPM   = ZONE   + 1 )

      INTEGER               SYSTEM
      PARAMETER           ( SYSTEM = AMPM   + 1 )

      INTEGER               NMODS
      PARAMETER           ( NMODS = SYSTEM )

      CHARACTER*(3)         YESNO

      INTEGER               YEAR
      INTEGER               MON
      INTEGER               DAY
      INTEGER               HR
      INTEGER               MIN
      INTEGER               SEC

      INTEGER               I

      DOUBLE PRECISION      TVEC ( 6 )

      CHARACTER*(3)         TYPE
      LOGICAL               MODS
      CHARACTER*(5)         MODIFY ( NMODS )
      LOGICAL               PASS
      CHARACTER*(240)       ERROR
      CHARACTER*(240)       MESSGE

!
!     Explicitly set the MODIFY array before use.
!
      DO I = 1, NMODS
         MODIFY(I) = ' '
      END DO

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_TCHECK' )

      CALL TCASE ( 'Make sure we can set and get the checking ' &
      //           'status. ' )

         CALL TPARCH ( 'NO'  )
         CALL TCHCKD ( YESNO )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSC ( 'YESNO', YESNO, '=', 'NO',  OK )

         CALL TPARCH ( 'yes' )
         CALL TCHCKD ( YESNO )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSC ( 'YESNO', YESNO, '=', 'YES',  OK )

         CALL TPARCH ( 'NO'  )
         CALL TCHCKD ( YESNO )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSC ( 'YESNO', YESNO, '=', 'NO',  OK )

      CALL TCASE ( 'With checking turned off make sure that no ' &
      //           'checks are performed. ' )

         TVEC(1) = 1993.0D9
         TVEC(2) =   23.0D0
         TVEC(3) = 1024.0D0
         TVEC(4) =   -2.0D0
         TVEC(5) =  127.0D0
         TVEC(6) =  -12.0D0

         MODS    = .FALSE.

         CALL TCHECK ( TVEC, 'YMD', MODS, MODIFY, PASS, ERROR )

         CALL CHCKSL ( 'PASS',  PASS,       .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',     OK )


      CALL TCASE ( 'Checking on:  JAN -1, 1995 12:13:29' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    1.0D0
         TVEC(3) =   -1.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   29.0D0

         MODS    = .FALSE.

         CALL TCHECK ( TVEC, 'YMD',    MODS, MODIFY, PASS, ERROR )
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  JAN 32, 1995 12:13:29' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    1.0D0
         TVEC(3) =   32.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   29.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  Feb 30, 1996 12:13:29' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1996.0D0
         TVEC(2) =    2.0D0
         TVEC(3) =   30.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   29.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  Feb 29, 1995 12:13:29' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    2.0D0
         TVEC(3) =   29.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   29.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )

      CALL TCASE ( 'Checking on:  Mar 32, 1995 12:13:29' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    3.0D0
         TVEC(3) =   32.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   29.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  Apr 31, 1995 12:13:29' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    4.0D0
         TVEC(3) =   31.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   29.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  May 32, 1995 12:13:29' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    5.0D0
         TVEC(3) =   32.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   29.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  Jun 31, 1995 12:13:29' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    6.0D0
         TVEC(3) =   31.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   29.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  Jul 32, 1995 12:13:29' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    7.0D0
         TVEC(3) =   32.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   29.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  Aug 32, 1995 12:13:29' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    8.0D0
         TVEC(3) =   32.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   29.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )

      CALL TCASE ( 'Checking on:  Sep 31, 1995 12:13:29' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    9.0D0
         TVEC(3) =   31.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   29.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  Oct 32, 1995 12:13:29' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =   10.0D0
         TVEC(3) =   32.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   29.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  Nov 31, 1995 12:13:29' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =   11.0D0
         TVEC(3) =   32.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   29.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  Dec 32, 1995 12:13:29' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =   12.0D0
         TVEC(3) =   32.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   29.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  Jul 31, 1995 24:13:29' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    7.0D0
         TVEC(3) =   31.0D0
         TVEC(4) =   24.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   29.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  Jul 31, 1995 12:60:29' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    7.0D0
         TVEC(3) =   31.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   60.0D0
         TVEC(6) =   29.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )



      CALL TCASE ( 'Checking on:  Jul 31, 1995 12:13:60' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    7.0D0
         TVEC(3) =   31.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   60.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  Jul 31, 1995 13:13:29 A.M.' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    7.0D0
         TVEC(3) =   31.0D0
         TVEC(4) =   13.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   29.0D0

         MODS         = .TRUE.
         MODIFY(AMPM) = 'A.M.'

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )




      CALL TCASE ( 'Checking on:  "Jul.3" 1, 1995 00:00:00' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    7.3D0
         TVEC(3) =    1.0D0
         TVEC(4) =    0.0D0
         TVEC(5) =    0.0D0
         TVEC(6) =    0.0D0

         MODS    = .FALSE.

         CALL TCHECK ( TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  Jul 31.3, 1995 12:13:59' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    7.0D0
         TVEC(3) =   31.3D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.0D0
         TVEC(6) =   59.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  Jul 31, 1995 12.3:13:59' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    7.0D0
         TVEC(3) =   31.0D0
         TVEC(4) =   12.3D0
         TVEC(5) =   13.0D0
         TVEC(6) =   59.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )


      CALL TCASE ( 'Checking on:  Jul 31, 1995 12:13.3:59' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =    7.0D0
         TVEC(3) =   31.0D0
         TVEC(4) =   12.0D0
         TVEC(5) =   13.3D0
         TVEC(6) =   59.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )

      CALL TCASE ( 'Checking on:  Jan  1, 1995.2 00:00:00' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.2D0
         TVEC(2) =    1.0D0
         TVEC(3) =    1.0D0
         TVEC(4) =    0.0D0
         TVEC(5) =    0.0D0
         TVEC(6) =    0.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )



      CALL TCASE ( 'Checking on:  1995-366// 00:00:00' )

         CALL TPARCH ( 'YES' )

         TVEC(1) = 1995.0D0
         TVEC(2) =  366.0D0
         TVEC(3) =    0.0D0
         TVEC(4) =    0.0D0
         TVEC(5) =    0.0D0
         TVEC(6) =    0.0D0

         MODS    = .FALSE.

         CALL TCHECK (TVEC, 'YD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS',  PASS, .FALSE.,    OK )
         CALL CHCKSC ( 'ERROR', ERROR, '!=', ' ',  OK )

      CALL TCASE ( 'With checking turned on, make sure that ' &
      //           'normal items are not rejected. (YMD) ' )


         CALL TPARCH ( 'YES' )

         MODS = .FALSE.
         TYPE = 'YMD'

         SEC  = 0
         MIN  = 0
!
!        Here's the deal for every hour of every day in 1995 and
!        1996 we generate a time vector that should be legal
!        and then check it.
!
         DO YEAR = 1995, 1996

            TVEC(1) = DBLE(YEAR)

            DO MON = 1, 12

               TVEC(2) = DBLE(MON)

               DO DAY = 1, 31

                  IF (       MON  .EQ. 2 &
                       .AND. YEAR .EQ. 1996 &
                       .AND. DAY  .GT. 29  ) THEN

                     TVEC(3) = 29.0D0

                  ELSE IF (       MON .EQ. 2 &
                            .AND. DAY .GT. 28 ) THEN

                     TVEC(3) = 28.0D0

                  ELSE IF (         DAY .EQ. 31 &
                           .AND. (      MON .EQ. 4 &
                                   .OR. MON .EQ. 6 &
                                   .OR. MON .EQ. 9 &
                                   .OR. MON .EQ. 11 ) ) THEN

                     TVEC(3) = 30.0D0

                  ELSE

                     TVEC(3) = DBLE(DAY)

                  END IF

                  DO HR = 0, 23

                     MIN = MIN + 23
                     IF ( MIN .GE. 60 ) THEN
                        MIN = MIN - 60

                     END IF

                     SEC = SEC + 17

                     IF ( SEC .GE. 60 ) THEN
                        SEC = SEC - 60
                     END IF

                     TVEC(4) = DBLE(HR)
                     TVEC(5) = DBLE(MIN)
                     TVEC(6) = DBLE(SEC)

                     CALL TCHECK (TVEC, TYPE, MODS, MODIFY, PASS, ERROR)
                     CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )

                     IF ( .NOT. OK ) THEN
                        MESSGE = 'Year #, Month #, Day #, Hour ' &
                        //       '#, Minute #, Second #. '


                        CALL REPMI ( MESSGE, '#', YEAR, MESSGE )
                        CALL REPMI ( MESSGE, '#', MON, MESSGE )
                        CALL REPMI ( MESSGE, '#', DAY, MESSGE )
                        CALL REPMI ( MESSGE, '#', HR, MESSGE )
                        CALL REPMI ( MESSGE, '#', MIN, MESSGE )
                        CALL REPMI ( MESSGE, '#', SEC, MESSGE )

                        CALL TSTLOG ( MESSGE, .TRUE. )
                        CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )
                     END IF

                  END DO
               END DO
            END DO
         END DO

      CALL TCASE ( 'With checking turned on, make sure that ' &
      //           'normal items are not rejected. (YD) ' )


         CALL TPARCH ( 'YES' )

         MODS = .FALSE.
         TYPE = 'YD'

         DO YEAR = 1995, 1996

            TVEC(1) = DBLE(YEAR)

            DO DAY = 1, 366

               IF ( DAY .EQ. 366 .AND. YEAR .EQ. 1995 ) THEN
                  TVEC(2) = 365.0D0
               ELSE
                  TVEC(2) = DBLE(DAY)
               END IF

               DO HR = 0, 23

                  MIN = MIN + 23
                  IF ( MIN .GE. 60 ) THEN
                     MIN = MIN - 60

                  END IF

                  SEC = SEC + 17

                  IF ( SEC .GE. 60 ) THEN
                     SEC = SEC - 60
                  END IF

                  TVEC(3) = DBLE(HR)
                  TVEC(4) = DBLE(MIN)
                  TVEC(5) = DBLE(SEC)

                  CALL TCHECK (TVEC, TYPE, MODS, MODIFY, PASS, ERROR)
                  CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )

                  IF ( .NOT. OK ) THEN
                     MESSGE = 'Year #, Day #, Hour ' &
                     //       '#, Minute #, Second #. '


                     CALL REPMI ( MESSGE, '#', YEAR, MESSGE )
                     CALL REPMI ( MESSGE, '#', DAY, MESSGE )
                     CALL REPMI ( MESSGE, '#', HR, MESSGE )
                     CALL REPMI ( MESSGE, '#', MIN, MESSGE )
                     CALL REPMI ( MESSGE, '#', SEC, MESSGE )

                     CALL TSTLOG ( MESSGE, .TRUE. )
                     CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )
                  END IF

               END DO
            END DO
         END DO

      CALL TCASE ( 'Make sure that an epoch during a potential ' &
      //           'leapsecond is not regarded as erroneous. ' )


         TVEC(1) = 1995.0D0
         TVEC(2) =   12.0D0
         TVEC(3) =   31.0D0
         TVEC(4) =   23.0D0
         TVEC(5) =   59.0D0
         TVEC(6) =   60.1D0

         CALL TPARCH ( 'YES' )

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )

         IF ( .NOT. OK ) THEN
            CALL TSTLOG ( '1995 DEC 31 23:59:60.1', .TRUE. )
         END IF


         TVEC(1) = 1995.0D0
         TVEC(2) =    6.0D0
         TVEC(3) =   30.0D0
         TVEC(4) =   23.0D0
         TVEC(5) =   59.0D0
         TVEC(6) =   60.1D0

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )

         IF ( .NOT. OK ) THEN
            CALL TSTLOG ( '1995 JUN 30 23:59:60.1', .TRUE. )
         END IF

         TVEC(1) = 1996.0D0
         TVEC(2) =   12.0D0
         TVEC(3) =   31.0D0
         TVEC(4) =   23.0D0
         TVEC(5) =   59.0D0
         TVEC(6) =   60.1D0

         CALL TPARCH ( 'YES' )

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )

         IF ( .NOT. OK ) THEN
            CALL TSTLOG ( '1996 DEC 31 23:59:60.1', .TRUE. )
         END IF


         TVEC(1) = 1996.0D0
         TVEC(2) =    6.0D0
         TVEC(3) =   30.0D0
         TVEC(4) =   23.0D0
         TVEC(5) =   59.0D0
         TVEC(6) =   60.1D0

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )


         IF ( .NOT. OK ) THEN
            CALL TSTLOG ( '1996 JUN 30 23:59:60.1', .TRUE. )
         END IF

         TVEC(1) = 1995.0D0
         TVEC(2) =  365.0D0
         TVEC(3) =   23.0D0
         TVEC(4) =   59.0D0
         TVEC(5) =   60.1D0

         CALL TCHECK (TVEC, 'YD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )


         IF ( .NOT. OK ) THEN
            CALL TSTLOG ( '1995-365 // 23:59:60.1', .TRUE. )
         END IF

         TVEC(1) = 1996.0D0
         TVEC(2) =  366.0D0
         TVEC(3) =   23.0D0
         TVEC(4) =   59.0D0
         TVEC(5) =   60.1D0

         CALL TPARCH ( 'YES' )

         CALL TCHECK (TVEC, 'YD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )


         IF ( .NOT. OK ) THEN
            CALL TSTLOG ( '1996-366 // 23:59:60.1', .TRUE. )
         END IF


         TVEC(1) = 1995.0D0
         TVEC(2) =  181.0D0
         TVEC(3) =   23.0D0
         TVEC(4) =   59.0D0
         TVEC(5) =   60.1D0

         CALL TPARCH ( 'YES' )

         CALL TCHECK (TVEC, 'YD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )

         IF ( .NOT. OK )  THEN
            CALL TSTLOG ( '1995-182 // 23:59:60.1', .TRUE. )
         END IF


         TVEC(1) = 1996.0D0
         TVEC(2) =  182.0D0
         TVEC(3) =   23.0D0
         TVEC(4) =   59.0D0
         TVEC(5) =   60.1D0

         CALL TPARCH ( 'YES' )

         CALL TCHECK (TVEC, 'YD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )

         IF ( .NOT. OK )  THEN
            CALL TSTLOG ( '1995-183 // 23:59:60.1', .TRUE. )
         END IF


      CALL TCASE ( 'Make sure that an epoch during a potential ' &
      //           'leapsecond is not regarded as erroneous. ' &
      //           '( 11:00 P.M. subcase.' )


         TVEC(1) = 1995.0D0
         TVEC(2) =   12.0D0
         TVEC(3) =   31.0D0
         TVEC(4) =   11.0D0
         TVEC(5) =   59.0D0
         TVEC(6) =   60.1D0

         MODS = .TRUE.
         MODIFY(AMPM) = 'P.M.'

         CALL TPARCH ( 'YES' )

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )

         IF ( .NOT. OK ) THEN
            CALL TSTLOG ( '1995 DEC 31 P.M. 11:59:60.1', .TRUE. )
         END IF


         TVEC(1) = 1995.0D0
         TVEC(2) =    6.0D0
         TVEC(3) =   30.0D0
         TVEC(4) =   11.0D0
         TVEC(5) =   59.0D0
         TVEC(6) =   60.1D0

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )

         IF ( .NOT. OK ) THEN
            CALL TSTLOG ( '1995 JUN 30 P.M. 11:59:60.1', .TRUE. )
         END IF

         TVEC(1) = 1996.0D0
         TVEC(2) =   12.0D0
         TVEC(3) =   31.0D0
         TVEC(4) =   11.0D0
         TVEC(5) =   59.0D0
         TVEC(6) =   60.1D0

         CALL TPARCH ( 'YES' )

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )

         IF ( .NOT. OK ) THEN
            CALL TSTLOG ( '1996 DEC 31 P.M. 11:59:60.1', .TRUE. )
         END IF


         TVEC(1) = 1996.0D0
         TVEC(2) =    6.0D0
         TVEC(3) =   30.0D0
         TVEC(4) =   11.0D0
         TVEC(5) =   59.0D0
         TVEC(6) =   60.1D0

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )


         IF ( .NOT. OK ) THEN
            CALL TSTLOG ( '1996 JUN 30 P.M. 11:59:60.1', .TRUE. )
         END IF

         TVEC(1) = 1995.0D0
         TVEC(2) =  365.0D0
         TVEC(3) =   11.0D0
         TVEC(4) =   59.0D0
         TVEC(5) =   60.1D0

         CALL TCHECK (TVEC, 'YD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )


         IF ( .NOT. OK ) THEN
            CALL TSTLOG ( '1995-365 // P.M. 11:59:60.1', .TRUE. )
         END IF

         TVEC(1) = 1996.0D0
         TVEC(2) =  366.0D0
         TVEC(3) =   11.0D0
         TVEC(4) =   59.0D0
         TVEC(5) =   60.1D0

         CALL TPARCH ( 'YES' )

         CALL TCHECK (TVEC, 'YD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )


         IF ( .NOT. OK ) THEN
            CALL TSTLOG ( '1996-366 // P.M. 11:59:60.1', .TRUE. )
         END IF


         TVEC(1) = 1995.0D0
         TVEC(2) =  181.0D0
         TVEC(3) =   11.0D0
         TVEC(4) =   59.0D0
         TVEC(5) =   60.1D0

         CALL TPARCH ( 'YES' )

         CALL TCHECK (TVEC, 'YD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )

         IF ( .NOT. OK )  THEN
            CALL TSTLOG ( '1995-182 // P.M. 11:59:60.1', .TRUE. )
         END IF


         TVEC(1) = 1996.0D0
         TVEC(2) =  182.0D0
         TVEC(3) =   11.0D0
         TVEC(4) =   59.0D0
         TVEC(5) =   60.1D0

         CALL TPARCH ( 'YES' )

         CALL TCHECK (TVEC, 'YD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )

         IF ( .NOT. OK )  THEN
            CALL TSTLOG ( '1995-183 // P.M. 11:59:60.1', .TRUE. )
         END IF

         CALL TPARCH ( 'NO' )



      CALL TCASE ( 'Make sure that B.C. leap years are not rejected.' )


!
!        Set the year to 1 B.C.
!
         TVEC(1) =   1.0D0
         TVEC(2) =   2.0D0
         TVEC(3) =   29.0D0
         TVEC(4) =    0.0D0
         TVEC(5) =    0.0D0
         TVEC(6) =    0.0D0

         MODS = .TRUE.

         DO I = 1, NMODS
            MODIFY(I) = ' '
         END DO

         CALL TPARCH ( 'YES' )

         MODIFY(ERA) = 'B.C.'

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )

         IF ( .NOT. OK ) THEN
            CALL TSTLOG ( 'B.C. 1 FEB 29', .TRUE. )
         END IF



!
!        Set the year to 109 B.C.
!
         TVEC(1) =   109.0D0
         TVEC(2) =   2.0D0
         TVEC(3) =   29.0D0
         TVEC(4) =    0.0D0
         TVEC(5) =    0.0D0
         TVEC(6) =    0.0D0

         MODS = .TRUE.

         DO I = 1, NMODS
            MODIFY(I) = ' '
         END DO

         CALL TPARCH ( 'YES' )

         MODIFY(ERA) = 'B.C.'

         CALL TCHECK (TVEC, 'YMD', MODS, MODIFY, PASS, ERROR)
         CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )

         IF ( .NOT. OK ) THEN
            CALL TSTLOG ( 'B.C. 109 FEB 29', .TRUE. )
         END IF

         CALL TPARCH ( 'NO' )



      CALL T_SUCCESS ( OK )
      RETURN
      END
