!$Procedure      F_JUL2GR ( Family of tests for Julian and Gregorian

      SUBROUTINE F_JUL2GR ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine checks to make sure that the conversion between
!     the Julian and Gregorian Calendars is performed correctly
!     and consistently.
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
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               LEAPS
      INTEGER               YEAR
      INTEGER               MONTH
      INTEGER               DAY
      INTEGER               DAYS
      INTEGER               DOY
      INTEGER               THISYR
      INTEGER               THISMO
      INTEGER               THISDY


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_JUL2GR' )


      CALL TCASE ( 'The Julian Date corresponding to October 15, ' &
      //           '1582 should be October 5, 1582 ' )

         YEAR = 1582
         MONTH = 10
         DAY   = 15

         CALL GR2JUL ( YEAR, MONTH, DAY, DOY )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'YEAR',  YEAR,  '=', 1582, 0, OK )
         CALL CHCKSI ( 'MONTH', MONTH, '=',   10, 0, OK )
         CALL CHCKSI ( 'DAY',   DAY,   '=',   05, 0, OK )


      CALL TCASE ( 'The Gregorian date corresponding to the ' &
      //           'Julian date October 5, 1582 should be ' &
      //           'October 15, 1582 ' )

         YEAR = 1582
         MONTH =  10
         DAY   =   5

         CALL JUL2GR ( YEAR, MONTH, DAY, DOY )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'YEAR',  YEAR,  '=', 1582, 0, OK )
         CALL CHCKSI ( 'MONTH', MONTH, '=',   10, 0, OK )
         CALL CHCKSI ( 'DAY',   DAY,   '=',   15, 0, OK )


      CALL TCASE ( 'September 3, 1752 Julian, should be ' &
      //           'September 14, 1752 Gregorian ' )


         YEAR = 1752
         MONTH =   9
         DAY   =   3

         CALL JUL2GR ( YEAR, MONTH, DAY, DOY )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'YEAR',  YEAR,  '=', 1752, 0, OK )
         CALL CHCKSI ( 'MONTH', MONTH, '=',    9, 0, OK )
         CALL CHCKSI ( 'DAY',   DAY,   '=',   14, 0, OK )

      CALL TCASE ( 'Make sure that on centuries, the number of ' &
      //           'days in the year is 366 for each Julian ' &
      //           'Year. ' )

         DO I = 0, 2000, 100

            YEAR   = I
            MONTH  = 12
            DAY    = 31

            CALL JUL2GR ( YEAR, MONTH, DAY, DOY )
            CALL GR2JUL ( YEAR, MONTH, DAY, DOY )

            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKSI ( 'YEAR',  YEAR,  '=',   I, 0, OK )
            CALL CHCKSI ( 'MONTH', MONTH, '=',  12, 0, OK )
            CALL CHCKSI ( 'DAY',   DAY,   '=',  31, 0, OK )
            CALL CHCKSI ( 'DOY',   DOY,   '=', 366, 0, OK )

         END DO

      CALL TCASE ( 'Make sure that the number of days on ' &
      //           'centuries is 366 only on those centuries ' &
      //           'divisible by 400. ' )


         DO I = 0, 2000, 100

            YEAR  = I
            DAYS  = 365 + MAX ( 1 + (YEAR/400)*400 - YEAR, 0 )

            MONTH = 12
            DAY   = 31

            CALL GR2JUL ( YEAR, MONTH, DAY, DOY )
            CALL JUL2GR ( YEAR, MONTH, DAY, DOY )

            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKSI ( 'YEAR',  YEAR,  '=',    I, 0, OK )
            CALL CHCKSI ( 'MONTH', MONTH, '=',   12, 0, OK )
            CALL CHCKSI ( 'DAY',   DAY,   '=',   31, 0, OK )
            CALL CHCKSI ( 'DOY',   DOY,   '=', DAYS, 0, OK )

         END DO



      CALL TCASE ( 'Perform a large set of cases from Julian to ' &
      //           'Gregorian and back. ' )



         DO I = 1904, 1904

            YEAR  = I
            LEAPS = MAX( (( YEAR/4 ) * 4) - YEAR + 1, 0 )

            DO J = 1, 12

               MONTH = J

               DO K = 1,31

                  IF ( MONTH .EQ. 2 ) THEN
                     DAY = MIN ( 28 + LEAPS, K )
                  ELSE IF ( MONTH .EQ. 4 ) THEN
                     DAY = MIN ( 30, K )
                  ELSE IF ( MONTH .EQ. 6 ) THEN
                     DAY = MIN ( 30, K )
                  ELSE IF ( MONTH .EQ. 9 ) THEN
                     DAY = MIN ( 30, K )
                  ELSE IF ( MONTH .EQ. 11 ) THEN
                     DAY = MIN ( 30, K )
                  ELSE
                     DAY = K
                  END IF

                  THISYR = YEAR
                  THISMO = MONTH
                  THISDY = DAY

                  CALL JUL2GR ( YEAR, MONTH, DAY, DOY )
                  CALL GR2JUL ( YEAR, MONTH, DAY, DOY )

                  CALL CHCKXC ( .FALSE., ' ',  OK )
                  CALL CHCKSI ( 'YEAR',  YEAR,  '=', THISYR, 0, OK )
                  CALL CHCKSI ( 'MONTH', MONTH, '=', THISMO, 0, OK )
                  CALL CHCKSI ( 'DAY',   DAY,   '=', THISDY, 0, OK )

               END DO

            END DO

         END DO


      CALL TCASE ( 'Perform a large set of cases from Gregorian to ' &
      //           'Julian and back. ' )


         DO I = 1604, 1604

            YEAR = I
            LEAPS =  MAX( (( YEAR/4 ) * 4) - YEAR + 1, 0 )

            DO J = 1, 12

               MONTH = J

               DO K = 1,31

                  IF ( MONTH .EQ. 2 ) THEN
                     DAY = MIN ( 28 + LEAPS, K )
                  ELSE IF ( MONTH .EQ. 4 ) THEN
                     DAY = MIN ( 30, K )
                  ELSE IF ( MONTH .EQ. 6 ) THEN
                     DAY = MIN ( 30, K )
                  ELSE IF ( MONTH .EQ. 9 ) THEN
                     DAY = MIN ( 30, K )
                  ELSE IF ( MONTH .EQ. 11 ) THEN
                     DAY = MIN ( 30, K )
                  ELSE
                     DAY = K
                  END IF

                  THISYR = YEAR
                  THISMO = MONTH
                  THISDY = DAY

                  CALL GR2JUL ( YEAR, MONTH, DAY, DOY )
                  CALL JUL2GR ( YEAR, MONTH, DAY, DOY )

                  CALL CHCKXC ( .FALSE., ' ',  OK )
                  CALL CHCKSI ( 'YEAR',  YEAR,  '=', THISYR, 0, OK )
                  CALL CHCKSI ( 'MONTH', MONTH, '=', THISMO, 0, OK )
                  CALL CHCKSI ( 'DAY',   DAY,   '=', THISDY, 0, OK )

               END DO

            END DO

         END DO

      CALL T_SUCCESS ( OK )
      RETURN
      END
