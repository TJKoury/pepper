!$Procedure F_ZZGETELM ( Family of tests for ZZGETELM)

      SUBROUTINE F_ZZGETELM ( OK )
      use pepper_legacy
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine performs a number of tests of the SPICELIB
!     routine ZZGETELM
!
! $ Author
!
!     E. D. Wright 17-APR-2004 (JPL)
!-&


      CHARACTER*(80)         LINES(2)
      CHARACTER*(128)        ERROR
      INTEGER                FRSTYR
      DOUBLE PRECISION       EPOCH
      DOUBLE PRECISION       ELEMS (10)
      LOGICAL                ERRSIG

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGETELM' )


!
!     Create and load  leapseconds kernel.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Assign test values.
!
      FRSTYR   = 1969

!
!     Check for a positive process on a correct TLE set.
!

      CALL TCASE ( 'Confirm processing of a good TLE' )

      LINES(1) = '1 25544U 98067A   98324.89267077  .00616830' &
               //'  11572-4  29139-3 0    84'
      LINES(2) = '2 25544  51.5947 165.1012 0122649  89.2072 ' &
               //'272.3014 16.05443269   101'

      CALL ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, ERRSIG, ERROR )
      CALL CHCKSL ( 'OK 1', ERRSIG, .TRUE.,  OK )

!
!     Call the user level routine. Expect no errors.
!
      CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Now test the error checks. Too long. Longest allowed line: 69.
!
      CALL TCASE ( 'TLE too long' )

      LINES(1) = '1 25544U 98067A   98324.89267077  .00616830' &
               //'  11572-4  29139-3 0    84'
      LINES(2) = '2 25544  51.5947 165.1012 0122649  89.2072 ' &
               //'272.3014 16.05443269       101'

      CALL ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, ERRSIG, ERROR )
      CALL CHCKSL ( 'OK 3', ERRSIG, .FALSE.,  OK )

      CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
      CALL CHCKXC ( .TRUE., 'SPICE(BADTLE)', OK )


!
!     Now test the error checks. Too short. Shortest allowed line: 68.
!
      CALL TCASE ( 'TLE too short' )

      LINES(1) = '1 25544U 98067A   98324.89267077  .00616830' &
               //'  11572-4  29139-3 0 84'
      LINES(2) = '2 25544  51.5947 165.1012 0122649  89.2072 ' &
               //'272.3014 16.05443269 101'

      CALL ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, ERRSIG, ERROR )
      CALL CHCKSL ( 'OK 3', ERRSIG, .FALSE.,  OK )

      CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
      CALL CHCKXC ( .TRUE., 'SPICE(BADTLE)', OK )


!
!     Check bounds on numerical values.
!

!
!     Inclination [0,180]
!
      CALL TCASE ( 'Inclination low' )

      LINES(1) = '1 25544U 98067A   98324.89267077  .00616830' &
               //'  11572-4  29139-3 0    84'
      LINES(2) = '2 25544 -51.5947 165.1012 0122649  89.2072 ' &
               //'272.3014 16.05443269   101'

      CALL ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, ERRSIG, ERROR )
      CALL CHCKSL ( 'OK 4', ERRSIG, .FALSE.,  OK )

      CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
      CALL CHCKXC ( .TRUE., 'SPICE(BADTLE)', OK )

      CALL TCASE ( 'Inclination high' )

      LINES(1) = '1 25544U 98067A   98324.89267077  .00616830' &
               //'  11572-4  29139-3 0    84'
      LINES(2) = '2 25544 181.5947 165.1012 0122649  89.2072 ' &
               //'272.3014 16.05443269   101'

      CALL ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, ERRSIG, ERROR )
      CALL CHCKSL ( 'OK 5', ERRSIG, .FALSE.,  OK )

      CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
      CALL CHCKXC ( .TRUE., 'SPICE(BADTLE)', OK )

!
!     RA ascending node [0,360)
!
      CALL TCASE ( 'RA node low' )

      LINES(1) = '1 25544U 98067A   98324.89267077  .00616830' &
               //'  11572-4  29139-3 0    84'
      LINES(2) = '2 25544  51.5947 -65.1012 0122649  89.2072 ' &
               //'272.3014 16.05443269   101'

      CALL ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, ERRSIG, ERROR )
      CALL CHCKSL ( 'OK 4', ERRSIG, .FALSE.,  OK )

      CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
      CALL CHCKXC ( .TRUE., 'SPICE(BADTLE)', OK )


      CALL TCASE ( 'RA node high' )

      LINES(1) = '1 25544U 98067A   98324.89267077  .00616830' &
               //'  11572-4  29139-3 0    84'
      LINES(2) = '2 25544  51.5947 365.1012 0122649  89.2072 ' &
               //'272.3014 16.05443269   101'

      CALL ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, ERRSIG, ERROR )
      CALL CHCKSL ( 'OK 4', ERRSIG, .FALSE.,  OK )

      CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
      CALL CHCKXC ( .TRUE., 'SPICE(BADTLE)', OK )


!
!     Arg periapsis [0,360)
!
      CALL TCASE ( 'Arg periapsis low' )

      LINES(1) = '1 25544U 98067A   98324.89267077  .00616830' &
               //'  11572-4  29139-3 0    84'
      LINES(2) = '2 25544  51.5947  65.1012 0122649 -89.2072 ' &
               //'272.3014 16.05443269   101'

      CALL ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, ERRSIG, ERROR )
      CALL CHCKSL ( 'OK 4', ERRSIG, .FALSE.,  OK )

      CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
      CALL CHCKXC ( .TRUE., 'SPICE(BADTLE)', OK )

      CALL TCASE ( 'Arg periapsis high' )

      LINES(1) = '1 25544U 98067A   98324.89267077  .00616830' &
               //'  11572-4  29139-3 0    84'
      LINES(2) = '2 25544  51.5947  65.1012 0122649 389.2072 ' &
               //'272.3014 16.05443269   101'

      CALL ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, ERRSIG, ERROR )
      CALL CHCKSL ( 'OK 4', ERRSIG, .FALSE.,  OK )

      CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
      CALL CHCKXC ( .TRUE., 'SPICE(BADTLE)', OK )


!
!     Mean anomoly [0,360)
!
      CALL TCASE ( 'Mean anomoly low' )

      LINES(1) = '1 25544U 98067A   98324.89267077  .00616830' &
               //'  11572-4  29139-3 0    84'
      LINES(2) = '2 25544  51.5947 165.1012 0122649  89.2072 ' &
               //'-72.3014 16.05443269   101'

      CALL ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, ERRSIG, ERROR )
      CALL CHCKSL ( 'OK 4', ERRSIG, .FALSE.,  OK )

      CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
      CALL CHCKXC ( .TRUE., 'SPICE(BADTLE)', OK )

      CALL TCASE ( 'Mean anomoly high' )

      LINES(1) = '1 25544U 98067A   98324.89267077  .00616830' &
               //'  11572-4  29139-3 0    84'
      LINES(2) = '2 25544  51.5947 165.1012 0122649  89.2072 ' &
               //'360.3014 16.05443269   101'

      CALL ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, ERRSIG, ERROR )
      CALL CHCKSL ( 'OK 4', ERRSIG, .FALSE.,  OK )

      CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
      CALL CHCKXC ( .TRUE., 'SPICE(BADTLE)', OK )

!
!     Mean motion (0,20)
!
      CALL TCASE ( 'Mean motion low' )

      LINES(1) = '1 25544U 98067A   98324.89267077  .00616830' &
               //'  11572-4  29139-3 0    84'
      LINES(2) = '2 25544  51.5947 165.1012 0122649  89.2072 ' &
               //'272.3014 -16.05443269   101'

      CALL ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, ERRSIG, ERROR )
      CALL CHCKSL ( 'OK 4', ERRSIG, .FALSE.,  OK )

      CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
      CALL CHCKXC ( .TRUE., 'SPICE(BADTLE)', OK )

      CALL TCASE ( 'Mean motion high' )

      LINES(1) = '1 25544U 98067A   98324.89267077  .00616830' &
               //'  11572-4  29139-3 0    84'
      LINES(2) = '2 25544  51.5947 165.1012 0122649  89.2072 ' &
               //'272.3014 20.05443269   101'

      CALL ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, ERRSIG, ERROR )
      CALL CHCKSL ( 'OK 4', ERRSIG, .FALSE.,  OK )

      CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
      CALL CHCKXC ( .TRUE., 'SPICE(BADTLE)', OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END

