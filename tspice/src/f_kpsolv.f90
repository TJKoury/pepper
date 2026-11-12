!$Procedure      F_KPSOLV (Family of tests for KPSOLV )

      SUBROUTINE F_KPSOLV ( OK )
      use pepper_legacy
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine loops over a large set of points in the
!     unit circle to test the accuracy of the computations
!     carried out by KPSOLV.
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
      DOUBLE PRECISION      EVEC ( 2 )
      DOUBLE PRECISION      FX
      DOUBLE PRECISION      H
      DOUBLE PRECISION      K
      DOUBLE PRECISION      R
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      X

      INTEGER               I
      INTEGER               J



!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_KPSOLV' )

      CALL TCASE ( 'Determine how well the solution fits h,k ' &
      //           'whose polar coordinates are r = {0, .05, .1, ' &
      //           '... .9} and theta = { 0, .1 ... pi*2 } ' )


         R = 0.0D0

         DO I = 1,20

            THETA = 0.0D0

            DO J = 1, 63

               CALL TSTMSG ( '#', 'Failure for R = # and THETA = #' )
               CALL TSTMSF ( R     )
               CALL TSTMSF ( THETA )

               H       = R * COS(THETA)
               K       = R * SIN(THETA)
               EVEC(1) = H
               EVEC(2) = K

               X  = KPSOLV( EVEC )
               FX = H*COS(X) + K*SIN(X)

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSD ( 'FX', FX, '~', X, 1.0D-15, OK )

               THETA = THETA + 0.1D0

            END DO

            R = R + 0.05D0

         END DO



      CALL T_SUCCESS ( OK )
      RETURN
      END
