!$Procedure      F_Q2M (Family of tests for Q2M )

      SUBROUTINE F_Q2M ( OK )
      use pepper_legacy
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This performs a set of rudimentary tests of the
!     SPICE subroutine Q2M
!
!-&


!
!     Test Utility Functions
!


!
!     Local Variables
!
      DOUBLE PRECISION      Q ( 0:3 )

      DOUBLE PRECISION      M    ( 3, 3 )
      DOUBLE PRECISION      MEXP ( 3, 3 )


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_Q2M' )

      CALL TCASE ( 'Identity Transformation' )


         Q(0) = 1.0D0

         Q(1) = 0.0D0
         Q(2) = 0.0D0
         Q(3) = 0.0D0

         CALL Q2M   ( Q, M )
         CALL IDENT ( MEXP )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'M', M, '~', MEXP, 9, 1.0D-14, OK )

      CALL TCASE ( 'Rotation by 90 degrees about Z' )

         Q(0) = DSQRT(2.0D0) / 2.0D0
         Q(1) = 0.0D0
         Q(2) = 0.0D0
         Q(3) = Q(0)


         MEXP ( 1, 1 ) =  0.0D0
         MEXP ( 2, 1 ) =  1.0D0
         MEXP ( 3, 1 ) =  0.0D0

         MEXP ( 1, 2 ) = -1.0D0
         MEXP ( 2, 2 ) =  0.0D0
         MEXP ( 3, 2 ) =  0.0D0

         MEXP ( 1, 3 ) =  0.0D0
         MEXP ( 2, 3 ) =  0.0D0
         MEXP ( 3, 3 ) =  1.0D0

         CALL Q2M   ( Q, M )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'M', M, '~', MEXP, 9, 1.0D-14, OK )


      CALL TCASE ( 'Rotation by 90 degrees about Y' )

         Q(0) = DSQRT(2.0D0) / 2.0D0
         Q(1) = 0.0D0
         Q(2) = Q(0)
         Q(3) = 0.0D0


         MEXP ( 1, 1 ) =  0.0D0
         MEXP ( 2, 1 ) =  0.0D0
         MEXP ( 3, 1 ) = -1.0D0

         MEXP ( 1, 2 ) =  0.0D0
         MEXP ( 2, 2 ) =  1.0D0
         MEXP ( 3, 2 ) =  0.0D0

         MEXP ( 1, 3 ) =  1.0D0
         MEXP ( 2, 3 ) =  0.0D0
         MEXP ( 3, 3 ) =  0.0D0

         CALL Q2M   ( Q, M )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'M', M, '~', MEXP, 9, 1.0D-14, OK )


      CALL TCASE ( 'Rotation by 90 degrees about X' )

         Q(0) = DSQRT(2.0D0) / 2.0D0
         Q(1) = Q(0)
         Q(2) = 0.0D0
         Q(3) = 0.0D0


         MEXP ( 1, 1 ) =  1.0D0
         MEXP ( 2, 1 ) =  0.0D0
         MEXP ( 3, 1 ) =  0.0D0

         MEXP ( 1, 2 ) =  0.0D0
         MEXP ( 2, 2 ) =  0.0D0
         MEXP ( 3, 2 ) =  1.0D0

         MEXP ( 1, 3 ) =  0.0D0
         MEXP ( 2, 3 ) = -1.0D0
         MEXP ( 3, 3 ) =  0.0D0

         CALL Q2M   ( Q, M )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'M', M, '~', MEXP, 9, 1.0D-14, OK )


      CALL TCASE ( 'Rotation by 90 degrees about X with non-unit ' &
      //           'input quaternion. ' )


         Q(0) = 1.0D0
         Q(1) = Q(0)
         Q(2) = 0.0D0
         Q(3) = 0.0D0


         MEXP ( 1, 1 ) =  1.0D0
         MEXP ( 2, 1 ) =  0.0D0
         MEXP ( 3, 1 ) =  0.0D0

         MEXP ( 1, 2 ) =  0.0D0
         MEXP ( 2, 2 ) =  0.0D0
         MEXP ( 3, 2 ) =  1.0D0

         MEXP ( 1, 3 ) =  0.0D0
         MEXP ( 2, 3 ) = -1.0D0
         MEXP ( 3, 3 ) =  0.0D0

         CALL Q2M   ( Q, M )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'M', M, '~', MEXP, 9, 1.0D-14, OK )


      CALL TCASE ( 'Rotation by 60 degrees about Z' )
!
!        Recall that we need to put in the COS and SIN of 30 degrees
!        into the various components of the quaternion.  But these
!        values are SQRT(3)/2 and 0.5 respectively.
!
         Q(0) = DSQRT(3.0D0) / 2.0D0
         Q(1) = 0.0D0
         Q(2) = 0.0D0
         Q(3) = 0.5D0


         MEXP ( 1, 1 ) =  0.5D0
         MEXP ( 2, 1 ) =  Q(0)
         MEXP ( 3, 1 ) =  0.0D0

         MEXP ( 1, 2 ) = -Q(0)
         MEXP ( 2, 2 ) =  0.5D0
         MEXP ( 3, 2 ) =  0.0D0

         MEXP ( 1, 3 ) =  0.0D0
         MEXP ( 2, 3 ) =  0.0D0
         MEXP ( 3, 3 ) =  1.0D0

         CALL Q2M   ( Q, M )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'M', M, '~', MEXP, 9, 1.0D-14, OK )


      CALL TCASE ( 'Rotation by 60 degrees about Y' )

         Q(0) = DSQRT(3.0D0) / 2.0D0
         Q(1) = 0.0D0
         Q(2) = 0.5D0
         Q(3) = 0.0D0


         MEXP ( 1, 1 ) =  0.5D0
         MEXP ( 2, 1 ) =  0.0D0
         MEXP ( 3, 1 ) = -Q(0)

         MEXP ( 1, 2 ) =  0.0D0
         MEXP ( 2, 2 ) =  1.0D0
         MEXP ( 3, 2 ) =  0.0D0

         MEXP ( 1, 3 ) =  Q(0)
         MEXP ( 2, 3 ) =  0.0D0
         MEXP ( 3, 3 ) =  0.5D0

         CALL Q2M   ( Q, M )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'M', M, '~', MEXP, 9, 1.0D-14, OK )


      CALL TCASE ( 'Rotation by 60 degrees about X' )

         Q(0) = DSQRT(3.0D0) / 2.0D0
         Q(1) = 0.5D0
         Q(2) = 0.0D0
         Q(3) = 0.0D0


         MEXP ( 1, 1 ) =  1.0D0
         MEXP ( 2, 1 ) =  0.0D0
         MEXP ( 3, 1 ) =  0.0D0

         MEXP ( 1, 2 ) =  0.0D0
         MEXP ( 2, 2 ) =  0.5D0
         MEXP ( 3, 2 ) =  Q(0)

         MEXP ( 1, 3 ) =  0.0D0
         MEXP ( 2, 3 ) = -Q(0)
         MEXP ( 3, 3 ) =  0.5D0

         CALL Q2M   ( Q, M )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'M', M, '~', MEXP, 9, 1.0D-14, OK )






      CALL T_SUCCESS ( OK )
      RETURN
      END
