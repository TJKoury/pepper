!$Procedure      F_ETCAL ( Family of tests for ETCAL )

      SUBROUTINE F_ETCAL  ( OK )

      IMPLICIT NONE
      LOGICAL               OK


! $ Abstract
!
!     This subroutine checks a number simple inputs to ETCAL
!     to make sure that values returned are correct.
!
!     Note that since there are no calls to SIGERR in ETCAL
!     there will never be any exceptions. We don't check for any.
!
!-&

      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      SPD

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET2

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 48 )

      CHARACTER*(WDSIZE)    STRING
      CHARACTER*(WDSIZE)    ERROR
      CHARACTER*(WDSIZE)    TITLE

      INTEGER               I
      INTEGER               J



      CALL TOPEN ( 'F_ETCAL' )


      CALL TCASE ( 'Times out of range --- too big ' )

         ET = 1.0D17

         CALL ETCAL ( ET, STRING )
         CALL CHCKSC ( 'STRING', STRING(1:12), '=', 'Epoch after ', OK )

      CALL TCASE ( 'Times out of range --- too small ' )

         ET = -1.0D17

         CALL ETCAL ( ET, STRING )
         CALL CHCKSC ( 'STRING', STRING(1:12), '=', 'Epoch before', OK )


      CALL TCASE ( 'Zero point of the Julian Date Scale' )

         ET = - J2000()*SPD()
         CALL ETCAL ( ET, STRING )
         CALL CHCKSC( 'STRING', STRING, '=', &
                      '4714 B.C. NOV 24 12:00:00.000', OK )

      CALL TCASE ( 'From a string to ET and back ' )

         CALL TPARSE ( '1993 JAN 12, 13:12:28.999', ET, ERROR )
         CALL ETCAL  ( ET,  STRING )
         CALL CHCKSC ( 'STRING', STRING, '=', &
                       '1993 JAN 12 13:12:28.999', OK )


      CALL TCASE ( 'From a string to ET and back ' )

         CALL TPARSE ( '893 JAN 12, 13:12:28.999', ET, ERROR )
         CALL ETCAL  ( ET,  STRING )
         CALL CHCKSC ( 'STRING', STRING, '=', &
                       '893 A.D. JAN 12 13:12:28.999', OK )

!
!     In this case we loop over a large collection of ET values and
!     make sure that TPARSE reproduces the ET output from ETCAL.
!
      ET = -100000000.0D0
      DO I = 1, 100


         TITLE = 'TPARSE - ETCAL Compatibility #'
         J     =  I

         CALL REPMI ( TITLE, '#', J, TITLE )
         CALL TCASE ( TITLE )


            ET    = ET + 2000000.D0
            ERROR = ' '

            CALL ETCAL  ( ET, STRING )
            CALL TPARSE ( STRING, ET2, ERROR  )

            CALL  CHCKSC ( 'ERROR', ERROR, '=', ' ',        OK )
            CALL  CHCKSD ( 'ET',    ET,    '=', ET2, 0.0D0, OK )

      END DO


      CALL T_SUCCESS( OK )
      RETURN

      END
