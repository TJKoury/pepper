!$Procedure      F_LTIME ( Family of light time tests )

      SUBROUTINE F_LTIME ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the routine LTIME to make sure that it
!     gives results compatible with SPKEZ.
!
!-&


!
!     Test Utility Functions
!
!     None.
!
!
!     SPICELIB Functions
!
      EXTERNAL              LTIME
!
!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      CHARACTER*(LNSIZE)    ERROR

      DOUBLE PRECISION      ELAPS
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET2
      DOUBLE PRECISION      EXPET
      DOUBLE PRECISION      EXPET2
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      STATE ( 6 )

      INTEGER               HANDLE

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_LTIME' )

      CALL TSTSPK ( 'test.bsp', .TRUE., HANDLE )

      CALL TCASE ( 'Test the advertised exception' )

         CALL TPARSE ( '1 JAN 1995',        ET,  ERROR )
         CALL LTIME  (  ET, 399, 'TO', 499, ET2, ELAPS )

         CALL CHCKXC ( .TRUE., 'SPICE(BADDIRECTION)', OK )

      CALL TCASE ( 'Check to make sure downlink light time ' &
      //           'matches the light time returned by SPKEZ ' &
      //           'when the option ''CN'' is used in SPKEZ. ' )

         CALL TPARSE ( '1 JAN 1995',            ET,  ERROR        )
         CALL LTIME  (  ET, 399, '<-',          599, ET2,   ELAPS )
         CALL SPKEZ  (  599, ET, 'J2000', 'CN', 399, STATE, LT    )

         EXPET2 = ET - LT

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ELAPS', ELAPS, '~', LT,    1.0D-12, OK )
         CALL CHCKSD ( 'ET2',   ET2, '~/', EXPET2, 1.0D-13, OK )

      CALL TCASE ( 'Check to make sure that the uplink time is ' &
      //           'compatible with the down link time. ' )

         CALL TPARSE ( '1 JAN 1995', ET, ERROR )
         CALL LTIME  (  ET,  399, '->', 599, ET2,   LT )
         CALL LTIME  (  ET2, 599, '<-', 399, EXPET, ELAPS )

         EXPET2 = ET + LT

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ET2',   ET2, '~/', EXPET2, 1.0D-13, OK )
         CALL CHCKSD ( 'ELAPS', ELAPS, '~/', LT,    1.0D-13, OK )
         CALL CHCKSD ( 'ET',    ET,    '~/', EXPET, 1.0D-13, OK )


!
!     That's all folks.
!
      CALL SPKUEF (  HANDLE    )
      CALL KILFIL ( 'test.bsp' )


      CALL T_SUCCESS ( OK )
      RETURN
      END
