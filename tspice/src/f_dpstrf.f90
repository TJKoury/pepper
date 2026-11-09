!$Procedure      F_DPSTRF (Family of tests for dpstrf )

      SUBROUTINE F_DPSTRF ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine performs a sequence of tests on the SPICELIB
!     routine dpstrf.
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

      CHARACTER*(LNSIZE)    ESTRNG
      CHARACTER*(LNSIZE)    STRING

      INTEGER               SIGDIG
      DOUBLE PRECISION      X

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_DPSTRF' )



      CALL TCASE  ( 'Test Rounding X =  1.28372D+8, SIGDIG = 2' )

         X      = 1.28372D+8
         SIGDIG = 2
         ESTRNG = ' 130000000.'

         CALL DPSTRF ( X, SIGDIG, 'F', STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE  ( 'Test Rounding X = -1.28372D+8, SIGDIG = 2' )

         X      = -1.28372D+8
         SIGDIG = 2
         ESTRNG = '-130000000.'

         CALL DPSTRF ( X, SIGDIG, 'F', STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )



      CALL TCASE  ( 'Test Rounding X =  9.9995 , SIGDIG = 4' )

         X      = 9.9995D+0
         SIGDIG = 4
         ESTRNG = ' 10.00'

         CALL DPSTRF ( X, SIGDIG, 'F', STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )



      CALL TCASE  ( 'Test Rounding X =  9.9995 , SIGDIG = 5' )

         X      = 9.9995D+0
         SIGDIG = 5
         ESTRNG = ' 9.9995'

         CALL DPSTRF ( X, SIGDIG, 'F', STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( 'Test small values X = 1.2829D-21, SIGDIG = 5' )

         X      = 1.2829D-21
         SIGDIG = 5
         ESTRNG = ' 0.0000000000000000000012829'


         CALL DPSTRF ( X, SIGDIG, 'F', STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( 'Test large values X = 9.92829D+35 SIGDIG = 3' )

         X      = 9.92829D+35
         SIGDIG = 3
         ESTRNG = &
         ' 993000000000000000000000000000000000.'

         CALL DPSTRF ( X, SIGDIG, 'F', STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( 'Test large values X = 9.96829D+35 SIGDIG = 2' )

         X      = 9.96829D+35
         SIGDIG = 2
         ESTRNG = &
         ' 1000000000000000000000000000000000000.'

         CALL DPSTRF ( X, SIGDIG, 'F', STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( 'Intermediate value of X with non-zero ' &
      //           'integer and fractional parts.  X = 123.18272 ' &
      //           'SIGDIG = 14 ' )

         X      = 123.18272D+0
         SIGDIG = 14

         ESTRNG = ' 123.18272000000'

         CALL DPSTRF ( X, SIGDIG, 'F', STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( 'Intermediate value of X with non-zero ' &
      //           'integer and fractional parts.  X = -123.18272 ' &
      //           'SIGDIG = 14 ' )

         X      = -123.18272D+0
         SIGDIG = 14

         ESTRNG = '-123.18272000000'

         CALL DPSTRF ( X, SIGDIG, 'F', STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( 'Zero case X = 0, SIGDIG = 7 ' )

         X      = 0.0D0
         SIGDIG = 7
         ESTRNG = ' 0.0000000'

         CALL DPSTRF ( X, SIGDIG, 'F', STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
