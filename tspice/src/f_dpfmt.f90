!$Procedure      F_DPFMT ( Family of tests for DPFMT)

      SUBROUTINE F_DPFMT ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine exercises the routine DPFMT to make sure
!     that the outputs are as expected.
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
      PARAMETER           ( LNSIZE = 300 )

      CHARACTER*(LNSIZE)    STRING
      CHARACTER*(LNSIZE)    ESTRNG

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(LNSIZE)    FMT

      DOUBLE PRECISION      X


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_DPFMT' )

      X = 1.234D+5

      CALL TCASE ( 'Exception PICTUR = '' '' ' )

         FMT = ' '
         CALL DPFMT  ( X, FMT, STRING )
         CALL CHCKXC ( .TRUE., 'SPICE(NOPICTURE)', OK )

      CALL TCASE ( 'Exception PICTUR = ''+'' ' )

         FMT = '+'
         CALL DPFMT  ( X, FMT, STRING )
         CALL CHCKXC ( .TRUE., 'SPICE(BADPICTURE)', OK )

      CALL TCASE ( 'Exception PICTUR = ''-'' ' )

         FMT = '-'
         CALL DPFMT  ( X, FMT, STRING )
         CALL CHCKXC ( .TRUE., 'SPICE(BADPICTURE)', OK )

      CALL TCASE ( 'Exception PICTUR = ''.'' ' )

         FMT = '.'
         CALL DPFMT  ( X, FMT, STRING )
         CALL CHCKXC ( .TRUE., 'SPICE(BADPICTURE)', OK )

      CALL TCASE ( 'Exception PICTUR = ''+.'' ' )

         FMT = '+.'
         CALL DPFMT  ( X, FMT, STRING )
         CALL CHCKXC ( .TRUE., 'SPICE(BADPICTURE)', OK )

      CALL TCASE ( 'Exception PICTUR = ''-.'' ' )

         FMT = '-.'
         CALL DPFMT  ( X, FMT, STRING )
         CALL CHCKXC ( .TRUE., 'SPICE(BADPICTURE)', OK )


      CALL TCASE ( 'Exception Long picture/short output string' )

         FMT = 'xxxx.xxxxxxxxx'
         CALL DPFMT  ( X, FMT, STRING(1:8) )
         CALL CHCKXC ( .TRUE., 'SPICE(OUTPUTTOOSHORT)', OK )


      CALL TCASE ( 'X = 1.23456789D-37, fmt = ' &
      //           '''xxxx.' &
      //           'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'' ')


         X       = 1.23456789D-37
         FMT     = 'xxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
         ESTRNG  = '   0.00000000000000000000000000000'

         CALL DPFMT ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( 'X = 1.23456789D+32, fmt = xxxx.xxx' )

         X       = 1.23456789D+32
         FMT     = 'xxxx.xxx'
         ESTRNG  = '1.23E+32'


         CALL DPFMT ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( 'X = 1.28289392D+7 , FMT = +xxxxxxxx.xxx ' )

         X      =   1.28289392D+7
         FMT    = '+xxxxxxxxx.xxx'
         ESTRNG = '+ 12828939.200'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( 'X = 182.938  fmt = xxx' )

         X      = 182.938D+0
         FMT    = 'xxx'
         ESTRNG = '183'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( 'X = 182.938  fmt = xx' )

         X      = 182.938D+0
         FMT    = 'xx'
         ESTRNG = '**'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( 'X = -182.938,  fmt = xx.xxxxxx' )

         X      = -182.938D+00
         FMT    = 'xx.xxxxxx'
         ESTRNG = '-1.83E+02'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( 'X = 8.50D+0, fmt = 0x' )

         X      = 8.5D+0
         FMT    = '0x'
         ESTRNG = '09'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( ' X = -80.5D+0, fmt = xx ' )

         X      = -80.5D+0
         FMT    = 'xx'
         ESTRNG = '**'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( ' X = 80.5D+0, fmt = xx ' )

         X      =  80.5D+0
         FMT    = 'xx'
         ESTRNG = '81'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( ' X = -80.5D+0, fmt = xxx ' )

         X      = -80.5D+0
         FMT    = 'xxx'
         ESTRNG = '-81'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( ' X =  80.5D+0, fmt = -xx ' )

         X      =  80.5D+0
         FMT    = 'xxx'
         ESTRNG = ' 81'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( ' X = 9.99,   fmt = x.x' )

         X      =  9.99D+0
         FMT    = 'x.x'
         ESTRNG = '***'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( ' X = 9.99,   fmt = x.xx' )

         X      =  9.99D+0
         FMT    = 'x.xx'
         ESTRNG = '9.99'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( 'X = 99.9999999  fmt = xx.xxxxxx' )

         X      =  99.9999999D+0
         FMT    = 'xx.xxxxxx'
         ESTRNG = '1.000E+02'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( 'X = 99.9999999  fmt = +xx.xxxxxx' )

         X      =  99.9999999D+0
         FMT    = '+xx.xxxxxx'
         ESTRNG = '+1.000E+02'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( 'X = 99.9999999  fmt = -xx.xxxxxx' )

         X      =  99.9999999D+0
         FMT    = '-xx.xxxxxx'
         ESTRNG = ' 1.000E+02'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( 'X = 0.28910, fmt = +.xxxx' )

         X      =  0.28910D+0
         FMT    = '+.xxxx'
         ESTRNG = '+.2891'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( 'X = -0.28910, fmt = x.xxx ' )

         X      = -0.28910D+0
         FMT    = 'x.xxxx'
         ESTRNG = '-.2891'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( 'X = 0.0D0, fmt = +x.xxxx ' )

         X      =   0.0D0
         FMT    = '+x.xxxx'
         ESTRNG = ' 0.0000'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( 'X = 0.0D0, fmt = .xxxx ' )

         X      =   0.0D0
         FMT    = '.xxxx'
         ESTRNG = '.0000'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( ' X = -1.000D0, fmt = xxxx.xxx ' )

         X      =  -1.0D0
         FMT    = 'xxxx.xxx'
         ESTRNG = '  -1.000'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( ' X = -1.000D0, fmt = -xxxx.xxx ' )

         X      =  -1.0D0
         FMT    = '-xxxx.xxx'
         ESTRNG = '-   1.000'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( 'X = 123.456  fmt = xxxxxx.xxx' )

         X      =  123.456D0
         FMT    = 'xxxxxx.xxx'
         ESTRNG = '   123.456'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( 'X = -123.456  fmt = xxxxxx.xxx' )

         X      =  -123.456D0
         FMT    = 'xxxxxx.xxx'
         ESTRNG = '  -123.456'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( 'X = -123.456  fmt = 0xxxxx.xxx' )

         X      =  -123.456D0
         FMT    = '0xxxxx.xxx'
         ESTRNG = '-00123.456'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( 'X =  123.456  fmt = 0xxxxx.xxx' )

         X      =   123.456D0
         FMT    = '0xxxxx.xxx'
         ESTRNG = '000123.456'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( 'X =  123.456  fmt = +0xxxx.xxx' )

         X      =   123.456D0
         FMT    = '+0xxxx.xxx'
         ESTRNG = '+00123.456'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL TCASE ( 'X =  123.456  fmt = -0xxxx.xxx' )

         X      =   123.456D0
         FMT    = '-0xxxx.xxx'
         ESTRNG = ' 00123.456'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( 'X =  123.456  fmt = -xxxxx.xxx' )

         X      =   123.456D0
         FMT    = '-xxxxx.xxx'
         ESTRNG = '   123.456'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )

      CALL TCASE ( 'X =  123.456  fmt = +xxxxx.xxx' )

         X      =   123.456D0
         FMT    = '+xxxxx.xxx'
         ESTRNG = '+  123.456'

         CALL DPFMT  ( X, FMT, STRING )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
