!$Procedure      F_SPKAPP ( Family of tests for SPKAPP)

      SUBROUTINE F_SPKAPP ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the various pathways in SPKAPP.
!
!
! $ Version
!
!-    Version 2.0.0 13-DEC-2001 (NJB)
!
!-    Version 1.0.0  (WLT)
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      VNORM

!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      CHARACTER*(LNSIZE)    ERROR


      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ELT
      DOUBLE PRECISION      LT

      DOUBLE PRECISION      SOBS   ( 6 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STATE2 ( 6 )
      DOUBLE PRECISION      ESTATE ( 6 )

      INTEGER               SPKHAN


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SPKAPP' )

      CALL KILFIL ( 'test_pck.ker' )
      CALL TSTPCK ( 'test_pck.ker', .TRUE.,  .FALSE. )
      CALL TSTSPK ( 'test_spk.bsp', .TRUE.,   SPKHAN )
      CALL TPARSE ( '1 JAN 1995',    ET,      ERROR  )



      CALL TCASE ( 'Make sure unrecognized aberration ' &
      //           'corrections are handle as exceptions. ' )

      CALL SPKSSB ( 399,  ET, 'J2000', SOBS )
      CALL SPKAPP ( 499,  ET, 'J2000', SOBS, 'LTIME', STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINVALIDOPTION)',  OK )



      CALL TCASE ( 'Make sure that non-inertial frames are ' &
      //           'detected and diagnosed as unacceptable. ' )

      CALL SPKSSB ( 399,  ET, 'IAU_EARTH', SOBS )
      CALL SPKAPP ( 499,  ET, 'IAU_EARTH', SOBS, 'NONE', STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)',  OK )



      CALL TCASE ( 'Compare SPKAPP and SPKGEO when no ' &
      //           'corrections are requested ' )

      CALL SPKGEO ( 499, ET, 'DE-125', 399, ESTATE, ELT )

      CALL SPKSSB ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPP ( 499, ET, 'DE-125', SOBS, 'NONE', STATE, LT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, 1.0D-12, OK )
      CALL CHCKSD ( 'LT', LT,       '~/', ELT,       1.0D-12, OK )



      CALL TCASE ( 'Compare SPKAPP and SPKAPP_O ( the old ' &
      //           'version ) when light time only is requested. ' )

      CALL SPKSSB   ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPP   ( 499, ET, 'DE-125', SOBS, 'LT', STATE,  LT  )
      CALL SPKAPP_O ( 499, ET, 'DE-125', SOBS, 'LT', ESTATE, ELT )

      ELT = VNORM(ESTATE)/CLIGHT()

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, 1.0D-12, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       1.0D-12, OK )



      CALL TCASE ( 'Compare SPKAPP and SPKAPP_O when light time ' &
      //           'plus stellar aberration corrections are ' &
      //           'applied. ' )

      CALL SPKSSB   ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPP   ( 499, ET, 'DE-125', SOBS, 'LT+S', STATE,  LT )
      CALL SPKAPP_O ( 499, ET, 'DE-125', SOBS, 'LT+S', ESTATE, ELT )

      ELT = VNORM(ESTATE)/CLIGHT()

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, 1.0D-12, OK )
      CALL CHCKSD ( 'LT', LT,       '~/', ELT,       1.0D-12, OK )



      CALL TCASE ( 'Examine the differences in LT and STATES for ' &
      //           'when simple light time corrections are ' &
      //           'applied versus converged light time ' &
      //           'corrections. ' )

      CALL SPKSSB   ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPP   ( 799, ET, 'DE-125', SOBS, 'LT', STATE,  LT  )
      CALL SPKAPP   ( 799, ET, 'DE-125', SOBS, 'CN', ESTATE, ELT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, 1.0D-10, OK )
      CALL CHCKSD ( 'LT', LT,       '~',  ELT,       1.0D-7,  OK )



      CALL TCASE ( 'Repeat for the transmission case.' )

      CALL SPKSSB   ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPP   ( 799, ET, 'DE-125', SOBS, 'XLT', STATE,  LT  )
      CALL SPKAPP   ( 799, ET, 'DE-125', SOBS, 'XCN', ESTATE, ELT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, 1.0D-10, OK )
      CALL CHCKSD ( 'LT', LT,       '~',  ELT,       1.0D-7,  OK )


      CALL TCASE ( 'Repeat, using blanks and mixed case for REF, ' &
      //           'where REF is inertial'                          )

      CALL SPKSSB   ( 399, ET, 'DE-125',     SOBS )
      CALL SPKAPP   ( 799, ET, ' d E- 125 ', SOBS, 'XLT', STATE,  LT  )
      CALL SPKAPP   ( 799, ET, 'DE-125',     SOBS, 'XCN', ESTATE, ELT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, 1.0D-10, OK )
      CALL CHCKSD ( 'LT', LT,       '~',  ELT,       1.0D-7,  OK )


      CALL TCASE ( 'Test transmit light time:  make sure XLT ' &
      //           'corrections gives negative of position obtained ' &
      //           'when target observer vector is computed using ' &
      //           'at ET+LT using LT correction.' )

      CALL SPKSSB   ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPP   ( 799, ET, 'DE-125', SOBS, 'XLT', ESTATE,  ELT )

      CALL SPKSSB   ( 799, ET+ELT, 'DE-125', SOBS )
      CALL SPKAPP   ( 399, ET+ELT, 'DE-125', SOBS, 'LT', STATE, LT )
      CALL VMINUG   ( STATE, 6, STATE2 )

      CALL CHCKSD ( 'LT',    LT,     '~/',  ELT,       1.0D-8,  OK )
      CALL CHCKAD ( 'STATE', STATE2, '~~/', ESTATE, 3, 1.0D-8, OK )



      CALL TCASE ( 'Test transmit light time:  make sure XCN ' &
      //           'corrections gives negative of position obtained ' &
      //           'when target observer vector is computed using ' &
      //           'at ET+LT using CN correction.' )

      CALL SPKSSB   ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPP   ( 799, ET, 'DE-125', SOBS, 'XCN', ESTATE,  ELT )

      CALL SPKSSB   ( 799, ET+ELT, 'DE-125', SOBS )
      CALL SPKAPP   ( 399, ET+ELT, 'DE-125', SOBS, 'CN', STATE, LT )
      CALL VMINUG   ( STATE, 6, STATE2 )

      CALL CHCKSD ( 'LT',    LT,     '~/',  ELT,       1.0D-14, OK )
      CALL CHCKAD ( 'STATE', STATE2, '~~/', ESTATE, 3, 1.0D-14, OK )



      CALL TCASE ( 'Repeat, using mixed case and white space in flag.' )

      CALL SPKSSB   ( 399, ET, 'de-125', SOBS )
      CALL SPKAPP   ( 799, ET, 'DE-125', SOBS, ' Xc N', ESTATE,  ELT )

      CALL SPKSSB   ( 799, ET+ELT, 'DE-125', SOBS )
      CALL SPKAPP   ( 399, ET+ELT, 'DE-125', SOBS, 'C n', STATE, LT )
      CALL VMINUG   ( STATE, 6, STATE2 )

      CALL CHCKSD ( 'LT',    LT,     '~/',  ELT,       1.0D-14, OK )
      CALL CHCKAD ( 'STATE', STATE2, '~~/', ESTATE, 3, 1.0D-14, OK )



      CALL TCASE ( 'Test transmit stellar aberration:  make sure ' &
      //           'correction is same as that obtained by using ' &
      //           'LT only and then applying STLABX. ' )

      CALL SPKSSB ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPP ( 799, ET, 'DE-125', SOBS, 'XLT+S', ESTATE,  ELT )

      CALL SPKAPP ( 799, ET, 'DE-125', SOBS, 'XLT', STATE, LT )

      CALL STLABX ( STATE, SOBS(4), STATE2 )

      CALL CHCKSD ( 'LT',    LT,     '~/',  ELT,       1.0D-14,  OK )
      CALL CHCKAD ( 'STATE', STATE2, '~~/', ESTATE, 3, 1.0D-14, OK )



      CALL TCASE ( 'Repeat, using mixed case and white space in flag.' )

      CALL SPKSSB ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPP ( 799, ET, 'DE-125', SOBS, ' X lT+ s', ESTATE,  ELT )

      CALL SPKAPP ( 799, ET, 'DE-125', SOBS, 'XlT', STATE, LT )

      CALL STLABX ( STATE, SOBS(4), STATE2 )

      CALL CHCKSD ( 'LT',    LT,     '~/',  ELT,       1.0D-14,  OK )
      CALL CHCKAD ( 'STATE', STATE2, '~~/', ESTATE, 3, 1.0D-14, OK )


      CALL TCASE ( 'Test "CN" transmit stellar aberration:  make sure ' &
      //           'correction is same as that obtained by using ' &
      //           'CN only and then applying STLABX. ' )

      CALL SPKSSB ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPP ( 799, ET, 'DE-125', SOBS, 'XCN+S', ESTATE,  ELT )

      CALL SPKAPP ( 799, ET, 'DE-125', SOBS, 'XCN', STATE, LT )

      CALL STLABX ( STATE, SOBS(4), STATE2 )

      CALL CHCKSD ( 'LT',    LT,     '~/',  ELT,       1.0D-14,  OK )
      CALL CHCKAD ( 'STATE', STATE2, '~~/', ESTATE, 3, 1.0D-14, OK )

!
!     That's all folks.
!
      CALL SPKUEF ( SPKHAN )
      CALL KILFIL ( 'test_spk.bsp'  )


      CALL T_SUCCESS ( OK )
      RETURN
      END
