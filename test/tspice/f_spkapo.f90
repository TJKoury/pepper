!$Procedure      F_SPKAPO ( Family of tests for SPKAPO)

      SUBROUTINE F_SPKAPO ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the various pathways in SPKAPO.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Version
!
!-    Version 2.0.0 05-DEC-2001 (NJB)
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
      CALL TOPEN ( 'F_SPKAPO' )

      CALL KILFIL ( 'test_pck.ker' )
      CALL TSTPCK ( 'test_pck.ker', .TRUE.,  .FALSE. )
      CALL TSTSPK ( 'test_spk.bsp', .TRUE.,   SPKHAN )
      CALL TPARSE ( '1 JAN 1995',    ET,      ERROR  )



      CALL TCASE ( 'Make sure unrecognized aberration ' &
      //           'corrections are handle as exceptions. ' )

      CALL SPKSSB ( 399,  ET, 'J2000', SOBS )
      CALL SPKAPO ( 499,  ET, 'J2000', SOBS, 'LTIME', STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINVALIDOPTION)',  OK )


      CALL TCASE ( 'Make sure that non-inertial frames are ' &
      //           'detected and diagnosed as unacceptable. ' )

      CALL SPKSSB ( 399,  ET, 'IAU_EARTH', SOBS )
      CALL SPKAPO ( 499,  ET, 'IAU_EARTH', SOBS, 'NONE', STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)',  OK )


      CALL TCASE ( 'Compare SPKAPO and SPKGEO when no ' &
      //           'corrections are requested ' )

      CALL SPKGPS ( 499, ET, 'DE-125', 399, ESTATE, ELT )

      CALL SPKSSB ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPO ( 499, ET, 'DE-125', SOBS, 'NONE', STATE, LT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 3, 1.0D-14, OK )
      CALL CHCKSD ( 'LT', LT,       '~/', ELT,       1.0D-14, OK )



      CALL TCASE ( 'Compare SPKAPO and SPKAPP ' &
      //           'when light time only is requested. ' )

      CALL SPKSSB   ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPO   ( 499, ET, 'DE-125', SOBS, 'LT', STATE,  LT  )
      CALL SPKAPP   ( 499, ET, 'DE-125', SOBS, 'LT', ESTATE, ELT )

      ELT = VNORM(ESTATE)/CLIGHT()

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 3, 1.0D-14, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       1.0D-14, OK )



      CALL TCASE ( 'Repeat for transmission case.' )

      CALL SPKSSB   ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPO   ( 499, ET, 'DE-125', SOBS, 'XLT', STATE,  LT  )
      CALL SPKAPP   ( 499, ET, 'DE-125', SOBS, 'XLT', ESTATE, ELT )

      ELT = VNORM(ESTATE)/CLIGHT()

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 3, 1.0D-14, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       1.0D-14, OK )



      CALL TCASE ( 'Compare SPKAPO and SPKAPP when light time ' &
      //           'plus stellar aberration corrections are ' &
      //           'applied. ' )

      CALL SPKSSB   ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPO   ( 499, ET, 'DE-125', SOBS, 'LT+S', STATE,  LT )
      CALL SPKAPP   ( 499, ET, 'DE-125', SOBS, 'LT+S', ESTATE, ELT )

      ELT = VNORM(ESTATE)/CLIGHT()

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 3, 1.0D-14, OK )
      CALL CHCKSD ( 'LT', LT,       '~/', ELT,       1.0D-14, OK )


      CALL TCASE ( 'Repeat for transmission case.' )

      CALL SPKSSB   ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPO   ( 499, ET, 'DE-125', SOBS, 'XLT+S', STATE,  LT  )
      CALL SPKAPP   ( 499, ET, 'DE-125', SOBS, 'XLT+S', ESTATE, ELT )

      ELT = VNORM(ESTATE)/CLIGHT()

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 3, 1.0D-14, OK )
      CALL CHCKSD ( 'LT', LT,       '~/', ELT,       1.0D-14, OK )



      CALL TCASE ( 'Examine the differences in LT and STATES for ' &
      //           'when simple light time corrections are ' &
      //           'applied versus converged light time ' &
      //           'corrections. ' )

      CALL SPKSSB   ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPO   ( 799, ET, 'DE-125', SOBS, 'LT', STATE,  LT  )
      CALL SPKAPO   ( 799, ET, 'DE-125', SOBS, 'CN', ESTATE, ELT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 3, 1.0D-10, OK )
      CALL CHCKSD ( 'LT', LT,       '~',  ELT,       1.0D-7,  OK )


      CALL TCASE ( 'Repeat for transmission case.' )

      CALL SPKSSB   ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPO   ( 799, ET, 'DE-125', SOBS, 'XLT', STATE,  LT  )
      CALL SPKAPO   ( 799, ET, 'DE-125', SOBS, 'XCN', ESTATE, ELT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 3, 1.0D-10, OK )
      CALL CHCKSD ( 'LT', LT,       '~',  ELT,       1.0D-7,  OK )



      CALL TCASE ( 'Test transmit light time:  make sure XLT ' &
      //           'corrections gives negative of position obtained ' &
      //           'when target observer vector is computed using ' &
      //           'at ET+LT using LT correction.' )

      CALL SPKSSB   ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPO   ( 799, ET, 'DE-125', SOBS, 'XLT', ESTATE,  ELT )

      CALL SPKSSB   ( 799, ET+ELT, 'DE-125', SOBS )
      CALL SPKAPO   ( 399, ET+ELT, 'DE-125', SOBS, 'LT', STATE, LT )
      CALL VMINUS   ( STATE, STATE2 )

      CALL CHCKSD ( 'LT',    LT,     '~/',  ELT,       1.0D-8, OK )
      CALL CHCKAD ( 'STATE', STATE2, '~~/', ESTATE, 3, 1.0D-8, OK )



      CALL TCASE ( 'Test transmit light time:  make sure XCN ' &
      //           'corrections gives negative of position obtained ' &
      //           'when target observer vector is computed using ' &
      //           'at ET+LT using CN correction.' )

      CALL SPKSSB   ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPO   ( 799, ET, 'DE-125', SOBS, 'XCN', ESTATE,  ELT )

      CALL SPKSSB   ( 799, ET+ELT, 'DE-125', SOBS )
      CALL SPKAPO   ( 399, ET+ELT, 'DE-125', SOBS, 'CN', STATE, LT )
      CALL VMINUS   ( STATE, STATE2 )

      CALL CHCKSD ( 'LT',    LT,     '~/',  ELT,       1.0D-14, OK )
      CALL CHCKAD ( 'STATE', STATE2, '~~/', ESTATE, 3, 1.0D-14, OK )



      CALL TCASE ( 'Repeat, using mixed case and white space in flag.' )

      CALL SPKSSB   ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPO   ( 799, ET, 'DE-125', SOBS, ' Xc N', ESTATE,  ELT )

      CALL SPKSSB   ( 799, ET+ELT, 'DE-125', SOBS )
      CALL SPKAPO   ( 399, ET+ELT, 'DE-125', SOBS, 'C n', STATE, LT )
      CALL VMINUS   ( STATE, STATE2 )

      CALL CHCKSD ( 'LT',    LT,     '~/',  ELT,       1.0D-14, OK )
      CALL CHCKAD ( 'STATE', STATE2, '~~/', ESTATE, 3, 1.0D-14, OK )



      CALL TCASE ( 'Test transmit stellar aberration:  make sure ' &
      //           'correction is same as that obtained by using ' &
      //           'LT only and then applying STLABX. ' )

      CALL SPKSSB ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPO ( 799, ET, 'DE-125', SOBS, 'XLT+S', ESTATE,  ELT )

      CALL SPKAPO ( 799, ET, 'DE-125', SOBS, 'XLT', STATE, LT )

      CALL STLABX ( STATE, SOBS(4), STATE2 )

      CALL CHCKSD ( 'LT',    LT,     '=',  ELT,       0.D0,  OK )
      CALL CHCKAD ( 'STATE', STATE2, '=',  ESTATE, 3, 0.D0,  OK )



      CALL TCASE ( 'Repeat, using mixed case and white space in flag.' )
      CALL SPKSSB ( 399, ET, 'DE-125', SOBS )
      CALL SPKAPO ( 799, ET, 'DE-125', SOBS, ' X lT+ s', ESTATE,  ELT )

      CALL SPKAPO ( 799, ET, 'DE-125', SOBS, 'XlT', STATE, LT )

      CALL STLABX ( STATE, SOBS(4), STATE2 )

      CALL CHCKSD ( 'LT',    LT,     '=',  ELT,       0.D0,  OK )
      CALL CHCKAD ( 'STATE', STATE2, '=',  ESTATE, 3, 0.D0,  OK )

!
!     That's all folks.
!
      CALL SPKUEF ( SPKHAN )
      CALL KILFIL ( 'test_spk.bsp'  )


      CALL T_SUCCESS ( OK )
      RETURN
      END
