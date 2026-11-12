!$Procedure      F_TSTCK3 ( Family of tests for tstck3 and tstatd )

      SUBROUTINE F_TSTCK3 ( OK )
      use pepper_legacy
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine checks out the test routines TSTCK3 and TSTATD.
!
!
! $ Version
!
!-    Version 2.0.0 25-NOV-2001 (NJB)
!
!        Updated to relax tolerance on matrix quotient rotation angle
!        from 2.0D-12 to 1.0D-11 for cases 5-7 inclusive.  Original
!        tolerance was suitable for all platforms but MS Visual C++/C.
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

      CHARACTER*(LNSIZE)    ERROR

      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      ANGVEL ( 3 )
      DOUBLE PRECISION      AV     ( 3 )
      DOUBLE PRECISION      AV1    ( 3 )
      DOUBLE PRECISION      AV2    ( 3 )
      DOUBLE PRECISION      AXIS   ( 3, 20 )
      DOUBLE PRECISION      COUT
      DOUBLE PRECISION      COUT1
      DOUBLE PRECISION      COUT2
      DOUBLE PRECISION      DT
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      ET2
      DOUBLE PRECISION      MATRIX ( 3, 3 )
      DOUBLE PRECISION      MAXANG
      DOUBLE PRECISION      MINANG
      DOUBLE PRECISION      OFF
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      ROT    ( 3, 3 )
      DOUBLE PRECISION      ROT1   ( 3, 3 )
      DOUBLE PRECISION      ROT2   ( 3, 3 )
      DOUBLE PRECISION      SCLK1
      DOUBLE PRECISION      SCLK2
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      TAXIS  ( 3 )
      DOUBLE PRECISION      TEMP   ( 3, 3 )
      DOUBLE PRECISION      UAV1   ( 3 )
      DOUBLE PRECISION      UAXIS  ( 3 )
      DOUBLE PRECISION      UTAXIS ( 3 )
      DOUBLE PRECISION      ZEROPT

      INTEGER               I
      INTEGER               CKHAND

      LOGICAL               FND
      LOGICAL               FND1
      LOGICAL               FND2



!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_TSTCK3' )



      CALL TCASE ( 'Check to make sure that we can get an ' &
      //           'attitude back from the C-kernel. ')

         CALL TSTCK3 ( 'TEST.CK', 'TEST.SCLK', &
                       .TRUE.,    .TRUE.,      .FALSE., CKHAND )
         SCLKDP = 0.0D0

         CALL CKGPAV(-9999, SCLKDP, 0.0D0, 'J2000', ROT, AV, COUT, FND )


         CALL CHCKXC ( .FALSE.,  ' ',                       OK )
         CALL CHCKSL ( 'FOUND',   FND, .TRUE.,              OK )
         CALL CHCKSD ( 'CLKOUT',  COUT, '=', SCLKDP, 0.0D0, OK )


         SCLKDP = 1000000000.D0

         CALL CKGPAV(-9999, SCLKDP, 0.0D0, 'J2000', ROT, AV, COUT, FND )


         CALL CHCKXC ( .FALSE.,  ' ',                       OK )
         CALL CHCKSL ( 'FOUND',   FND, .TRUE.,              OK )
         CALL CHCKSD ( 'CLKOUT',  COUT, '=', SCLKDP, 0.0D0, OK )

         SCLKDP = 1.0D+13

         CALL CKGPAV(-9999, SCLKDP, 0.0D0, 'J2000', ROT, AV, COUT, FND )


         CALL CHCKXC ( .FALSE.,  ' ',                       OK )
         CALL CHCKSL ( 'FOUND',   FND, .TRUE.,              OK )
         CALL CHCKSD ( 'CLKOUT',  COUT, '=', SCLKDP, 0.0D0, OK )

      CALL TCASE ( 'Check to see that the rotations returned by ' &
      //           'TSTATD behave as advertised. ' )


         AXIS(1, 1) = 1.0D0
         AXIS(2, 1) = 2.0D0
         AXIS(3, 1) = 4.0D0


         AXIS(1, 2) = 2.0D0
         AXIS(2, 2) = 4.0D0
         AXIS(3, 2) = 1.0D0


         AXIS(1, 3) = 4.0D0
         AXIS(2, 3) = 1.0D0
         AXIS(3, 3) = 2.0D0


         AXIS(1, 4) = 4.0D0
         AXIS(2, 4) = 2.0D0
         AXIS(3, 4) = 1.0D0


         AXIS(1, 5) = 2.0D0
         AXIS(2, 5) = 1.0D0
         AXIS(3, 5) = 4.0D0


         AXIS(1, 6) = 1.0D0
         AXIS(2, 6) = 4.0D0
         AXIS(3, 6) = 2.0D0


         AXIS(1, 7) = 1.0D0
         AXIS(2, 7) = 2.0D0
         AXIS(3, 7) = 3.0D0


         AXIS(1, 8) = 2.0D0
         AXIS(2, 8) = 3.0D0
         AXIS(3, 8) = 1.0D0


         AXIS(1, 9) = 3.0D0
         AXIS(2, 9) = 1.0D0
         AXIS(3, 9) = 2.0D0


         AXIS(1,10) = 3.0D0
         AXIS(2,10) = 2.0D0
         AXIS(3,10) = 1.0D0


         AXIS(1,11) = 2.0D0
         AXIS(2,11) = 1.0D0
         AXIS(3,11) = 3.0D0


         AXIS(1,12) = 1.0D0
         AXIS(2,12) = 3.0D0
         AXIS(3,12) = 2.0D0


         AXIS(1,13) = 2.0D0
         AXIS(2,13) = 3.0D0
         AXIS(3,13) = 6.0D0


         AXIS(1,14) = 3.0D0
         AXIS(2,14) = 6.0D0
         AXIS(3,14) = 2.0D0


         AXIS(1,15) = 6.0D0
         AXIS(2,15) = 2.0D0
         AXIS(3,15) = 3.0D0


         AXIS(1,16) = 6.0D0
         AXIS(2,16) = 3.0D0
         AXIS(3,16) = 2.0D0

         AXIS(1,17) = 3.0D0
         AXIS(2,17) = 2.0D0
         AXIS(3,17) = 6.0D0


         AXIS(1,18) = 2.0D0
         AXIS(2,18) = 6.0D0
         AXIS(3,18) = 3.0D0


         AXIS(1,19) = 1.0D0
         AXIS(2,19) = 1.0D0
         AXIS(3,19) = 1.0D0


         AXIS(1,20) = 0.0D0
         AXIS(2,20) = 0.0D0
         AXIS(3,20) = 1.0D0


         ET = -900000010.0D0

         DO I = 1, 20

            DT = 1000000.0D0
!
!           Compute the rotation of the structure over a 1 million
!           second interval.
!
            CALL TSTATD ( ET-DT, ROT1, AV1 )
            CALL TSTATD ( ET,    ROT2, AV2 )

            CALL MTXM   ( ROT2,   ROT1,   MATRIX )
            CALL RAXISA ( MATRIX, TAXIS,  ANGLE )

            CALL VHAT   ( TAXIS,     UTAXIS )
            CALL VHAT   ( AV1,       UAV1   )
            CALL VHAT   ( AXIS(1,I), UAXIS  )

            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( 'AV1',    AV1,    '=', AV2,   3, 0.0D0,   OK )
            CALL CHCKSD ( 'ANGLE',  ANGLE,  '~/',0.1D0,    1.0D-13, OK )
            CALL CHCKAD ( 'UAV1',   UAV1,   '~', UAXIS, 3, 1.0D-14, OK )
            CALL CHCKAD ( 'UTAXIS', UTAXIS, '~', UAXIS, 3, 1.0D-13, OK )
            CALL CHCKAD ( 'UAV1',   UAV1,   '||',UAXIS, 3, 1.0D-13, OK )
            CALL CHCKAD ( 'UTAXIS', UTAXIS, '||',UAXIS, 3, 1.0D-13, OK )

            ET = ET + 100000000.0D0

         END DO



      CALL TCASE ( 'Check for continuity of the C-kernel and ' &
      //          'TSTATD across the 100 million second ' &
      //          'boundaries refered to in TSTATD. ' )


         ET = -900000000.0D0

         CALL TPARSE ( '1-JAN-1980', ZEROPT, ERROR )


         DO WHILE ( ET .LT. ZEROPT )
            ET = ET + 100000000.0D0
         END DO

         RATE = 1.0D0/10000000.0D0
         MAXANG = 2.0D0 * RATE
         MINANG = 0.1D0 * RATE

         DO WHILE ( ET .LT. ZEROPT + 999999999.0D0 )

            ET1 = ET - 1.0D0
            ET2 = ET + 1.0D0

            CALL SCE2T ( -9, ET1, SCLK1 )
            CALL SCE2T ( -9, ET2, SCLK2 )
            CALL CKGPAV ( -9999,  SCLK1, 0.0D0, 'J2000', ROT1, AV1, &
                           COUT1, FND1  )
            CALL CKGPAV ( -9999,  SCLK2, 0.0D0, 'J2000', ROT2, AV2, &
                           COUT2, FND2  )


            CALL MTXM   ( ROT2, ROT1,  TEMP  )
            CALL RAXISA ( TEMP, TAXIS, ANGLE )

            CALL CHCKXC ( .FALSE., ' ',                         OK )
            CALL CHCKSL ( 'FND1',  FND1, .TRUE.,                OK )
            CALL CHCKSL ( 'FND2',  FND2, .TRUE.,                OK )
            CALL CHCKSD ( 'COUT1', COUT1, '=',   SCLK1,  0.0D0, OK )
            CALL CHCKSD ( 'COUT2', COUT2, '=',   SCLK2,  0.0D0, OK )
            CALL CHCKSD ( 'ANGLE', ANGLE, '>',   MINANG, 0.0D0, OK )
            CALL CHCKSD ( 'ANGEL', ANGLE, '<',   MAXANG, 0.0D0, OK )


            CALL TSTATD ( ET1, ROT1, AV1 )
            CALL TSTATD ( ET2, ROT2, AV2 )


            CALL MTXM   ( ROT2, ROT1,  TEMP  )
            CALL RAXISA ( TEMP, TAXIS, ANGLE )

            CALL CHCKXC ( .FALSE., ' ',                         OK )
            CALL CHCKSD ( 'ANGLE', ANGLE, '>',   MINANG, 0.0D0, OK )
            CALL CHCKSD ( 'ANGEL', ANGLE, '<',   MAXANG, 0.0D0, OK )


            ET = ET + 100000000.0D0

         END DO


      CALL TCASE ( 'Make sure the conversion between ET and ' &
      //           'ticks works as expected. ' )

         CALL TPARSE ( '1-JAN-1980', ZEROPT, ERROR )

         ET = ZEROPT

         DO I = 1, 999

            CALL SCE2T ( -9, ET, SCLKDP )

            COUT =     ( ET - ZEROPT ) * 10000.0D0

            CALL CHCKSD ( 'SCLKDP', SCLKDP, '=', COUT, 0.0D0, OK )

            ET = ET + 10000000.0D0
         END DO





      CALL TCASE ( 'Check to see if attitudes returned by the ' &
      //           'C-kernel for are very nearly the same as that ' &
      //           'returned by TSTATD (for body -9999). ' )


         CALL TPARSE ( '1-JAN-1980', ET, ERROR )

         DO I = 1, 100

            CALL SCE2T  ( -9,    ET,     SCLKDP )

            CALL TSTATD (  ET,   MATRIX, ANGVEL )
            CALL CKGPAV ( -9999, SCLKDP, 0.0D0, 'GALACTIC', &
                           ROT,  AV,     COUT,   FND )


            CALL TSTMSG ( '#', 'Subcase: #. Angle between ' &
            //                 'rotations: #  ' )


            CALL MXMT   (  MATRIX, ROT,   TEMP  )
            CALL RAXISA (  TEMP,   TAXIS, ANGLE )

            CALL TSTMSI ( I )
            CALL TSTMSD ( ANGLE )

            CALL CHCKXC ( .FALSE.,  ' ',                            OK )

            CALL CHCKSL ( 'FOUND',  FND,       .TRUE.,              OK )
            CALL CHCKSD ( 'ANGLE',  ANGLE,'~',  0.0D0,     1.0D-14, OK )

            CALL CHCKSD ( 'CLKOUT', COUT, '=',  SCLKDP,    0.0D0,   OK )
            CALL CHCKAD ( 'ROT',    ROT,  '~/', MATRIX, 9, 1.0D-11, OK )
            CALL CHCKAD ( 'AV',     AV,   '||', ANGVEL, 3, 1.0D-11, OK )
            CALL CHCKAD ( 'AV',     AV,   '~/', ANGVEL, 3, 1.0D-14, OK )

            ET = ET + 10000000.0D0
         END DO

      CALL TCASE ( 'Check to see if attitudes returned by the ' &
      //           'C-kernel for agree with those ' &
      //           'returned by TSTATD (for body -10000). ' )


         CALL TPARSE ( '1-JAN-1980', ET, ERROR )

         OFF = 0.0D0

         DO I = 1, 100

            CALL SCE2T  ( -9,    ET,     SCLKDP )

            CALL TSTATD (  ET+OFF, MATRIX, ANGVEL )
            CALL CKGPAV ( -10000,  SCLKDP, 0.0D0, 'FK4', &
                           ROT,    AV,     COUT,   FND )


            CALL TSTMSG ( '#', 'Subcase: #. Angle between ' &
            //                 'rotations: #  ' )


            CALL MXMT   (  MATRIX, ROT,   TEMP  )
            CALL RAXISA (  TEMP,   TAXIS, ANGLE )

            CALL TSTMSI ( I )
            CALL TSTMSD ( ANGLE )

            CALL CHCKXC ( .FALSE.,  ' ',                            OK )

            CALL CHCKSL ( 'FOUND',  FND,       .TRUE.,              OK )
            CALL CHCKSD ( 'ANGLE',  ANGLE,'~',  0.0D0,     1.0D-14, OK )

            CALL CHCKSD ( 'CLKOUT', COUT, '=',  SCLKDP,    0.0D0,   OK )
            CALL CHCKAD ( 'ROT',    ROT,  '~/', MATRIX, 9, 1.0D-11, OK )
            CALL CHCKAD ( 'AV',     AV,   '||', ANGVEL, 3, 1.0D-11, OK )
            CALL CHCKAD ( 'AV',     AV,   '~/', ANGVEL, 3, 1.0D-14, OK )

            ET = ET + 10000000.0D0
         END DO

      CALL TCASE ( 'Check to see if attitudes returned by the ' &
      //           'C-kernel for agree with those ' &
      //           'returned by TSTATD (for body -10001 ). ' )


         CALL TPARSE ( '1-JAN-1980', ET, ERROR )

         OFF = 0.0D0

         DO I = 1, 100

            CALL SCE2T  ( -9,    ET,     SCLKDP )

            CALL TSTATD (  ET+OFF, MATRIX, ANGVEL )
            CALL CKGPAV ( -10001,  SCLKDP, 0.0D0, 'J2000', &
                           ROT,    AV,     COUT,   FND )


            CALL TSTMSG ( '#', 'Subcase: #. Angle between ' &
            //                 'rotations: #  ' )


            CALL MXMT   (  MATRIX, ROT,   TEMP  )
            CALL RAXISA (  TEMP,   TAXIS, ANGLE )

            CALL TSTMSI ( I )
            CALL TSTMSD ( ANGLE )

            CALL CHCKXC ( .FALSE.,  ' ',                            OK )

            CALL CHCKSL ( 'FOUND',  FND,       .TRUE.,              OK )
            CALL CHCKSD ( 'ANGLE',  ANGLE,'~',  0.0D0,     1.0D-14, OK )

            CALL CHCKSD ( 'CLKOUT', COUT, '=',  SCLKDP,    0.0D0,   OK )
            CALL CHCKAD ( 'ROT',    ROT,  '~/', MATRIX, 9, 1.0D-11, OK )
            CALL CHCKAD ( 'AV',     AV,   '||', ANGVEL, 3, 1.0D-11, OK )
            CALL CHCKAD ( 'AV',     AV,   '~/', ANGVEL, 3, 1.0D-14, OK )

            ET = ET + 10000000.0D0
         END DO

      CALL CKUPF   ( CKHAND    )
      CALL KILFIL  ( 'TEST.CK' )
      CALL T_SUCCESS ( OK )
      RETURN
      END
